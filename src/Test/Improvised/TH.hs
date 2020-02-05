{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Improvised.TH
  ( makeImprovised
  , makeImprovCollection
  , Improvised
  ) where

import           Control.Lens (toListOf)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           Data.Traversable
import           FCI.Internal
import           Language.Haskell.TH hiding (cxt)
import           Language.Haskell.TH.Lens hiding (name)
import           Test.Improvised.Internal
import           Test.Improvised.THStuff


options :: MkInstOptions
options = defaultOptions
  { mkInstClassConName = (++ "Impl") . mkInstClassConName defaultOptions
  , mkInstMethodFieldName = ('_' :) . mkInstMethodFieldName defaultOptions
  }


makeImprovised :: Name -> Q [Dec]
makeImprovised name = do
  dict_info <- getClassDictInfo options name
  let class_name = className dict_info

  fmap join $ sequenceA
    [ mkInstWithOptions options name
    , makePatternSyn dict_info
    , makeHasFieldClass dict_info
          (getClassName class_name)
          (getMethodName class_name)
    , fmap pure $ makeMockableInstance dict_info
    ]


removeImplSuffix :: String -> String
removeImplSuffix = reverse . drop 4 . reverse

removeUnderscorePrefix :: String -> String
removeUnderscorePrefix = drop 1

makePatternSyn :: ClassDictInfo -> Q [Dec]
makePatternSyn cdi = do
  let dcon_name         = dictConName cdi
      pattern_name      = overName removeImplSuffix $ dictConName cdi
      (methods, supers) = partition isMethodField $ dictFields cdi
      bndr_names        =
        fmap (overName removeUnderscorePrefix . fieldName) methods
      bndrs             = fmap VarP bndr_names
      pats              = (WildP <$ supers) <> bndrs
      args              = fmap VarE $ fmap (const 'inst) supers <> bndr_names
      inst_type = getInstType cdi
      free_vars = toListOf typeVars inst_type
  pure
    [ PatSynD
        pattern_name
        (RecordPatSyn bndr_names)
        (ExplBidir [Clause bndrs (NormalB $ applyE (ConE dcon_name) args) []])
        (ConP dcon_name pats)
    , PatSynSigD pattern_name
        . quantifyType' mempty (dictConstraints cdi)
        . foldr (:->) inst_type
        . fmap (quantifyType' (S.fromList free_vars) [])
        $ fmap origType methods
    ]


makeHasFieldClass :: ClassDictInfo -> Name -> Name -> Q [Dec]
makeHasFieldClass cdi class_name method_name = do
  a_name <- newName "env"
  let cs = getClassStuff cdi
  Just m_name <- pure $ getTyVar $ csMType cs
  let vars = mapMaybe getTyVar (csArgs cs <> [csMType cs])
      fundeps = (FunDep [a_name] [m_name] :)
              . fmap (strengthenFundep m_name a_name)
              $ dictFundeps cdi
  pure
    [ ClassD [] class_name (fmap PlainTV $ vars <> [a_name]) fundeps
        . pure
        . SigD method_name
        $ VarT a_name :-> csInstType cs
    , makeHasFieldInstance
        class_name
        (fmap VarT vars)
        (csInstType cs)
        method_name
        []
        (VarE 'id)
    ]


makeHasFieldInstance
    :: Name
    -> [Type]
    -> Type
    -> Name
    -> [Pat]
    -> Exp
    -> Dec
makeHasFieldInstance class_name args inst_ty method_name pats expr =
  InstanceD
      Nothing
      []
      (applyT (ConT class_name) $ args ++ [inst_ty])
    [ FunD method_name
        . pure
        $ Clause pats (NormalB expr) []
    , PragmaD $ InlineP method_name Inlinable ConLike AllPhases
    ]


------------------------------------------------------------------------------
-- | If a fundep includes @m_name@ on the left side, also add @a_name@ on the
-- left. This is necessary to preserve fundep logic from class @MonadX@ to the
-- @HasMonadX@ (see 'makeHasFieldClass")
strengthenFundep :: Name -> Name -> FunDep -> FunDep
strengthenFundep m_name a_name fundep@(FunDep lhs rhs)
  | elem m_name lhs = FunDep (a_name : lhs) rhs
  | otherwise       = fundep



getClassName :: Name -> Name
getClassName = overName ("Has" ++)


getMethodName :: Name -> Name
getMethodName = overName (("get" ++) . (++ "Improvised"))


liftExprIntoM :: Type -> Name -> Position -> (Exp, Type) -> Q Exp
liftExprIntoM m_type r_name pos (arg_exp, arg_type) =
  case classifyArg m_type arg_type of
    Boring  -> pure arg_exp
    Monadic -> pure $ liftCorrectPosition r_name pos arg_exp
    Lambda -> do
      Just (arg_types, result_type) <- pure $ unsnoc $ splitArrowTs arg_type
      arg_names <- for arg_types $ const $ newName "x"
      splice_args <- for (zip (fmap VarE arg_names) arg_types) $ \arg -> do
        liftExprIntoM m_type r_name (negatePosition pos) arg
      let call = applyE arg_exp splice_args
      splice_result <- liftExprIntoM m_type r_name pos (call, result_type)
      pure $ LamE (fmap VarP arg_names) splice_result


unsnoc :: [a] -> Maybe ([a], a)
unsnoc []  = Nothing
unsnoc [a] = Just ([], a)
unsnoc as  = Just (init as, last as)



liftCorrectPosition :: Name -> Position -> Exp -> Exp
liftCorrectPosition r_name Negative arg =
  VarE 'coerceImprovisable `AppE` arg `AppE` VarE r_name
liftCorrectPosition _ Positive arg =
  VarE 'lift `AppE` arg


------------------------------------------------------------------------------
-- | Builds an expression of the form
--
-- @r_name ^. dict_name . to field_name@
makeHasDictLookup :: Name -> Name -> Name -> Exp
makeHasDictLookup r_name dict_name field_name =
  VarE field_name `AppE` (VarE dict_name `AppE` VarE r_name)

--   applyE (VarE 'view)
--     [ InfixE
--         (Just $ VarE dict_name)
--         (VarE '(.))
--         (Just $ VarE 'to `AppE` VarE field_name)
--     , VarE r_name
--     ]


------------------------------------------------------------------------------
-- | Lifts a call to @field_name@ with @args@, by coercing them if they are in
-- the @m_type@ monad, and ignoring them othewise.
makeLiftedCall :: Type -> Name -> Name -> Name -> [(Name, Type)] -> Q Exp
makeLiftedCall m_type r_name dict_name field_name args = do
  args' <- for args $ \(arg, ty) -> liftExprIntoM m_type r_name Negative (VarE arg, ty)
  pure $ applyE (makeHasDictLookup r_name dict_name field_name) args'


------------------------------------------------------------------------------
-- | Builds an expression of the form
--
-- @Mockable $ ReaderT $ \r_name -> expr@
--
-- where @expr@ is expected to be the result of a 'makeLiftedCall'.
makeMockableScaffold :: Name -> Exp -> Exp
makeMockableScaffold r_name expr =
  ConE 'Improvisable `AppE` (ConE 'ReaderT `AppE` LamE [VarP r_name] expr)


------------------------------------------------------------------------------
-- | Lifts a method call by calling 'makeMockableScaffold' and 'makeLiftedCall'
-- by generating the @r_name@ and argument names.
makeLiftedMethod :: Type -> Name -> Name -> Name -> [Type] -> Q [Dec]
makeLiftedMethod m_type dict_name method_name field_name arg_types = do
  arg_names <- for arg_types $ const $ newName "x"
  r_name <- newName "r"
  let args = zip arg_names arg_types
  call <- makeLiftedCall m_type r_name dict_name field_name args
  pure
    [ FunD method_name
      $ pure
      $ Clause (fmap VarP arg_names)
          (NormalB
            $ makeMockableScaffold r_name call
          ) []
    , PragmaD $ InlineP method_name Inlinable FunLike AllPhases
    ]


makeMockableInstance :: ClassDictInfo -> Q Dec
makeMockableInstance cdi = do
  dict_name <- newName "dict"
  let ClassStuff m_type class_ctr _ args = getClassStuff cdi
  let class_name = className cdi
      okname = getMethodName class_name

  methods <-
    for (filter isMethodField $ dictFields cdi) $ \fi ->
      makeLiftedMethod
            m_type
            okname
            (origName fi)
            (fieldName fi)
        . init
        . splitArrowTs
        $ origType fi

  pure
    $ InstanceD
        Nothing
        ( applyT (ConT (getClassName class_name)) (args ++ [m_type, VarT dict_name])
        : dictConstraints cdi
        ) (class_ctr `AppT` (applyT (ConT ''Improvisable) [VarT dict_name, m_type]))
    $ join methods


data ClassStuff = ClassStuff
  { csMType       :: Type
  , csClassTyCtor :: Type
  , csInstType    :: Type
  , csArgs        :: [Type]
  }

getClassStuff :: ClassDictInfo -> ClassStuff
getClassStuff cdi =
  let class_name = className cdi
      class_vars = drop 1 $ splitAppTs $ dictTyArg cdi
      Just (vars_to_keep, removeSig -> m_type) = unsnoc class_vars
      class_ctr = applyT (ConT class_name) vars_to_keep
      inst_type = ConT ''Improvised `AppT` (class_ctr `AppT` m_type)
   in ClassStuff m_type class_ctr inst_type vars_to_keep


getInstType :: ClassDictInfo -> Type
getInstType = csInstType . getClassStuff


isMethodField :: ClassDictField -> Bool
isMethodField = (== Method) . fieldSource


data Position
  = Positive
  | Negative
  deriving (Eq, Ord, Show)

negatePosition :: Position -> Position
negatePosition Positive = Negative
negatePosition Negative = Positive

data ArgType
  = Boring
  | Monadic
  | Lambda
  deriving (Eq, Ord, Show)


classifyArg :: Type -> Type -> ArgType
classifyArg _ (_ :-> _) = Lambda
classifyArg m_type arg_type
  | m_type == head (splitAppTs arg_type) = Monadic
  | otherwise = Boring



makeImprovCollection :: Name -> Q [Dec]
makeImprovCollection nm = do
  reify nm >>= \case
    TyConI (DataD _ tycon_name vars _ [con] _) ->
      case con of
        NormalC con_name (fmap snd -> ts) ->
          makeHasDictInstForField tycon_name vars con_name ts
        RecC    con_name (fmap thd -> ts) ->
          makeHasDictInstForField tycon_name vars con_name ts
        _ -> error "Only for normal constructors and records"
    _ -> error "makeImprovCollection mustbe called on a type constructor"


makeHasDictInstForField :: Name -> [TyVarBndr] -> Name -> [Type] -> Q [Dec]
makeHasDictInstForField tycon_name vars con_name ts =
  for (zip ts [0..])
    . uncurry
    . hasDictInst tycon_name vars con_name
    $ length ts


isImprovised :: Type -> Bool
isImprovised t = removeTyAnns t == ConT ''Improvised

hasDictInst
    :: Name
    -> [TyVarBndr]
    -> Name
    -> Int
    -> Type
    -> Int
    -> Q Dec
hasDictInst tycon_name bndrs con_name num t idx = do
  field <- newName "x"
  let pats = do
        n <- [0 .. num - 1]
        pure $ case n == idx of
          True -> VarP field
          False -> WildP


  let apps = splitAppTs t
  case apps of
    [dict, c] | isImprovised dict -> do
      (ConT class_name : args) <- pure $ splitAppTs c
      pure $
        makeHasFieldInstance
          (getClassName class_name)
          args
          (applyT (ConT tycon_name) $ fmap (VarT . getBndrName) bndrs)
          (getMethodName class_name)
          [ConP con_name pats]
          (VarE field)
    _ -> error "hasDictInst must be called with 'Improvised' as the type"


thd :: (a, b, c) -> c
thd (_, _, c) = c


