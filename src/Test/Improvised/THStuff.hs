module Test.Improvised.THStuff where

import           Control.Arrow ((>>>))
import           Control.Lens
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set
import           Language.Haskell.TH hiding (cxt)
import           Language.Haskell.TH.Lens hiding (name)


pattern (:->) :: Type -> Type -> Type
pattern t :-> ts <- AppT (AppT ArrowT t) ts
  where
    t :-> ts = AppT (AppT ArrowT t) ts

removeSig :: Type -> Type
removeSig (SigT t _) = t
removeSig t = t

------------------------------------------------------------------------------
removeTyAnns :: Type -> Type
removeTyAnns = \case
  ForallT _ _ t -> removeTyAnns t
  SigT t _      -> removeTyAnns t
  ParensT t     -> removeTyAnns t
  t -> t

------------------------------------------------------------------------------
splitAppTs :: Type -> [Type]
splitAppTs = removeTyAnns >>> \case
  t `AppT` arg -> splitAppTs t ++ [arg]
  t            -> [t]

------------------------------------------------------------------------------
splitArrowTs :: Type -> [Type]
splitArrowTs = removeTyAnns >>> \case
  t :-> ts -> t : splitArrowTs ts
  t        -> [t]

overName :: (String -> String) -> Name -> Name
overName f = mkName . f . nameBase


-- | This function works like 'quantifyType' except that it takes
-- a list of variables to exclude from quantification.
quantifyType' :: Set Name -> Cxt -> Type -> Type
quantifyType' exclude c t = ForallT vs c t
  where
  vs = map PlainTV
     $ filter (`Set.notMember` exclude)
     $ nub -- stable order
     $ toListOf typeVars t


getBndrName :: TyVarBndr -> Name
getBndrName (PlainTV name) = name
getBndrName (KindedTV name _) = name


getTyVar :: Type -> Maybe Name
getTyVar (removeTyAnns -> VarT n) = Just n
getTyVar _                        = Nothing


applyE :: Exp -> [Exp] -> Exp
applyE = foldl' AppE

applyT :: Type -> [Type] -> Type
applyT = foldl' AppT

