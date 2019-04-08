{-# LANGUAGE ViewPatterns #-}

module Comprehension.Plugin (plugin) where

import Prelude hiding (init, last)
import Data.Bool
import Data.Data
import Data.Generics as SYB
import Data.String (fromString)
import Type.Reflection as R

import qualified GhcPlugins as GHC
import HsExtension (GhcPs, NoExt (..))
import HsSyn
import qualified HsTypes
import Outputable (SDoc, ppr)
import qualified Outputable as Ppr
import SrcLoc

plugin :: GHC.Plugin
plugin = GHC.defaultPlugin { GHC.parsedResultAction = \ _ -> go } where
    go :: GHC.ModSummary -> GHC.HsParsedModule -> GHC.Hsc GHC.HsParsedModule
    go _ m =
        [m { GHC.hpm_module = hpm_module' }
           | dflags <- GHC.getDynFlags
           , hpm_module' <- transform dflags (GHC.hpm_module m)]

transform :: GHC.DynFlags -> GHC.Located (HsModule GhcPs) -> GHC.Hsc (GHC.Located (HsModule GhcPs))
transform _dflags = Ppr.pprTraceDebug "comprehensions-ghc:Comprehension.Plugin:" . gppr' <*> SYB.everywhereM (SYB.mkM transform') where
    transform' :: LHsExpr GhcPs -> GHC.Hsc (LHsExpr GhcPs)
    transform' = pure . \ case
        L l (HsDo _ comp (L l' (reverse -> L l'' (LastStmt _ body _ x):ss)))
          | case comp of ListComp -> True
                         MonadComp -> True
                         _ -> False ->
            L l . HsDo NoExt DoExpr . L l' . reverse $ L l'' (BodyStmt NoExt (noLoc $ HsApp NoExt pureExpr body) x x):ss
        xl -> xl

    pureExpr :: GenLocated SrcSpan (HsExpr GhcPs)
    pureExpr =
        noLoc . HsVar NoExt . noLoc . GHC.mkOrig (GHC.Module (GHC.stringToUnitId "base") $ GHC.mkModuleName "GHC.Base") . GHC.mkVarOcc $
        "pure"

gppr' :: Data a => a -> Ppr.SDoc
gppr' = gpprPrec' 0

gpprPrec' :: Data a => Rational -> a -> SDoc
gpprPrec' prec t
  | headTyConOf (L () ()) == headTyConOf t = gmapQi 1 (gpprPrec' prec) t
  | headTyConOf [()]      == headTyConOf t = pprList $ asList t
  | Just NoExt <- cast t = Ppr.empty
  | Just _ <- cast t :: Maybe GHC.SourceText = Ppr.empty
  | Just _ <- cast t :: Maybe HsTypes.Promoted = Ppr.empty
  | Just l <- cast t :: Maybe (HsLit GhcPs) = ppr l
  | Just n <- cast t :: Maybe GHC.RdrName   = Ppr.doubleQuotes $ ppr n
  | otherwise = Ppr.sdocWithDynFlags $ \ dflags ->
    let sdocs = filter (not . Ppr.isEmpty dflags) $
                (:) <$> fromString . showConstr . toConstr <*> gmapQ (gpprPrec' (prec + 1)) $ t
    in bool id Ppr.parens (prec > 0 && length sdocs > 1) $ Ppr.fsep sdocs
  where
    pprList = Ppr.brackets . Ppr.pprWithCommas id

    headTyConOf :: Typeable a => a -> TyCon
    headTyConOf = fst . splitApps . R.typeOf

    asList :: Data a => a -> [SDoc]
    asList t | [] <- gmapQ (pure ()) t = []
             | otherwise = gmapQi 0 gppr' t : gmapQi 1 asList t
