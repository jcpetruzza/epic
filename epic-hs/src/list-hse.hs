{-# LANGUAGE OverloadedStrings  #-}
module Main (main)


where

import           Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           GHC.Generics (Generic)
import qualified Language.Haskell.Exts as HSE
import           System.Environment as Env

import Envelope
import SrcLoc


main :: IO ()
main = do
  [file] <- Env.getArgs
  mainWith file []

mainWith :: FilePath -> [HSE.Extension] -> IO ()
mainWith file extensions = do
  HSE.parseFileWithExts extensions file >>= \case
    HSE.ParseOk mod -> do
      let decls = envelopedDecl <$> moduleDecls mod
      mapM_ (LBS.putStrLn . JSON.encode) decls
    HSE.ParseFailed loc errMsg -> putStrLn $ "FAILED " <> show loc <> " " <> errMsg


data DeclType
  = ImportDecl
  | ValDecl
  | TypeDecl
  | ClassDecl
  | ClassInstDecl
  deriving (Eq, Generic, FromJSON, ToJSON)

data EnvTypeDecl = EnvTypeDecl
  deriving (Eq, Generic, FromJSON, ToJSON)

data ModuleDecl a
  = ModuleDecl
      { declType :: DeclType
      , declBody :: a
      }
  deriving (Eq, Functor, Foldable, Traversable, Generic, FromJSON, ToJSON)

data Decl l
  = Import (HSE.ImportDecl l)
  | NonImp (HSE.Decl l)
  deriving (Eq, Functor, Foldable, Traversable, Generic)

prettyDecl :: Decl l -> Text
prettyDecl = \case
  Import d -> Text.pack $ HSE.prettyPrint d
  NonImp d -> Text.pack $ HSE.prettyPrint d

instance HSE.Annotated Decl where
  ann = \case
    Import d -> HSE.ann d
    NonImp d -> HSE.ann d

  amap f = \case
    Import d -> Import (HSE.amap f d)
    NonImp d -> NonImp (HSE.amap f d)

declSpan :: Decl HSE.SrcSpanInfo -> Src Span
declSpan d =
  Src
    { srcFilename =
        HSE.srcSpanFilename hseSpan
    , src =
        Span
          { spanStart = uncurry RowCol (HSE.srcSpanStart hseSpan)
          , spanEnd   = uncurry RowCol (HSE.srcSpanEnd   hseSpan)
          }
    }
  where
    hseSpan = HSE.srcInfoSpan (HSE.ann d)


envelopedDecl
  :: ModuleDecl (Decl HSE.SrcSpanInfo)
  -> Enveloped EnvTypeDecl (Located (ModuleDecl Text))
envelopedDecl modDecl =
  Enveloped EnvTypeDecl $
    Located (declSpan decl) (prettyDecl decl <$ modDecl)
  where
    decl = declBody modDecl


moduleDecls :: HSE.Module l -> [ModuleDecl (Decl l)]
moduleDecls = \case
  HSE.Module _ _ _ imps decls ->
    (fmap Import <$> processImports imps) <> (fmap NonImp <$> processDecls decls)
  HSE.XmlPage{}   -> []
  HSE.XmlHybrid{} -> []
  where
    processImports imps
      = [ModuleDecl ImportDecl imp | imp <- imps]

    processDecls decls
      = catMaybes [do t <- declType d; pure (ModuleDecl t d) | d <- decls]

    declType = \case
      HSE.TypeDecl{}          -> pure TypeDecl
      HSE.TypeFamDecl{}       -> Nothing
      HSE.ClosedTypeFamDecl{} -> Nothing
      HSE.DataDecl{}          -> pure TypeDecl
      HSE.GDataDecl{}         -> pure TypeDecl
      HSE.DataFamDecl{}       -> Nothing
      HSE.TypeInsDecl{}       -> Nothing
      HSE.DataInsDecl{}       -> Nothing
      HSE.GDataInsDecl{}      -> Nothing
      HSE.ClassDecl{}         -> pure ClassDecl
      HSE.InstDecl{}          -> pure ClassInstDecl
      HSE.DerivDecl{}         -> pure ClassInstDecl
      HSE.InfixDecl{}         -> Nothing
      HSE.DefaultDecl{}       -> Nothing
      HSE.SpliceDecl{}        -> Nothing
      HSE.TypeSig{}           -> pure ValDecl
      HSE.PatSynSig{}         -> Nothing
      HSE.FunBind{}           -> pure ValDecl
      HSE.PatBind{}           -> pure ValDecl
      HSE.PatSyn{}            -> Nothing
      HSE.ForImp{}            -> pure ValDecl
      HSE.ForExp{}            -> pure ValDecl
      HSE.RulePragmaDecl{}    -> pure ValDecl
      HSE.DeprPragmaDecl{}    -> Nothing
      HSE.WarnPragmaDecl{}    -> Nothing
      HSE.InlineSig{}         -> pure ValDecl
      HSE.InlineConlikeSig{}  -> Nothing
      HSE.SpecSig{}           -> Nothing
      HSE.SpecInlineSig{}     -> Nothing
      HSE.InstSig{}           -> Nothing
      HSE.AnnPragma{}         -> Nothing
      HSE.MinimalPragma{}     -> Nothing
      HSE.RoleAnnotDecl{}     -> Nothing
