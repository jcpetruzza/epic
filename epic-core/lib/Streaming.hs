module Streaming
  ( -- * Producers
    fileHunks
  , hInputHunks
  , produceHunks

    -- * Consumers
  , hOutputHunks
  , emitHunks
  )

where

import           Hunk ( Hunk, mkHunk )
import           LinesOfText
import           Surface ( Surface(..), runSurfaceBuilderWith )

import qualified Control.Exception as Ex
import qualified Data.Attoparsec.Text as P
import           Data.Text ( Text )
import qualified Data.Text.Lazy.IO as LText
import           Pipes
import qualified Pipes.Attoparsec
import qualified Pipes.Text.IO as Pipes.Text
import           System.IO ( Handle )


-- | Given a list of 'FilePath', produce one 'Hunk' per file with its
--   full contents.
--
--   NB. Will raise an 'IOError' if there are problems opening or
--   or reading from a file.
fileHunks :: [FilePath] -> Pipes.Producer (Hunk LinesOfText) IO ()
fileHunks fileNames
  = for (mapM_ yield fileNames) $ \fileName -> do
      ls <- liftIO $ LinesOfText.fromLazyText <$> LText.readFile fileName
      case LinesOfText.totalSpan ls of
        Nothing       -> pure ()  -- empty file
        Just hunkSpan -> yield $ mkHunk fileName hunkSpan mempty ls


-- | A 'Producer' that parses the 'Surface' representation of 'Hunk's
--   and emit thems.
--
--   NB. Will raise a 'ParsingError' on invalid data.
hInputHunks :: (Surface a, MonadIO m) => Handle -> Pipes.Producer (Hunk a) m ()
hInputHunks
  = produceHunks . Pipes.Text.fromHandle


-- | Turn a 'Pipes.Producer' of 'Text' into a 'Producer' of 'Hunk's.
--   It will skip all empty lines between consecutive hunks.
--
--   NB. It will raise a 'Pipes.Attoparsec.ParsingError' on errors.
produceHunks
  :: (Surface a, Monad m)
  => Pipes.Producer Text m r
  -> Producer (Hunk a) m r

produceHunks prodText
  = do
      ea <- Pipes.Attoparsec.parsed hunkParser prodText
      case ea of
        Right r -> pure r
        Left (exc, _) -> Ex.throw exc
  where
    hunkParser
      = parseSurface <* P.takeWhile P.isEndOfLine


-- | Emit the surface representation of each 'Hunk'.
emitHunks :: (Surface a, MonadIO m) => Pipes.Pipe (Hunk a) Text m ()
emitHunks
  = for Pipes.cat $
      runSurfaceBuilderWith Pipes.yield . buildSurface


-- | A 'Consumer' that outputs a 'Hunk' to the given 'Handle'.
hOutputHunks :: (Surface a, MonadIO m) => Handle -> Pipes.Consumer (Hunk a) m ()
hOutputHunks h
  = emitHunks >-> Pipes.Text.toHandle h
