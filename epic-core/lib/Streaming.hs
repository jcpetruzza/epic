module Streaming
  ( -- * Producers
    fileHunks
  , hInputHunks
  , produceHunks

    -- * Consumers
  , hOutputHunks
  )

where

import           Hunk ( Hunk, mkHunk )
import           LinesOfText
import           Surface ( Surface(..) )
import           SrcLoc ( Span(..), RowCol(..) )

import qualified Control.Exception as Ex
import qualified Data.Attoparsec.Text as P
import           Data.Text ( Text )
import qualified Data.Text as Text
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Lazy.Builder as LTB
import           Data.Typeable ( Typeable )
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
      let hunkSpan = LinesOfText.totalSpan ls
      yield $ mkHunk fileName hunkSpan mempty ls


-- | A 'Producer' that parses the 'Surface' representation of 'Hunk's
--   and emit thems.
--
--   NB. Will raise a 'ParsingError' on invalid data.
hInputHunks :: Surface a => Handle -> Pipes.Producer (Hunk a) IO ()
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



-- | A 'Consumer' that outputs a 'Hunk' to the given 'Handle'.
hOutputHunks :: Surface a => Handle -> Pipes.Consumer (Hunk a) IO ()
hOutputHunks h
  = do first <- await
       liftIO $ printHunk first
       for cat $ \hunk -> liftIO $ do
         LText.hPutStrLn h ""
         printHunk hunk
  where
    printHunk
      = LText.hPutStr h . LTB.toLazyText . buildSurface
