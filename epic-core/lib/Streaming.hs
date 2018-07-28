module Streaming
  ( -- * Producers
    fileHunks

    -- * Consumers
  , hOutputHunks
  )

where

import           Hunk ( Hunk, mkHunk )
import           LinesOfText
import           Surface ( Surface(..) )
import           SrcLoc ( Span(..), RowCol(..) )

import           Data.Text ( Text )
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Lazy.Builder as LTB
import           Pipes
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
