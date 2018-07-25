module Main ( main )

where

import Hunk ( Hunk, mkHunk )
import LinesOfText
import SrcLoc ( Span(..), RowCol(..) )
import Surface ( Surface(..) )


import           Data.Text ( Text )
import qualified Data.Text.Lazy.IO as LText
import qualified Data.Text.Lazy.Builder as LTB
import           Pipes
import           System.Environment ( getArgs, getProgName )
import qualified System.Exit as Exit
import           System.IO ( Handle, stdout )

main :: IO ()
main
  = do
      args <- getArgs
      case args of
        [] -> usage >> Exit.exitFailure
        _  -> runEffect $
                fileHunks args >-> outputHunk stdout
  where
    usage
      = do self <- getProgName
           putStrLn $ "Usage: " ++ self ++ " FILE [FILE...]"

fileHunks :: [FilePath] -> Pipes.Producer (Hunk LinesOfText) IO ()
fileHunks fileNames
  = for (mapM_ yield fileNames) $ \fileName -> do
      ls <- liftIO $ LinesOfText.fromLazyText <$> LText.readFile fileName
      let hunkSpan = LinesOfText.totalSpan ls
      yield $ mkHunk fileName hunkSpan mempty ls

outputHunk :: Surface a => Handle -> Pipes.Consumer (Hunk a) IO ()
outputHunk h
  = do first <- await
       liftIO $ printHunk first
       for cat $ \hunk -> liftIO $ do
         LText.hPutStrLn h ""
         printHunk hunk
  where
    printHunk
      = LText.hPutStr h . LTB.toLazyText . buildSurface
