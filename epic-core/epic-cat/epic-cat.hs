module Main ( main )

where

import Streaming

import           Pipes ( runEffect, (>->) )
import           System.Environment ( getArgs, getProgName )
import qualified System.Exit as Exit
import           System.IO ( stdout )

main :: IO ()
main
  = do
      args <- getArgs
      case args of
        [] -> usage >> Exit.exitFailure
        _  -> runEffect $
                Streaming.fileHunks args >-> Streaming.hOutputHunks stdout
  where
    usage
      = do self <- getProgName
           putStrLn $ "Usage: " ++ self ++ " FILE [FILE...]"
