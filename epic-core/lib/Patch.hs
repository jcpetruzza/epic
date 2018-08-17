module Patch
  ( -- * The Patch type
    Patch(..)
  , applyPatch


  -- * The Patcher monad
  , Patcher
  , runPatcher
  , PatchApplyError(..)

  )

where

import Prelude hiding (span)

import           SrcLoc                     ( Located(..), RowCol(..), Span(..), Src(..) )

import qualified Control.Exception          as Exc
import           Control.Monad
import           Control.Monad.Catch        ( bracket )
import           Control.Monad.Except       ( ExceptT, throwError, runExceptT )
import           Control.Monad.IO.Class     ( liftIO )
import qualified Data.List                  as List
import           Data.Maybe                 ( fromMaybe )
import           Data.Text                  ( Text )
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as Text
import           GHC.Generics               ( Generic )

import           System.IO                  ( IOMode(ReadMode), hClose, openFile )
import           System.IO.Error
import qualified System.IO.Temp             as Temp
import qualified System.Directory           as Dir
import qualified System.FilePath            as FilePath


-- | Patches that can be applied on a file
data Patch a
  = PatchFileDelete FilePath
  | PatchFileCopy
      FilePath  -- ^ From
      FilePath  -- ^ To
  | PatchReplaceHunk
      (Located a)
  deriving
    ( Eq
    , Show
    , Generic
    )


data PatchApplyError
  = PatchApplyIOError IOError
  | PatchApplyFileNotFound FilePath
  | PatchApplyFileExists FilePath
  | PatchApplyBadSpan (Src Span)
  deriving
    ( Eq
    , Show
    , Generic
    )

-- {{{ The Patcher monad ------------------------------------------------------

type Patcher
  = ExceptT PatchApplyError IO

runPatcher :: Patcher a -> IO (Either PatchApplyError a)
runPatcher
  = runExceptT

ioActionOnErr :: (IOError -> Patcher a) -> IO a -> Patcher a
ioActionOnErr handleError action
  = do
      ea <- liftIO $ Exc.try $ action
      case ea of
        Right a  -> pure a
        Left exc -> handleError exc

ioAction :: IO a -> Patcher a
ioAction
  = ioActionOnErr (throwError . PatchApplyIOError)

patcherError :: PatchApplyError -> Patcher a
patcherError = throwError


-- }}} The Patcher monad ------------------------------------------------------



-- | Atomically apply the given patch.
applyPatch :: Patch [Text] -> Patcher ()
applyPatch = \case
  PatchFileDelete f -> do
    ioActionOnErr (fileNotFoundOrIOError f) $
      Dir.removeFile f

  PatchFileCopy srcFile destFile -> do
    destAlreadyExists <- liftIO $ Dir.doesFileExist destFile
    when destAlreadyExists $
      patcherError (PatchApplyFileExists destFile)

    ioActionOnErr (fileNotFoundOrIOError srcFile) $ Dir.copyFile srcFile destFile


  PatchReplaceHunk locHunk -> do
    let
      f         = srcFilename $ location locHunk
      targetDir = FilePath.takeDirectory f
    Temp.withTempFile targetDir "apply-patch.tmp" $ \f' h' -> do
       pure ()
       bracket
         (ioActionOnErr (fileNotFoundOrIOError f) (openFile f ReadMode))
         (ioAction . hClose) $ \h -> do
            patchToDest locHunk h h'
            ioAction $ hClose h'
            ioAction $ Dir.renameFile f' f

  where
    fileNotFoundOrIOError f ioErr
      | isDoesNotExistError ioErr = patcherError (PatchApplyFileNotFound f)
      | otherwise                 = patcherError (PatchApplyIOError ioErr)


    patchToDest locHunk srcHandle destHandle
      = do
          let start = spanStart span
              end   = spanEnd span

          when (not $ start <= end) $
            patcherError badPatch

          copyLines (row start)

          lineHunkStart <- fromMaybe Text.empty <$> nextLineIfExists
          copyLineColsUntil (col start) lineHunkStart

          writeHunk

          lineHunkEnd <- skipLines (row end - row start) lineHunkStart
          copyLineColsFrom (col end) lineHunkEnd

          copyLinesUntilEoF
      where
        span
          = src $ location locHunk

        hunkLines
          = element locHunk

        copyLines n
          = replicateM_ n (nextLine >>= ioAction . Text.hPutStrLn destHandle)

        copyLinesUntilEoF
          = nextLineIfExists >>= \case
              Nothing   -> pure ()
              Just line -> ioAction (Text.hPutStrLn destHandle line)
                             >> copyLinesUntilEoF

        skipLines n currLine
          = foldM (\_ _ -> nextLine) currLine [1..n]

        nextLine
          = ioActionOnErr (onEOF $ patcherError badPatch) $
              Text.hGetLine srcHandle

        nextLineIfExists
          = ioActionOnErr (onEOF $ pure Nothing) $
              Just <$> Text.hGetLine srcHandle

        copyLineColsUntil n line
          = do
              let initialCols = Text.take n line
              when (Text.length initialCols < n) $
                patcherError badPatch
              ioAction $ Text.hPutStr destHandle initialCols

        copyLineColsFrom n line
          = do
              let finalCols = Text.drop n line
              when (Text.null finalCols && Text.length line /= n) $
                patcherError badPatch
              ioAction $ Text.hPutStrLn destHandle finalCols

        writeHunk
          = ioAction $
              sequence_ $
                List.intersperse newline $
                  map (Text.hPutStr destHandle) hunkLines

        newline
          = Text.hPutStrLn destHandle Text.empty

        onEOF handleEOF ioErr
          | isEOFError ioErr = handleEOF
          | otherwise        = patcherError (PatchApplyIOError ioErr)

        badPatch
          = PatchApplyBadSpan (location locHunk)
