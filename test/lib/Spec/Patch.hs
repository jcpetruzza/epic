module Spec.Patch
  ( tests
  )

where

import SrcLoc
import Patch

import Test.Tasty
import Test.Tasty.HUnit

import           Data.Text        (Text)
import qualified Data.Text        as Text
import qualified Data.Text.IO     as Text

import qualified System.Directory as Dir
import           System.IO
import qualified System.IO.Temp   as Temp



tests :: TestTree
tests = testGroup "Patch"
  [ testDelete
  , testCopy
  , testReplaceHunk
  ]


testDelete :: TestTree
testDelete = testGroup "File Delete"
  [ testCase "Can delete" $
      withTempFile ["module Foo where"] $ \f -> do
        Dir.doesFileExist f @? "File existed"

        res <- runPatcher $
          applyPatch $ PatchFileDelete f

        res @?= Right ()
        (not <$> Dir.doesFileExist f) @? "File is gone"


  , testCase "Detects FileNotFound" $
      withFreshFilename $ \f -> do
        (not <$> Dir.doesFileExist f) @? "File does not exist"

        res <- runPatcher $
          applyPatch $ PatchFileDelete f

        res @?= Left (PatchApplyFileNotFound f)
  ]


testCopy :: TestTree
testCopy = testGroup "File Copy"
  [ testCase "Can copy" $ do
      let content = ["module Foo where", "foo = 1"]
      withTempFile content $ \src ->
        withFreshFilename $ \tgt -> do
          res <- runPatcher $
            applyPatch $ PatchFileCopy src tgt

          res @?= Right ()

          srcContent <- readFileContent src
          tgtContent <- readFileContent tgt

          srcContent @?= content
          tgtContent @?= content

  , testCase "Will not overwrite target" $
      withTempFile ["module Foo where"] $ \src ->
        withTempFile ["module Bar where"] $ \tgt -> do
          res <- runPatcher $
            applyPatch $ PatchFileCopy src tgt

          res @?= Left (PatchApplyFileExists tgt)

          srcContent <- readFileContent src
          tgtContent <- readFileContent tgt

          srcContent @?= ["module Foo where"]
          tgtContent @?= ["module Bar where"]

  , testCase "Detects source not found" $
      withFreshFilename $ \src ->
        withFreshFilename $ \tgt -> do
          res <- runPatcher $
            applyPatch $ PatchFileCopy src tgt

          res @?= Left (PatchApplyFileNotFound src)

  , testCase "Copy onto itself" $
      withTempFile ["module Foo where"] $ \src -> do
        res <- runPatcher $
          applyPatch $ PatchFileCopy src src

        res @?= Left (PatchApplyFileExists src)
  ]


testReplaceHunk :: TestTree
testReplaceHunk = testGroup "Replace hunks"
  [ testCase "Patch in the middle, full line" $ do
      withTempFile ["AAA", "BBB", "CCC", "DDD"] $ \f -> do
        res <- runPatcher $
          applyPatch $
           PatchReplaceHunk $
            Located (srcSpan f (1,0) (3,0))
              ["HHH", "III", "JJJ", ""]

        res @?= Right ()

        patched <- readFileContent f
        patched @?= ["AAA", "HHH", "III", "JJJ", "DDD"]

  , testCase "Patch in the middle, middle of line" $ do
      withTempFile ["AAA", "BBB", "CCC", "DDD"] $ \f -> do
        res <- runPatcher $
          applyPatch $
           PatchReplaceHunk $
            Located (srcSpan f (1,1) (3,1))
              ["HHH", "III", "JJJ"]

        res @?= Right ()

        patched <- readFileContent f
        patched @?= ["AAA", "BHHH", "III", "JJJDD"]

  , testCase "Patch at the beginning" $
        withTempFile ["AAA", "BBB", "CCC", "DDD"] $ \f -> do
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                Located (srcSpan f (0,0) (0,0))
                  ["HHH", "III"]

            res @?= Right ()

            patched <- readFileContent f
            patched @?= ["HHH", "IIIAAA", "BBB", "CCC", "DDD"]

  , testCase "Patch at the very end" $
        withTempFile ["AAA", "BBB", "CCC", "DDDD"] $ \f -> do
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                Located (srcSpan f (4,0) (4,0))
                  ["HHH", "III"]

            res @?= Right ()

            patched <- readFileContent f
            patched @?= ["AAA", "BBB", "CCC", "DDDD", "HHH", "III"]

  , testCase "Patch at end of last line" $
        withTempFile ["AAA", "BBB", "CCC", "DDD"] $ \f -> do
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                Located (srcSpan f (3,3) (3,3))
                  ["HHH", "III"]

            res @?= Right ()

            patched <- readFileContent f
            patched @?= ["AAA", "BBB", "CCC", "DDDHHH", "III"]

  , testCase "Patch middle of a line" $
        withTempFile ["AAA", "BBBBB", "CCC", "DDD"] $ \f -> do
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                Located (srcSpan f (1,2) (1,3))
                  ["HHH", "III"]

            res @?= Right ()

            patched <- readFileContent f
            patched @?= ["AAA", "BBHHH", "IIIBB", "CCC", "DDD"]

  , testCase "Invalid span (row)" $
        withTempFile ["AAA", "BBBBB", "CCC", "DDD"] $ \f -> do
            let badSpan = srcSpan f (1,2) (0,1)
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                 Located badSpan
                   ["HHH", "III"]

            res @?= Left (PatchApplyBadSpan badSpan)

            patched <- readFileContent f
            patched @?= ["AAA", "BBBBB", "CCC", "DDD"]

  , testCase "Invalid span (col)" $
        withTempFile ["AAA", "BBBBB", "CCC", "DDD"] $ \f -> do
            let badSpan = srcSpan f (1,2) (1,1)
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                 Located badSpan
                   ["HHH", "III"]

            res @?= Left (PatchApplyBadSpan badSpan)

            patched <- readFileContent f
            patched @?= ["AAA", "BBBBB", "CCC", "DDD"]

  , testCase "Invalid span (no initial row)" $
        withTempFile ["AAA", "BBBBB", "CCC", "DDD"] $ \f -> do
            let badSpan = srcSpan f (5,0) (5,0)
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                 Located badSpan
                   ["HHH", "III"]

            res @?= Left (PatchApplyBadSpan badSpan)

            patched <- readFileContent f
            patched @?= ["AAA", "BBBBB", "CCC", "DDD"]

  , testCase "Invalid span (no final row)" $
        withTempFile ["AAA", "BBBBB", "CCC", "DDD"] $ \f -> do
            let badSpan = srcSpan f (0,0) (5,0)
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                 Located badSpan
                   ["HHH", "III"]

            res @?= Left (PatchApplyBadSpan badSpan)

            patched <- readFileContent f
            patched @?= ["AAA", "BBBBB", "CCC", "DDD"]

  , testCase "Invalid span (no initial col)" $
        withTempFile ["AAA", "BBBBB", "CCC", "DDD"] $ \f -> do
            let badSpan = srcSpan f (0,4) (1,0)
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                 Located badSpan
                   ["HHH", "III"]

            res @?= Left (PatchApplyBadSpan badSpan)

            patched <- readFileContent f
            patched @?= ["AAA", "BBBBB", "CCC", "DDD"]

  , testCase "Invalid span (no final col)" $
        withTempFile ["AAA", "BBBBB", "CCC", "DDD"] $ \f -> do
            let badSpan = srcSpan f (0,0) (2,5)
            res <- runPatcher $
              applyPatch $
               PatchReplaceHunk $
                 Located badSpan
                   ["HHH", "III"]

            res @?= Left (PatchApplyBadSpan badSpan)

            patched <- readFileContent f
            patched @?= ["AAA", "BBBBB", "CCC", "DDD"]
  ]


withTempFile :: [Text] -> (FilePath -> IO ()) -> IO ()
withTempFile content useFile
  = Temp.withSystemTempFile "Foo.hs" $ \f h -> do
      mapM_ (Text.hPutStrLn h) content
      hClose h
      useFile f

withFreshFilename :: (FilePath -> IO ()) -> IO ()
withFreshFilename useFilename
  = Temp.withSystemTempFile "Foo.hs" $ \f h -> do
      hClose h
      Dir.removeFile f
      useFilename f

readFileContent :: FilePath -> IO [Text]
readFileContent f
  = Text.lines <$> Text.readFile f

srcSpan :: FilePath -> (Int, Int) -> (Int, Int) -> Src Span
srcSpan f (a, b) (c, d)
  = Src f $ Span (RowCol a b) (RowCol c d)
