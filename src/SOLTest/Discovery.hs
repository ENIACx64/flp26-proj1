-- | Discovering @.test@ files and their companion @.in@\/@.out@ files.
module SOLTest.Discovery (discoverTests) where

import SOLTest.Types
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
  )
import System.FilePath (replaceExtension, takeBaseName, takeExtension, (</>))

-- | Discover all @.test@ files in a directory.
--
-- When @recursive@ is 'True', subdirectories are searched recursively.
-- Returns a list of 'TestCaseFile' records, one per @.test@ file found.
-- The list is ordered by the file system traversal order (not sorted).
--
-- FLP: Implement this function. The following functions may come in handy:
--      @doesDirectoryExist@, @takeExtension@, @forM@ or @mapM@,
--      @findCompanionFiles@ (below).
discoverTests :: Bool -> FilePath -> IO [TestCaseFile]
discoverTests recursive dir = do
  entries <- listDirectory dir
  let fullPaths = map (dir </>) entries
  -- \| List of test case files is constructed using a locally defined function 'checkPath'
  testCaseFiles <- mapM checkPath fullPaths
  return (concat testCaseFiles)
  where
    checkPath path = do
      -- \| If path is a directory, either searches recursively or returns an empty list
      directoryExists <- doesDirectoryExist path
      if directoryExists
        then
          if recursive
            then discoverTests True path
            else return []
        else
          -- \| If path is not a directory, checks for extension and creates the record
          if takeExtension path == ".test"
            then do
              record <- findCompanionFiles path
              return [record]
            else
              return []

-- | Build a 'TestCaseFile' for a given @.test@ file path, checking for
-- companion @.in@ and @.out@ files in the same directory.
findCompanionFiles :: FilePath -> IO TestCaseFile
findCompanionFiles testPath = do
  let baseName = takeBaseName testPath
      inFile = replaceExtension testPath ".in"
      outFile = replaceExtension testPath ".out"
  hasIn <- doesFileExist inFile
  hasOut <- doesFileExist outFile
  return
    TestCaseFile
      { tcfName = baseName,
        tcfTestSourcePath = testPath,
        tcfStdinFile = if hasIn then Just inFile else Nothing,
        tcfExpectedStdout = if hasOut then Just outFile else Nothing
      }
