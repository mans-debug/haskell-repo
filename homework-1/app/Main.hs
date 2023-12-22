import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import GHC.Desugar (toAnnotationWrapper)
import System.Directory
import System.Environment
import System.FilePath
import System.IO

type Path = FilePath

type Size = Integer

data Options = Options
  { depth :: Int,
    humanReadable :: Bool,
    verbose :: Bool,
    rootPath :: Path
  }

type DuM a = ExceptT String (WriterT String (ReaderT Options (StateT Size IO))) a

getFileSize' :: Path -> DuM Size
getFileSize' filePath = do
  options <- ask
  when (verbose options) $ liftIO $ putStrLn $ "Getting size of file: " ++ filePath
  size <- liftIO $ withFile filePath ReadMode hFileSize
  let formattedSize = if humanReadable options then formatSize size else show size
  when (depth options == -1) $ tell $ formattedSize ++ "\t" ++ filePath ++ "\n"
  return size


duDirectory :: Path -> Int -> DuM Size
duDirectory dirPath currentDepth = do
  options <- ask
  when (verbose options) $ liftIO $ putStrLn $ "Entering directory: " ++ dirPath
  contents <- liftIO $ listDirectory dirPath
  sizes <- mapM (\name -> du (dirPath </> name) (currentDepth + 1)) contents
  let totalSize = sum sizes
  let depthExceeded = currentDepth > depth options
  let formattedSize = if humanReadable options then formatSize totalSize else show totalSize
  when (not depthExceeded || (depth options == (-1))) $ tell $ formattedSize ++ "\t" ++ dirPath ++ "\n"
  return totalSize

du :: Path -> Int -> DuM Size
du path currentDepth = do
  isDir <- liftIO $ doesDirectoryExist path
  options <- ask
  if isDir
    then do
      size <- duDirectory path currentDepth
      let depthExceeded = currentDepth > depth options
      return size
    else getFileSize' path

formatSize :: Size -> String
formatSize size
  | size < 1024 = show size ++ "B"
  | size < 1024 ^ 2 = show (size `div` 1024) ++ "KB"
  | size < 1024 ^ 3 = show (size `div` (1024 ^ 2)) ++ "MB"
  | otherwise = show (size `div` (1024 ^ 3)) ++ "GB"

runDu :: Options -> IO (Either String Size, String)
runDu options = do
  (result, log) <-
    flip runStateT 0 $
      runReaderT (runWriterT (runExceptT (du (rootPath options) 0))) options
  return result

parseArgs :: [String] -> Options
parseArgs args = foldl processOption defaultOptions args
  where
    defaultOptions =
      Options
        { depth = -1,
          humanReadable = False,
          verbose = False,
          rootPath = "."
        }

    processOption opts "-d" = opts {depth = read (head args)}
    processOption opts "-s" = opts {depth = 0}
    processOption opts "-h" = opts {humanReadable = True}
    processOption opts "-v" = opts {verbose = True}
    processOption opts dir = opts {rootPath = dir}

main :: IO ()
main = do
  args <- getArgs
  let options = parseArgs args
  (result, log) <- runDu options
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right size -> do
      putStrLn $ "\n\n\n" ++ log
