module Main where
import Control.Applicative ((<$>), (<*>))
import System.Environment
import Text.Parsec
import Text.Parsec.String
import GHC.Real (reduce)

data CSV = CSV {header :: [String], rows :: [[String]]}
  deriving (Show)

parseCSV :: Char -> Bool -> Parser CSV
parseCSV separator hasHeader = do
  headerRow <- parseRow separator
  rows <- if hasHeader then parseRowsWithHeader headerRow separator else parseRows separator
  return $ CSV headerRow rows

parseRow :: Char -> Parser [String]
parseRow separator = parseCell separator `sepBy` char separator <* newline


parseRowsWithHeader :: [String] -> Char -> Parser [[String]]
parseRowsWithHeader header separator = do
  rows <- many (parseRow separator)
  -- let equalsRowLength row = length row == length header
  let failedRows = filter (not . (\(i, row) -> length row == length header)) (zip [2..] rows)
  let headerLog row = " number of headers " ++ show (length row) ++ "/" ++ show (length header)
  let failedRowsLog = foldl (\acc (i, row@(id : _)) -> acc ++ "\n" ++ id ++ "\t" ++ show i ++ headerLog row) ""
  if null failedRows
    then return rows
    else fail $ "Number of cells in rows doesn't match the header\n" ++ "Failed rows \n" ++  failedRowsLog failedRows

parseRows :: Char -> Parser [[String]]
parseRows separator = many (parseRow separator)

parseCell :: Char -> Parser String
parseCell separator =
  do
    char '"'
    content <- many (noneOf ['"'])
    char '"'
    return content
    <|> many (noneOf [separator, '\n'])

readAndParseCSV :: FilePath -> Char -> Bool -> IO (Either ParseError CSV)
readAndParseCSV filePath separator hasHeader = do
  csvData <- readFile filePath
  return $ parse (parseCSV separator hasHeader) filePath csvData

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filePath, separator, hasHeader] -> do
      result <- readAndParseCSV filePath (head separator) (read hasHeader)
      case result of
        Left err -> putStrLn $ "Error parsing CSV: " ++ show err
        Right csv -> print csv
    _ -> putStrLn $ "Usage: program <CSVFilePath> <Separator> <HasHeader>\n" ++ "You passed the following arguments: " ++ show args
