import Text.ParserCombinators.Parsec
import Data.String.CSV

main :: IO ()
main = do
    do file <- parseFromFile csvFile "employees.csv"