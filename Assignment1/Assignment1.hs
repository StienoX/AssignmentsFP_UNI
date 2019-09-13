module Main where

    {- This is a framework in which all functions to be written are "undefined".  -
     - Note that in most cases parameters, pattern-matching and guards have been  -
     - omitted! You will have to add those yourself.                              -}
    
    import Data.Char
    import Data.List
    import Data.Maybe
    
    -- | Model
    
    type Field = String
    type Row   = [Field]
    type Table = [Row]
    
    -- | Main
    
    main :: IO ()
    main = interact (unlines . exercise . lines)
    
    exercise :: [String] -> [String]
    exercise = printTable
             . project ["last", "first", "salary"]
             . select "gender" "male"
             . parseTable
    
    -- | Parsing
    
    -- * Exercise 1
    
    parseTable :: [String] -> Table
    parseTable = map words
    
    -- | Printing
    
    -- * Exercise 2
    
    printLine :: [Int] -> String
    --printLine [] = "+"
    --printLine (x:xs) = "+" ++ replicate x '-' ++ printLine xs
    printLine xs = foldr mkstr "+" xs
        where mkstr y ys = ('+' : replicate y '-') ++ ys
    
    
        -- * Exercise 3
    printField :: Int -> String -> String
    printField n x | all isDigit x = replicate (n - length x) ' ' ++ x
                   | otherwise = x ++ replicate (n - length x) ' '
    -- * Exercise 4
                   
    printRow :: [(Int, String)] -> String
    printRow x = "|" ++ intercalate "|" (map (uncurry printField) x) ++ "|"
    
    -- * Exercise 5
    
    columnWidths :: Table -> [Int]
    columnWidths x = map (maximum . map length) (transpose x)
    
    -- * Exercise 6
    
    printTable :: Table -> [String]
    printTable table@(header:rows)
        = line ++ header' header ++ line ++ body' rows ++ line
        where header' xs = [printRow (zip col_width (map (map toUpper) xs))] 
              body' ys = map (printRow . zip col_width) ys
              col_width = columnWidths table
              line = [printLine col_width]
    
    -- | Querying
    
    -- * Exercise 7
    
    select :: Field -> Field -> Table -> Table
    select column value table@(header:rows)
        = undefined
    
    -- * Exercise 8
    
    project :: [Field] -> Table -> Table
    project columns table@(header:_)
        = undefined