import System.Environment
import Data.List (group)

main :: IO()
main = do
    args <- getArgs
    case args of
        [input, mode, output] ->
            if mode == "compress"
                then compressFile input output
                else if mode == "decompress"
                    then decompressFile input output
                    else putStrLn "Invalid mode. Use 'compress' or 'decompress'"
        _ -> putStrLn "Invalid arguments!\nUse '[input filename] [mode (compress/decompress)] [output filename]'"

compressFile :: String -> String -> IO()
compressFile input output = do
    content <- readFile input
    writeFile output (compress1 content)

decompressFile :: String -> String -> IO()
decompressFile input output = do
    content <- readFile input
    writeFile output (decompress content)

compress1 :: String -> String
compress1 content = take ((length str) - 1) str
  where str = compress content

compress :: String -> String
compress = concat . map (\x -> show (length x) ++ " " ++ [head x] ++ "|") . group

splitBy :: (Char -> Bool) -> [Char] -> [[Char]]
splitBy p s = case span (not . p) s of
                   (h, []) -> [h]
                   (h, _:t) -> h : splitBy p t

decompress1 :: String -> String
decompress1 inp = take (read (s !! 0) ::Int) $ repeat ((s !! 1) !! 0)
  where s = splitBy (== ' ') inp

decompress :: String -> String
decompress inp = foldr (++) "" (map decompress1 str)
  where str = splitBy (== '|') inp
