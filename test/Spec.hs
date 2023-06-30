import Examples.HTML
import Examples.Hex
import CatRegex

main :: IO ()
main = do
    putStrLn htmlTag
    putStrLn htmlTag'
    putStrLn imageTagSrc -- todo: add random testing for all of these
    putStrLn imageTagSrc' -- todo: add random testing for all of these
    putStrLn hexColor
    putStrLn hexColor'
    print $ (stringify $ char '.' :: String) == (stringify $ exactly' "\\." :: String)
    print $ (stringify $ char '/' :: String) == (stringify $ exactly' "\\/" :: String)
