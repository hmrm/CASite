import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/Config.hs"]
