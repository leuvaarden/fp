import Control.Monad
import qualified Data.ByteString.Char8 as C

pascal :: Int -> Int -> Int
pascal c r = if ((c == 0) || (c == r)) then 1 else (pascal (c) (r - 1)) + (pascal (c - 1) (r - 1))

printIt :: Int -> C.ByteString
printIt n = C.pack $ show $ [pascal y x | x <- [0..n], y <- [0..x]]

printItIo :: Int -> IO ()
printItIo n = mapM_ print [[pascal y x | y <- [0..x]] | x <- [0..n]]
