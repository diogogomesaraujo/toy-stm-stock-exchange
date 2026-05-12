module Main(main) where
import OrderBook
import Control.Concurrent.STM
import Test.QuickCheck

main :: IO ()
main = do
    ordBook            <- atomically $ newTOrderBook
    (ord1, ord2, ord3) <- generate (arbitrary :: Gen (Order Int, Order Int, Order Int))
    _                  <- atomically $ addTOrderBook ord1 ordBook
    _                  <- atomically $ addTOrderBook ord2 ordBook
    _                  <- atomically $ addTOrderBook ord3 ordBook
    ordBookStr         <- atomically $ showTOrderBook ordBook
    putStr ordBookStr
