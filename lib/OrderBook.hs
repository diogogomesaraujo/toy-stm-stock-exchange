-- TODO:
-- - Order book using STM.PriorityQueue
-- - Prove properties about the order book developed
--
-- Inspired by:
-- - https://medium.com/@thinhnguyen042002/lets-build-a-stock-exchange-from-scratch-18459611ad83
--

module OrderBook (TOrderBook, addTOrderBook) where

import TPriorityQueue
import Control.Concurrent.STM

type Timestamp = Int

data OrderType = Buy | Sell
    deriving (Show, Eq)

data Order c = Order {
    orderType :: OrderType,
    timestamp :: Timestamp,
    limitAmount :: c
} deriving (Show, Eq)

instance Ord c => Ord (Order c) where
    compare ordA ordB =
        compare (limitAmount ordA, timestamp ordA)
                (limitAmount ordB, timestamp ordB)


data TOrderBook c = TOrderBook {
    bidOrders :: TPriorityQueue (Order c),
    askOrders :: TPriorityQueue (Order c)
}

addTOrderBook :: Ord c => Order c -> TOrderBook c -> IO ()
addTOrderBook ord ordBook = do
    atomically $ case orderType ord of
        Buy  -> insertTPriorityQueue
                    ord (bidOrders ordBook)
        Sell -> insertTPriorityQueue
                    ord (askOrders ordBook)
