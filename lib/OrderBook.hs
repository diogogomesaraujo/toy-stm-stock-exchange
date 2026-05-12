-- TODO:
-- - Prove properties about the order book developed
--
-- Inspired by:
-- - https://medium.com/@thinhnguyen042002/lets-build-a-stock-exchange-from-scratch-18459611ad83
--

module OrderBook ( OrderType,
                  TOrderBook,
                  newTOrderBook,
                  addTOrderBook,
                  removeTOrderBook,
                  Order,
                  newOrder,
                  showTOrderBook ) where

import TPriorityQueue
import Control.Concurrent.STM
import Test.QuickCheck

type Timestamp = Int

data OrderType = Buy | Sell
    deriving (Show, Eq)

instance Arbitrary OrderType where
    arbitrary = elements [Buy, Sell]

data Order c = Order {
    orderType   :: OrderType,
    timestamp   :: Timestamp,
    limitAmount :: c
} deriving (Show, Eq)

newOrder :: Ord c => OrderType -> Timestamp -> c -> Order c
newOrder t ts l =
    Order { orderType   = t,
            timestamp   = ts,
            limitAmount = l }

instance Ord c => Ord (Order c) where
    compare ordA ordB =
        compare (limitAmount ordA, timestamp ordA)
                (limitAmount ordB, timestamp ordB)

instance (Ord c, Arbitrary c) => Arbitrary (Order c) where
    arbitrary = do
        ordType <- arbitrary
        ordTimestamp <- arbitrary
        ordLimitAmount <- arbitrary
        return $ newOrder ordType ordTimestamp ordLimitAmount

data TOrderBook c = TOrderBook {
    bidOrders :: TPriorityQueue (Order c),
    askOrders :: TPriorityQueue (Order c)
}

newTOrderBook :: Ord c => STM (TOrderBook c)
newTOrderBook = do
    bidOrds <- newTPriorityQueue
    askOrds <- newTPriorityQueue
    return $ TOrderBook { bidOrders = bidOrds,
                          askOrders = askOrds }

addTOrderBook :: Ord c => Order c -> TOrderBook c -> STM ()
addTOrderBook ord ordBook = do
    case orderType ord of
        Buy  -> insertTPriorityQueue
                    ord (bidOrders ordBook)
        Sell -> insertTPriorityQueue
                    ord (askOrders ordBook)

removeTOrderBook :: Ord c => OrderType -> TOrderBook c -> STM (Maybe (Order c))
removeTOrderBook t ordBook = do
    case t of
        Buy  -> pollTPriorityQueue $ bidOrders ordBook
        Sell -> pollTPriorityQueue $ askOrders ordBook

showTOrderBook :: (Ord c, Show c) => TOrderBook c -> STM String
showTOrderBook ordBook = do
    bidOrds <- showTPriorityQueue $ bidOrders ordBook
    askOrds <- showTPriorityQueue $ askOrders ordBook
    return $ show (bidOrds, askOrds)
