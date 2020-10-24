{-# LANGUAGE DeriveAnyClass #-}
module Accounting.Core
  ( module Accounting.Core
  , module Control.Monad.Writer
  ) where

import LocalPrelude

import Data.Typeable
import Data.Data
import Data.Scientific
import GHC.Generics (Generic)
import GHC.Real (Fractional(..))
import qualified Data.Hashable as H
import Control.Monad.Writer

-- ** Amount

newtype Amount = Amount
  { amountUn :: Scientific
  } deriving stock (Eq, Ord)
    deriving Show via Scientific
    deriving Num via Scientific
    deriving Fractional via Scientific
makeFields ''Amount

-- ** Instruction

data Account = Account
  { accountNumber :: Int
  , accountName :: String
  } deriving (Show, Typeable, Data, Generic)
makeFields ''Account

instance Eq Account where
  (==) = (==) `on` view number

instance Ord Account where
  compare = compare `on` (^.number)

deriving instance H.Hashable Account

data Instruction = Instruction
  { instructionAmount :: Amount
  , instructionAccount :: Account
  } deriving (Show)
makeFields ''Instruction

-- ** Transaction

type Time a = (Eq a, Ord a, Enum a, Show a)

-- | Transaction with time resolution and annotation
data Transaction t a where
  Transaction :: Time t => t -> a -> [Instruction] -> Transaction t a

deriving instance (Show t, Show a) => Show (Transaction t a)

class HasTime s a | s -> a where
  time :: Lens' s a
  {-# MINIMAL time #-}

instance HasTime (Transaction t a) t where
  time f (Transaction t d is) = fmap (\t' -> Transaction t' d is) (f t)

instructions :: Lens' (Transaction t a) [Instruction]
instructions f (Transaction t d is) = fmap (\is' -> Transaction t d is') (f is)

annotation :: Lens' (Transaction t a) a
annotation f (Transaction t d is) = fmap (\d' -> Transaction t d' is) (f d)
