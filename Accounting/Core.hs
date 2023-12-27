{-# LANGUAGE DeriveAnyClass #-}
module Accounting.Core where

import LocalPrelude
import Data.Typeable
import Data.Data
import GHC.Generics (Generic)
import qualified Data.Hashable as H

-- ** Amount

newtype Amount = Amount
  { amountUn :: Scientific
  } deriving stock (Eq, Ord)
    deriving newtype (Show, Read, Num, Fractional)

-- ** Instruction

data Account = Account
  { accountNumber :: Int
  , accountName :: String
  } deriving (Show, Read, Typeable, Data, Generic)
makeFields ''Account

instance Eq Account where
  (==) = (==) `on` view number

instance Ord Account where
  compare = compare `on` (^.number)

deriving instance H.Hashable Account

data Instruction = Instruction
  { instructionAmount :: Amount
  , instructionAccount :: Account
  } deriving (Show, Read, Eq, Ord)
makeFields ''Instruction

-- ** Transaction

-- | Transaction with time resolution and annotation
data Transaction t a where
  Transaction :: t -> a -> [Instruction] -> Transaction t a

deriving instance (Eq t, Eq a) => Eq (Transaction t a)
deriving instance (Show t, Show a) => Show (Transaction t a)
deriving instance (Read t, Read a, Ord t, Enum t, Show t) => Read (Transaction t a)

class HasTime s a | s -> a where
  time :: Lens' s a
  {-# MINIMAL time #-}

instance HasTime (Transaction t a) t where
  time f (Transaction t d is) = fmap (\t' -> Transaction t' d is) (f t)

instructions :: Lens' (Transaction t a) [Instruction]
instructions f (Transaction t d is) = fmap (\is' -> Transaction t d is') (f is)

annotation :: Functor f => (a -> f b) -> Transaction t a -> f (Transaction t b)
annotation f (Transaction t d is) = fmap (\d' -> Transaction t d' is) (f d)
