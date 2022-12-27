module LocalPrelude
  ( module LocalPrelude
  , module Export
  ) where

import Control.Arrow as Export hiding ((<+>))
import Control.Lens as Export hiding ((|>), un, at)
import Control.Monad.Writer as Export
import Control.Monad.State as Export
import Data.Coerce as Export
import Data.DList as Export (DList)
import Data.Foldable as Export
import Data.Function as Export (on)
import Data.Hashable
import Data.List as Export hiding (uncons)
import Data.Maybe as Export
import Data.Monoid as Export
import Data.Scientific as Export
import Data.Text.Lens as Export (packed)
import Data.Time as Export
import Data.Time.Lens as Export
import Prelude as Export hiding ((^), log, until)
import Text.Printf as Export
import qualified Data.HashMap.Strict as HM

todo :: a
todo = undefined

stringDate :: String -> Day
stringDate = takeWhile (/= ' ') ^ parseTimeM False todo "%0Y-%m-%d" ^ fromJust

(^) :: (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)

eq :: Eq a => a -> a -> Bool
eq = (==)

partition'
  :: (Eq k, Hashable k, Monoid v)
  => (k -> v -> Bool) -> HM.HashMap k v -> (HM.HashMap k v, HM.HashMap k v)
partition' predicate hm = HM.foldlWithKey' f mempty hm
  where
    f (trues, falses) k v = if predicate k v
      then (HM.insertWith mappend k v trues, falses)
      else (trues, HM.insertWith mappend k v falses)
