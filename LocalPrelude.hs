module LocalPrelude
  ( module LocalPrelude
  , module Export
  ) where

import Prelude as Export hiding ((^), log, until)
import Control.Lens as Export hiding ((|>), un, at)
import Control.Arrow as Export hiding ((<+>))
import Data.Function as Export (on)
import Data.Maybe as Export
import Data.Monoid as Export
import Data.Foldable as Export
import Data.Time as Export
import Data.Time.Lens as Export
import Text.Printf as Export
import Data.List as Export hiding (uncons)

todo :: a
todo = undefined

stringDate :: String -> Day
stringDate = takeWhile (/= ' ') ^ parseTimeM False todo "%0Y-%m-%d" ^ fromJust

(^) :: (a -> b) -> (b -> c) -> a -> c
(^) = flip (.)
