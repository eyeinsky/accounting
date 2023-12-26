module Accounting.DSL where

import LocalPrelude
import Accounting.Core as Export

-- * The instruction DSL

data IMonoid s a = IMonoid
  { iMonoidAnnotation :: s
  , iMonoidInstructions :: DList a
  }

instance Semigroup s => Semigroup (IMonoid s a) where
  a <> b = IMonoid ann is
    where
      ann = iMonoidAnnotation a <> iMonoidAnnotation b
      is = iMonoidInstructions a <> iMonoidInstructions b

instance Monoid s => Monoid (IMonoid s a) where
  mempty = IMonoid mempty mempty

type I ann = Writer (IMonoid ann Instruction)

log :: Monoid ann => Amount -> Account -> I ann ()
log amount account = tell . (\i -> IMonoid mempty (pure i)) $ Instruction amount account

annotate :: MonadWriter (IMonoid s a) m => s -> m ()
annotate ann = tell $ IMonoid ann mempty

execI :: forall ann a. I ann a -> (ann, [Instruction])
execI im = (ann, toList is)
  where
    res = execWriter im
    is = iMonoidInstructions res
    ann = iMonoidAnnotation res

-- * The T monad

data TParam t a = TParam
  { tParamSingles :: DList (Transaction t a)
  , tParamInfinites :: [Transaction t a]
  }
  {- | Single and infinite are separate since with infinite streams of
 transactions we want to sort them, so they can't be DList's, since
 these need to be turned into regular lists -- which is impossible
 with infinite lists (fix: is it really?)
  -}
makeFields ''TParam

instance Time t => Semigroup (TParam t a) where
  a <> b = TParam x y
    where
      x = (mappend `on` view singles) a b
      y = (merge `on` view infinites) a b

instance Time t => Monoid (TParam t a) where
  mempty = TParam mempty mempty

type T t ann = StateT (TParam t ann) IO

-- | Run the T monad and merge the finite and infinite transactions by
-- date.
execT :: Time t => T t a b -> IO [Transaction t a]
execT tm = merge' <$> execStateT tm mempty
  where
    merge' tw = merge (tw^.singles.to toList) (tw^.infinites)

-- * Helpers

-- | Merge a two lists of transactions and keep transactions' time-ordering
merge :: Time t => [Transaction t a] -> [Transaction t a] -> [Transaction t a]
merge ass@(a : as) bss@(b : bs) = case (compare `on` view time) a b of
  LT -> a : merge as bss
  EQ -> a : b : merge as bs
  GT -> b : merge ass bs
merge [] as = as
merge as [] = as

-- | Verify if transaction balances
verify :: Transaction t a -> Bool
verify (Transaction _ _ is) = sum (map (^.amount) is) == 0

transact :: Time t => t -> I ann a -> T t ann (Transaction t ann)
transact time im = (append $ singles .~ pure tr $ mempty) $> tr
  where
    (ann, is) = execI im
    tr = Transaction time ann is

type Filter t a = [Transaction t a] -> [Transaction t a]

-- | Repeat transaction with iterating with 'next', starting from
-- 'from', then applying 'f' to perhaps filter or take a certain
-- amount.
every
  :: forall t ann b. Time t
  => Filter t ann -> (Transaction t ann -> Transaction t ann) -> t -> I ann b -> T t ann ()
every p next from im = mempty & infinites .~ (p ts) & append
  where
    (ann, is) = execI im
    t0 = Transaction from ann is
    ts = iterate next t0

append :: Time t => TParam t ann -> T t ann ()
append ts = modify (<> ts)

-- * API

-- ** Instructions

debit :: Monoid ann => Amount -> Account -> I ann ()
debit = log

credit :: Monoid ann => Amount -> Account -> I ann ()
credit amount = log (0 - amount)

(-|) :: Monoid ann => Amount -> Account -> I ann ()
(-|) = credit
(+|) :: Monoid ann => Amount -> Account -> I ann ()
(+|) = debit

(|-) :: Monoid ann => Account -> Amount -> I ann ()
(|-) = flip credit
(|+) :: Monoid ann => Account -> Amount -> I ann ()
(|+) = flip debit

(~>) :: Monoid ann => Account -> Account -> Amount -> I ann ()
(from ~> to) amount = do
  to |+ amount
  from |- amount

infixl 5 -|, +|, |-, |+, ~>

-- ** Transaction

-- | A specialised 'every' taking until time 't'
until :: Time t => t -> (Transaction t ann -> Transaction t ann) -> t -> I ann b -> T t ann ()
until t = every (filter (\tr -> tr^.time < t))

monthly :: Filter Day ann -> Day -> I ann b -> T Day ann ()
monthly f = every f (time.month %~ (+1))

weekly :: Filter Day ann -> Day -> I ann b -> T Day ann ()
weekly f = every f (time.day %~ (+7))
