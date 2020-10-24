module Eesti where

import LocalPrelude
import Accounting


type Transaction' = Transaction Day


-- * Käibemaksu deklaratsioon (KMD)

käibemaksud
  :: forall a. Account -> Account -> [Transaction' a]
  -> [((Integer, Int), Amount, [Amount], [(Amount, Transaction' a)])]
käibemaksud sisendKm väljundKm ts = map f $ groupMonths $ sortBy (compare `on` view _1) kms
  where
    {- | Sisend- ja väljund-käibemaksude saldo: võtame ainult deebet
         sisendkäibemaksud ja kreedit väljundkäibemaksud. -}
    kms :: [(Day, Either
              (Amount, Transaction' a) -- ^ Sisend-käibemaks
              (Amount, Transaction' a) -- ^ Väljund-käibemaks
            )]
    kms = do
      tr <- ts
      i <- tr ^. instructions
      let acc = i^.account
          amount' = i^.amount

      -- | Jäta ainult sisendKm/väljundKm kasvu transaktsioonid
      guard $ acc == sisendKm && amount' > 0
           || acc == väljundKm && amount' < 0

      case () of
        _ | acc == väljundKm && amount' < 0
            -> pure (tr^.time, Right (amount', tr))

          | acc == sisendKm && amount' > 0
            -> pure (tr^.time, Left (amount', tr))

          | otherwise -> mempty

    ym t = (t^.year, t^.month) -- ^ year-month tuple
    groupMonths = groupBy (\a b -> ym (a^._1) == ym (b^._1))
    f is@ (t : _) = let
      is' = map (view _2) is
      amounts = map (either (view _1) (view _1)) is' :: [Amount]
      arved = is'
        & map (either (\_ -> Nothing) Just)
        & catMaybes
      mahaarvamised = filter (> 0) amounts :: [Amount]
      in (ym (t^._1), sum amounts, mahaarvamised, arved)
    f _ = error "The impossible happened"

-- * Print

nl :: IO ()
nl = putStrLn ""

-- | Käibemaksudeklaratsioonid kuude kaupa
_KMDd :: forall a. Show a => Account -> Account -> [Transaction' a] -> IO ()
_KMDd sisendKm väljundKm trs = forM_ (käibemaksud sisendKm väljundKm trs) $ \t -> do
  nl
  putStr $ show (t^._1._1) <> "-" <> show (t^._1._2) <> ": "

  nl
  putStr "  - KMD INF A + KMD 1.: "
  forM_ (t^._4) $ \(amount' :: Amount, tr :: Transaction' a) -> do
    nl
    putStr $ "    - " <> show amount'
      <> " " <> show (tr^.annotation)
      <> " " <> show (tr^.time)
    forM_ (tr^.instructions) $ \i -> do
      nl
      putStr $ "      - " <> (show $ i^.amount) <> " " <> i^.account.name

  nl
  putStr " - KMD 5 ja 5.1 (m.a kokku): "
  putStr $ show $ t^._3.to sum
  putStr " = sum "
  putStr $ show $ t^._3

  nl
  putStr " - KMD 12 (tasumisele kuuluv käibemaks): "
  putStr $ show $ t^._2

  nl
