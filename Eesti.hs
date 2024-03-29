module Eesti where

import LocalPrelude
import Accounting


-- * Tõlked

-- | Konto koondsumma üle tehingute
saldo :: Account -> [Transaction t a] -> Amount
saldo = balance

-- * Käibemaksu deklaratsioon (KMD)

käibemaksud
  :: forall a . Show a
  => Account
  -> Account
  -> [Transaction Day a]
  -> [((Integer, Int), Amount, [Amount], [(Amount, Transaction Day a)])]
käibemaksud sisendKm väljundKm ts = map f $ groupMonths $ sortBy (compare `on` view _1) kms
  where
    {- | Sisend- ja väljund-käibemaksude saldo: võtame ainult deebet
         sisendkäibemaksud ja kreedit väljundkäibemaksud. -}
    kms :: [(Day, Either
              (Amount, Transaction Day a) -- ^ Sisend-käibemaks
              (Amount, Transaction Day a) -- ^ Väljund-käibemaks
            )]
    kms = do
      tr <- ts
      i <- tr ^. instructions
      let acc = i^.account
          amount' = i^.amount

      -- | Jäta ainult sisendKm/väljundKm kasvu transaktsioonid
      guard $ acc == sisendKm && amount' > 0
           || acc == väljundKm && amount' < 0

      if | acc == väljundKm && amount' < 0
           -> pure (tr^.time, Right (amount', tr))
         | acc == sisendKm && amount' > 0
           -> pure (tr^.time, Left (amount', tr))
         | otherwise -> mempty

    ym t = (t^.year, t^.month) -- ^ year-month tuple
    groupMonths = groupBy (\a b -> ym (a^._1) == ym (b^._1))
    f is@(t : _) = let
      is' = map (view _2) is
      amounts = map (either (view _1) (view _1)) is' :: [Amount]
      arved = is'
        & map (either (\_ -> Nothing) Just)
        & catMaybes
      mahaarvamised = filter (> 0) amounts :: [Amount]
      in (ym (t^._1), sum amounts, mahaarvamised, arved)
    f _ = error "The impossible happened"

-- * Print

kontosaldo :: Account -> [Transaction t a] -> IO ()
kontosaldo konto transactions = do
  tekst' (konto^.name)
  tekst' ": "
  print (saldo konto transactions)

-- | Käibemaksudeklaratsioonid kuude kaupa
prindiKMDd
  :: forall a . (Show a)
  => Account -> Account -> Account -> [Transaction Day a] -> IO ()
prindiKMDd sisendKm väljundKm käibemaksustatavKonto trs = forM_ (käibemaksud sisendKm väljundKm trs) $ \t -> do
  nl
  putStr $ show (t^._1._1) <> "-" <> show (t^._1._2) <> ": "

  nl
  putStr "  KMD INF A (Müügitehingud):"
  forM_ (t^._4) $ \(amount' :: Amount, tr :: Transaction Day a) -> do
    nl
    putStrLn $ "    - tehingu kuupäev ja kirjeldus: " <> show (tr^.time) <> ", \"" <> tr^.annotation.to show <> "\""
    let käibemaksuta = saldo käibemaksustatavKonto [tr]
    putStrLn
      $  "      deklareeritud käive: " <> show (0 - amount')
    putStrLn
      $ "      arve summa käibemaksuta: " <> show (0 - käibemaksuta)

  nl
  nl
  putStr "  KMD põhivorm:"
  nl
  putStr "    KMD 1 (20% määraga maksustatavad toimingud ja tehingud): "
  putStr $ show $ 0 - (saldo käibemaksustatavKonto $ map snd $ t^._4)
  nl
  putStr "    KMD 4 (Käibemaks kokku (20% lahtrist 1 + 9% lahtrist 2) (+)): "
  putStr $ show $ sum $ map (abs . fst) (t^._4)

  nl
  putStr "    KMD 5 (mahaarvamised kokku): "
  putStr $ show $ t^._3.to sum
  putStr $ " (= " <> (intercalate " + " $ map show (t^._3)) <> ")"

  nl
  putStr "    KMD 12 (tasumisele kuuluv käibemaks): "
  putStr $ show $ 0 - t^._2

  nl
