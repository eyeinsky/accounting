module Eesti.Aastaaruanne where

import qualified Data.Text as TS
import qualified Data.HashMap.Strict as HM

import LocalPrelude
import Accounting
import Accounting.Helpers


-- | Aastalõpukanne tekitab aasta @kulud@-e ja @tulud@-e põhjal kande,
-- kus kulud ja tulud kantakse ktKoond kontole kokku ning seejärel
-- aruandeperioodi kasumiks.
lõpukandeInstruktsioonid
  :: Monoid a
  => ByAccounts Day a -> ByAccounts Day a -> Account -> Account -> (I a (), Amount)
lõpukandeInstruktsioonid tulud kulud koond aruandeperioodiKasum = let
    tuluSaldod = sumIOs <$> tulud :: HM.HashMap Account Amount
    kuluSaldod = sumIOs <$> kulud :: HM.HashMap Account Amount
    tuludSaldo = foldMap Sum tuluSaldod :: Sum Amount
    kuludSaldo = foldMap Sum kuluSaldod :: Sum Amount
    Sum kasum = tuludSaldo <> kuludSaldo :: Sum Amount
    kanne = do
      mapM_ (\(tulukonto, summa) -> tulukonto ~> koond $ summa) $ HM.toList tuluSaldod
      mapM_ (\(kulukonto, summa) -> kulukonto ~> koond $ summa) $ HM.toList kuluSaldod
      koond ~> aruandeperioodiKasum $ kasum
  in (kanne, kasum)

lõpukanne
  :: (Monoid a, Show a)
  => Maybe a             -- ^ Lõpukande annotatsioon
  -> [Transaction Day a] -- ^ Aasta kanded
  -> Account             -- ^ Koondkonto, kuhu tulud ja kulud kokku kanda
  -> Account             -- ^ Aruandeperioodi kasum, kuhu tulemus lõpuks kantakse
  -> Transaction Day a
lõpukanne ann ts koond aruandeperioodiKasum = let
  aasta = head ts^.time.year :: Integer
  (_,_,_, tulud, kulud) = toAccountTypes ts
  (instructions, _) = lõpukandeInstruktsioonid tulud kulud koond aruandeperioodiKasum
  tr = at (stringDate $ show aasta <> "-12-31") $ do
    maybe (return ()) annotate ann
    instructions
  in head (execT tr)

-- | (aasta, aasta kanded, lõpukanne)
type YearTS a = (Integer, [Transaction Day a], Transaction Day a)

aastaaruanded
  :: forall a . (Monoid a, Show a)
  => (Integer -> Maybe a)          -- ^ Lõpukande annotatsiooni loomis-funktsioon
  -> [Transaction Day a]           -- ^ Aasta kanded
  -> Account
  -> Account
  -> [YearTS a]
aastaaruanded mkAnn ts koond aruandeperioodiKasum = foldr lisaLk [] $ tsByYear ts
  where
    -- | Lisa lõpukanne iga aasta lõppu
    lisaLk :: (Integer, [Transaction Day a]) -> [YearTS a] -> [YearTS a]
    lisaLk (year, ts) init = let
      ann = mkAnn year
      in (year, ts, lõpukanne ann ts koond aruandeperioodiKasum) : init

-- * Print

printKasumiaruanne
  :: forall t a . (Show t, Show a)
  => [Account]                     -- ^ Tööjõukontod (aruandevorm
                                   -- nõuabneid eraldi vãljatooduna)
  -> ByAccounts t a                -- ^ Tulud kontode kaupa
  -> ByAccounts t a                -- ^ Kulud kontode kaupa
  -> IO Amount
printKasumiaruanne tööjõukuluKontod tulud kulud = let
  (tööjõukulud', muudKulud') =
    partition' (\k _ -> k `elem` tööjõukuluKontod) kulud

  in do
  nl
  h2 "Kasumiaruanne"
  h3 "Tulu"
  tuludSaldo' <- printAccounts tulud
  let tuludSaldo = negate tuludSaldo'
  ridaEur "Kokku" tuludSaldo
  nl

  h3 "Kulu"
  h4 "Kaubad, toore, materjal ja teenused"
  muuKulu' <- printAccounts muudKulud'
  ridaEur "Kokku" muuKulu'
  nl

  h4 "Tööjõukulud"
  tööjõukulud' <- printAccounts tööjõukulud'
  ridaEur "Kokku" tööjõukulud'
  nl

  h3 "Ärikasum (kahjum)"
  ridaEur "Tulu kokku" tuludSaldo
  let kõikKulu = muuKulu' + tööjõukulud'
  ridaEur "Kulu kokku" kõikKulu
  let ärikasum = tuludSaldo - kõikKulu
  ridaEur "Ärikasum" ärikasum
  nl

  h3 "Aruandeaasta kasum"
  ridaEur "Kokku" ärikasum

  return ärikasum

printBilanss :: (Show t, Show a) => ByAccounts t a -> ByAccounts t a -> ByAccounts t a -> IO ()
printBilanss varad kohustised omakapital = let
  sum_ hm = sumIOs $ mconcat $ HM.elems hm :: Amount
  kohustisedKokku' = sum_ kohustised
  omakapitalKokku' = sum_ omakapital
  in do
  nl
  h2 "Bilanss"
  ridaEur "Käibevarad kokku" $ sum_ varad
  _ <- printAccounts varad

  ridaEur "Kohustised kokku" kohustisedKokku'
  _ <- printAccounts kohustised

  ridaEur "Omakapital kokku" omakapitalKokku'
  _ <- printAccounts omakapital

  ridaEur "Kohustised ja omakapital kokku" $ kohustisedKokku' + omakapitalKokku'

printAastaaruanne
  :: forall a. (Show a, Monoid a)
  => [Account]
  -> [Transaction Day a]
  -> [Transaction Day a]
  -> Maybe TS.Text -> IO ()
printAastaaruanne tööjõukuluKontod aastaTs kokkuTs mb = do
  nl
  nl
  nl
  tekst "================================================================="
  maybe (return ()) (tekst . TS.unpack) mb
  tekst "================================================================="

  let (_, _, _, tulud, kulud) = toAccountTypes aastaTs :: ByAccountTypes Day a
  _ <- printKasumiaruanne tööjõukuluKontod tulud kulud

  nl
  nl
  tekst "-----------------------------------------------------------------"
  let (varad, kohustised, omakapital, _, _) = toAccountTypes kokkuTs :: ByAccountTypes Day a
  printBilanss varad kohustised omakapital

printAastaaruanded
  :: forall a . (Show a, Monoid a)
  => (Integer -> Maybe a)
  -> Account
  -> Account
  -> [Account]
  -> Integer                       -- ^ Kuni aastani
  -> [Transaction Day a]
  -> IO ()
printAastaaruanded mkAnn ktKoond aruandeperioodiKasum tööjõukuluKontod kuniAastani ts = do
  let koosLk' = aastaaruanded mkAnn ts ktKoond aruandeperioodiKasum :: [YearTS a]
      koosLk = takeWhile (\(y, _, _) -> y <= kuniAastani) koosLk' :: [YearTS a]

      koosLkZip :: [(YearTS a, YearTS a)]
      koosLkZip@(((y, ts', _), _) :_) = koosLk `zip` tail koosLk

      -- | [(aasta, akumuleerunud kanded, selle aasta kanded)]
      aastadAccTs :: [(Integer, [Transaction Day a], [Transaction Day a])]
      aastadAccTs = let
        f acc@((_, tsAcc, _) : _) ((_, _, lkEelmineAasta), (seeAasta, tsSeeAasta, _)) = let
          tsAcc' = tsAcc <> (lkEelmineAasta : tsSeeAasta)
          in (seeAasta, tsAcc', tsSeeAasta) : acc
        in foldl' f [(y, ts', ts')] koosLkZip

  forM_ aastadAccTs $ \(aasta, tsAcc, tsAasta) -> do
    printAastaaruanne tööjõukuluKontod tsAasta tsAcc (Just $ TS.pack $ show aasta)
