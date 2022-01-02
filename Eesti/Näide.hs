module Eesti.Näide where

import LocalPrelude
import Accounting
import Eesti
import Eesti.Aastaaruanne

-- * Kontod

vara, kohustis, omakapital', tulu', kulu' :: Int -> String -> Account
vara n kirjeldus = Account (100 + n) kirjeldus
kohustis n kirjeldus = Account (200 + n) kirjeldus
omakapital' n kirjeldus = Account (300 + n) kirjeldus
tulu' n kirjeldus = Account (400 + n) kirjeldus
kulu' n kirjeldus = Account (500 + n) kirjeldus

pank, sisendKM, kassa, võlgMulle, ettemaksukonto, väljundKM, käibemaks, võlg, omakapital, tulu, kulu, palgakulu, sotsiaalmaks :: Account


-- | Varad (1)
pank = vara 0 "Pank"
sisendKM = vara 1 "Sisendkäibemaks"
kassa = vara 2 "Kassa"
võlgMulle = vara 3 "Võlg mulle"
ettemaksukonto = vara 4 "Tax office prepaid"

-- | Kohustised (2)
väljundKM = kohustis 0 "Väljundkäibemaks"
käibemaks = kohustis 1 "Käibemaks (käibemaksuvõlg riigile)"
võlg = kohustis 2 "Võlg (kellelegi teisele)"

-- | Omakapital (3)
omakapital = omakapital' 1 "Omakapital"

ktKoond :: Account
ktKoond = omakapital' 2 "Koondkonto"

aruandeperioodiKasum :: Account
aruandeperioodiKasum = omakapital' 3 "Aruandeperioodi kasum"

-- | Tulud (4)
tulu = tulu' 0 "Tulu"

-- | Kulud (5)
kulu = kulu' 0 "Kulu"
palgakulu = kulu' 1 "Palgakulu"
sotsiaalmaks = kulu' 2 "Sotsiaalmaks"

tööjõukulud :: [Account]
tööjõukulud = [palgakulu, sotsiaalmaks]

-- * Annotatsioon

data Annotation = Annotation { description :: Maybe String }

instance Show Annotation where
  show (Annotation k) = maybe "" show k

instance Semigroup Annotation where
  Annotation a <> Annotation b = Annotation $ if
    | isJust a && isJust b -> b
    | isJust a -> a
    | isJust b -> b
    | otherwise -> Nothing

instance Monoid Annotation where
  mempty = Annotation mempty

kirjeldus :: String -> I Annotation ()
kirjeldus text = annotate $ Annotation $ Just text

-- * Tehingud

tehingud :: [Transaction Day Annotation]
tehingud = execT $ do

  -- * 2020

  at (stringDate "2020-01-15") $ do
    kirjeldus "Omakapital"
    omakapital ~> pank $ 2500.00

  at (stringDate "2020-02-15") $ do
    kirjeldus "Müük: müügiarve, ostja käibemaksukohuslane"
    tulu ~> võlgMulle $ 4000
    väljundKM ~> võlgMulle $ 800
    võlgMulle ~> pank $ 4800

  at (stringDate "2020-02-20") $ do
    kirjeldus "Ost: arvega ostetud asi"
    võlg ~> kulu $ 300
    võlg ~> sisendKM $ 60
  at (stringDate "2020-02-24") $ do
    pank ~> võlg $ 360

  at (stringDate "2020-02-25") $ do
    kirjeldus "Ost: teine, kohe makstud asi"
    võlg ~> kulu $ 22
    võlg ~> sisendKM $ 4.4
    pank ~> võlg $ 26.4

  at (stringDate "2020-03-15") $ do
    kirjeldus "Käibemaks: 2020-02"
    -- | Kanname käibemaksu konto @käibemaks@ peale kokku. Sisend ja
    -- väljund on peale seda nullis.
    sisendKM ~> käibemaks $ 64.4
    käibemaks ~> väljundKM $ 800
    -- | Kanname ettemaksu maksuametisse
    pank ~> ettemaksukonto $ 735.6
    -- | Käibemaksuvõlg ja ettemaks lahenduvad nulliks
    ettemaksukonto ~> käibemaks $ 735.6

  -- * 2021

  at (stringDate "2021-02-15") $ do
    kirjeldus "Müük: teenuse osutamine"
    tulu ~> võlgMulle $ 5000
    väljundKM ~> võlgMulle $ 1000
    võlgMulle ~> pank $ 6000

  at (stringDate "2021-02-20") $ do
    kirjeldus "Ost: arvega ostetud asi, nt monitor"
    võlg ~> kulu $ 400
    võlg ~> sisendKM $ 80
  at (stringDate "2021-02-24") $ do
    pank ~> võlg $ 480

  at (stringDate "2021-02-25") $ do
    kirjeldus "Ost: teine, kohe makstud asi"
    võlg ~> kulu $ 50
    võlg ~> sisendKM $ 10
    pank ~> võlg $ 60

  at (stringDate "2021-03-15") $ do
    kirjeldus "Käibemaks: 2021-02"
    -- | Kanname käibemaksu konto @käibemaks@ peale kokku. Sisend ja
    -- väljund on peale seda nullis.
    sisendKM ~> käibemaks $ 90
    käibemaks ~> väljundKM $ 1000
    -- | Kanname ettemaksu maksuametisse
    pank ~> ettemaksukonto $ 910
    -- | Käibemaksuvõlg ja ettemaks lahenduvad nulliks
    ettemaksukonto ~> käibemaks $ 910

-- * Raportid

main :: IO ()
main = do
  nl

  tekst "Käibemaksudeklaratsioonid:"
  prindiKMDd sisendKM väljundKM tulu tehingud
  nl

  tekst "Käibemaksu kontode saldo:"
  kontosaldo sisendKM tehingud
  kontosaldo väljundKM tehingud
  kontosaldo käibemaks tehingud
  nl

  -- | Pangasaldo
  kontosaldo pank tehingud
  nl


  nl
  let
    lõpukandeAnnotatsioon :: Integer -> Maybe Annotation
    lõpukandeAnnotatsioon aasta = Just $ Annotation $ Just $ show aasta <> " lõpukanne"
  printAastaaruanded lõpukandeAnnotatsioon ktKoond aruandeperioodiKasum tööjõukulud tehingud
