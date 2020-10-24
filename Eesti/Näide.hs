module Eesti.Näide where

import LocalPrelude
import Accounting
import Eesti

-- * Kontod

vara, kohustis, tulu', kulu' :: Int -> String -> Account
vara n kirjeldus = Account (100 + n) kirjeldus
kohustis n kirjeldus = Account (200 + n) kirjeldus
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
omakapital = Account 3 "Omakapital"

-- | Tulud (4)
tulu = tulu' 0 "Tulu"

-- | Kulud (5)
kulu = kulu' 0 "Kulu"
palgakulu = kulu' 1 "Palgakulu"
sotsiaalmaks = kulu' 2 "Sotsiaalmaks"

-- * Annotatsioon

data Annotation = Annotation { description :: String }

instance Show Annotation where
  show (Annotation k) = show k

instance Semigroup Annotation where
  _ <> a = a

instance Monoid Annotation where
  mempty = Annotation mempty

kirjeldus :: String -> I Annotation ()
kirjeldus text = annotate $ Annotation text

-- * Tehingud

tehingud :: [Transaction Day Annotation]
tehingud = execT $ do

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
    sisendKM ~> käibemaks $ 60.0 + 4.4
    käibemaks ~> väljundKM $ 800
    pank ~> ettemaksukonto $ 735.6
    ettemaksukonto ~> käibemaks $ 735.6

-- * Raportid

main :: IO ()
main = do
  nl

  -- | Käibemaksudeklaratsioonid
  _KMDd sisendKM väljundKM tehingud
  nl

  -- | Käibemaksu kontode saldo
  tekst "Käibemaksukontod:"
  kontosaldo sisendKM tehingud
  kontosaldo väljundKM tehingud
  kontosaldo käibemaks tehingud
  nl

  -- | Pangasaldo
  kontosaldo pank tehingud
  nl
