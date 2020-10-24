module Eesti.Näide where

import LocalPrelude
import Accounting
import Eesti

-- * Kontod

pank, sisendKM, kassa, võlgMulle, ettemaksukonto, väljundKM, käibemaks, võlg, omakapital, tulu, kulu, palgakulu, sotsiaalmaks :: Account

-- | Varad (1)
pank = Account 1 "Pank"
sisendKM = Account 1 "Sisendkäibemaks"
kassa = Account 1 "Kassa"
võlgMulle = Account 1 "Võlg mulle"
ettemaksukonto = Account 1 "Tax office prepaid"

-- | Kohustised (2)
väljundKM = Account 2 "Väljundkäibemaks"
käibemaks = Account 2 "Käibemaks (käibemaksuvõlg riigile)"
võlg = Account 2 "Võlg (kellelegi teisele)"

-- | Omakapital (3)
omakapital = Account 3 "Omakapital"

-- | Tulud (4)
tulu = Account 4 "Tulu"

-- | Kulud (5)
kulu = Account 5 "Kulu"
palgakulu = Account 5 "Palgakulu"
sotsiaalmaks = Account 5 "Sotsiaalmaks"

-- * Annotatsioon

data Annotation = Annotation { description :: String } deriving Show

instance Semigroup Annotation where
  _ <> a = a

instance Monoid Annotation where
  mempty = Annotation mempty

kirjeldus :: String -> I Annotation ()
kirjeldus text = annotate $ Annotation text

-- * Tehingud

transactions :: [Transaction Day Annotation]
transactions = execT $ do

  at (stringDate "2020-01-15") $ do
    kirjeldus "Omakapital"
    omakapital ~> pank $ 2500.00

  at (stringDate "2020-02-15") $ do
    kirjeldus "Osutatud teenus firmale X"
    tulu ~> võlgMulle $ 4000
    väljundKM ~> võlgMulle $ 800
    võlgMulle ~> pank $ 4800

  at (stringDate "2020-02-20") $ do
    kirjeldus "Ost"
    võlg ~> kulu $ 300
    võlg ~> sisendKM $ 60
  at (stringDate "2020-02-24") $ do
    pank ~> võlg $ 360

  at (stringDate "2020-03-15") $ do
    kirjeldus "Käibemaks: 2020-02"
    sisendKM ~> käibemaks $ 60
    käibemaks ~> väljundKM $ 800
    pank ~> ettemaksukonto $ 740
    ettemaksukonto ~> käibemaks $ 740

-- * Käibemaksu deklaratsioonid

kmd :: IO ()
kmd = _KMDd sisendKM väljundKM transactions
