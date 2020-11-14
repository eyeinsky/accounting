
Väljund [siit algavate tehingute](https://github.com/eyeinsky/accounting/blob/2f462140021423ad93963b252d967b273f92f337/Eesti/N%C3%A4ide.hs#L63)
kohta:

```
$ stack repl

*LocalPrelude Accounting Accounting.Core Accounting.DSL Accounting.Reports Data.Time.Lens Eesti Eesti.Näide LocalPrelude> :l Eesti.Näide
*Eesti.Näide> main

Käibemaksudeklaratsioonid:

2020-2:
  KMD INF A (Müügitehingud):
    - tehingu kuupäev ja kirjeldus: 2020-02-15/Müük: müügiarve, ostja käibemaksukohuslane
      deklareeritud käive: 800.0      arve summa käibemaksuta: 4000.0


  KMD põhivorm:
    KMD 1 (20% määraga maksustatavad toimingud ja tehingud): 4000.0
    KMD 5 ja 5.1 (mahaarvamised kokku): 64.4 (= 60.0 + 4.4)
    KMD 12 (tasumisele kuuluv käibemaks): 735.6

Käibemaksu kontode saldo:
Sisendkäibemaks: 0.0
Väljundkäibemaks: 0.0
Käibemaks (käibemaksuvõlg riigile): 0.0

Pank: 6178.0
```

`prindiKMDd` prindib käibemaksudeklaratsioonid kuude kaupa, väljundi
alusel saab:
- täita EMTA kodulehel käibemaksudeklaratsiooni
- teha käibemaksu äramaksmise kande oma raamatupidamisse (ja maksta
  käibemaks muidugi ka ära),
  [nagu siin veebruari kohta](https://github.com/eyeinsky/accounting/blob/2f462140021423ad93963b252d967b273f92f337/Eesti/N%C3%A4ide.hs#L89)
