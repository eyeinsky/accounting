module Accounting.Print where

import LocalPrelude
import Accounting.Core

title :: String -> IO ()
title t = putStrLn $ t <> "\n"

h1 :: String -> IO ()
h1 t = title $ "* " <> t

h2 :: String -> IO ()
h2 t = title $ "** " <> t

h3 :: String -> IO ()
h3 t = title $ "*** " <> t

h4 :: String -> IO ()
h4 t = title $ "**** " <> t

rida :: Show a => String -> a -> IO ()
rida tekst väärtus = putStrLn $ tekst <> ": " <> show väärtus

ridaEur :: String -> Amount -> IO ()
ridaEur tekst v = putStrLn $ tekst <> ": " <> str <> " (" <> show v <> ")"
  where
    str = formatScientific Fixed (Just 0) $ coerce v

tekst :: String -> IO ()
tekst = putStrLn

tekst' :: String -> IO ()
tekst' = putStr

nl :: IO ()
nl = putStrLn ""
