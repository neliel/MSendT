#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- Type   : script
-- Crée le: 20 Fév. 2013 à 10h24
-- Auteur : Sarfraz Kapasi
-- License: GPLv3

import MsendT
import qualified Data.Text    as T
import qualified Data.Text.IO as T

testAuth :: Auth
testAuth = Auth {user = "jacob.ilyane", pass ="00jacoubi11"}

testCred :: Cred
testCred =
  emptyCred { name  = "Jacob Ilyane"
            ,  mail = "jacob.ilyane@gmail.com"
            }

main :: IO ()
main =
  emailT gmail testAuth testCred testCred "tést" "teéàtContent" []


-- EOF
