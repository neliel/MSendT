#!/usr/bin/env runhaskell
-- Type   : module
-- Crée le: 17 Fév. 2013 à 15h23
-- Auteur : Sarfraz Kapasi
-- License: GPLv3

module MailData
( Provider (..)
, Auth     (..)
, Cred     (..)
, emptyCred
, gmail
) where
{-# LANGUAGE OverloadedStrings #-}

data Provider = Provider {server :: String, port :: Int}
data Auth     = Auth     {user   :: String, pass :: String}
data Cred     = Cred     {name   :: String, mail :: String, company :: String}

emptyCred :: Cred
emptyCred = Cred { name    = ""
                 , mail    = ""
                 , company = ""
                 }

gmail :: Provider
gmail = Provider {server = "smtp.gmail.com", port = 587}

--EOF
