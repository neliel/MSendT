#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- Type   : module
-- Crée le: 17 Fév. 2013 à 15h23
-- Auteur : Sarfraz Kapasi
-- License: GPLv3

module MailDataT
( Provider (..)
, Auth     (..)
, Cred     (..)
, emptyCred
, gmail
) where

import qualified Data.Text.Lazy as LT

data Provider = Provider {server :: LT.Text, port :: Int}
data Auth     = Auth     {user   :: LT.Text, pass :: LT.Text}
data Cred     = Cred     {name   :: LT.Text, mail :: LT.Text, company :: LT.Text}

emptyCred :: Cred
emptyCred = Cred { name    = ""
                 , mail    = ""
                 , company = ""
                 }

gmail :: Provider
gmail = Provider {server = "smtp.gmail.com", port = 587}

--EOF
