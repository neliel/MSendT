#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- Type   : module
-- Crée le: 17 Fév. 2013 à 16h46
-- Auteur : Sarfraz Kapasi
-- License: GPLv3

module Msend
( emailT
) where

import Network
import Network.TLS
import Network.TLS.Extra
import System.IO
import Text.Printf
import Codec.Binary.Base64.String
import Control.Monad (unless)
import Data.List     (isPrefixOf)
import qualified Crypto.Random.AESCtr       as RNG
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import MailData
import MIMEMsg

cWrite :: Handle -> String -> IO ()
cWrite h s  = do
    hPrintf h "%s\r\n" s
    printf    "> %s\n" s

tWrite :: Context -> BL.ByteString -> IO ()
tWrite ctx bts  = do
    sendData ctx $ bts `BL.append` "\r\n"
    printf    "> %s\n" $ BL.unpack bts

tWaitFor :: Context -> BC.ByteString -> IO ()
tWaitFor ctx bts = do
    dat <- recvData ctx
    BC.putStrLn dat
    unless (bts `BC.isPrefixOf` dat) (tWaitFor ctx bts)

cWaitFor :: Handle -> String -> IO ()
cWaitFor h str = do
    ln <- hGetLine h
    putStrLn ln
    unless (str `isPrefixOf` ln) (cWaitFor h str)

emailT :: Provider -> Auth -> Cred -> Cred -> String -> String -> [(String,FilePath)] -> IO ()
emailT provider auth from to subject email attachments = do
    let
      ciphers  = [cipher_AES128_SHA1,cipher_AES256_SHA1,cipher_RC4_128_MD5,cipher_RC4_128_SHA1]
      params   = defaultParamsClient{pCiphers = ciphers}
    g <- RNG.makeSystem
    mimeMail <- mimeMsg g from to subject email attachments
    h <- connectTo (server provider) (PortNumber (fromIntegral (port provider)))
    hSetBuffering h LineBuffering
    cWrite h "EHLO"
    cWaitFor h "250-STARTTLS"
    cWrite h "STARTTLS"
    cWaitFor h "220"
    con <- contextNewOnHandle h params g
    handshake con
    tWrite con "EHLO"
    tWaitFor con "250"
    tWrite con "AUTH LOGIN"
    tWaitFor con "334"
    tWrite con $ BL.pack $ encode (user auth)
    tWaitFor con "334"
    tWrite con $ BL.pack $ encode (pass auth)
    tWaitFor con "235"
    tWrite con $ BL.pack ("MAIL FROM:<"++(mail from)++">")
    tWaitFor con "250"
    tWrite con $ BL.pack ("RCPT TO:<"++(mail to)++">")
    tWaitFor con "250"
    tWrite con "DATA"
    tWaitFor con "354"
    tWrite con $ BL.pack $ mimeMail
    tWrite con $ BL.pack "\r\n."
    tWaitFor con "250"
    tWrite con "QUIT"
    tWaitFor con "221"
    bye con
--EOF
