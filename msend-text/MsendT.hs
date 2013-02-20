#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- Type   : module
-- Crée le: 17 Fév. 2013 à 16h46
-- Auteur : Sarfraz Kapasi
-- License: GPLv3

module MsendT
( emailT
) where

import Network
import Network.TLS
import Network.TLS.Extra
import System.IO
import Text.Printf
import Codec.Binary.Base64.String
import Control.Monad (unless)
import Data.Monoid   ( (<>) )
import Data.List     (isPrefixOf)
import qualified Crypto.Random.AESCtr       as RNG
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.Lazy             as LT
import MailDataT
import MIMEMsgT

cWrite :: Handle -> String -> IO ()
cWrite h s  = do
    hPrintf h "%s\r\n" s
    printf    "> %s\n" s

tWrite :: Context -> BL.ByteString -> IO ()
tWrite ctx bts  = do
    sendData ctx $ bts <> "\r\n"
    BL.putStrLn ("> " <> bts)

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

emailT :: Provider -> Auth -> Cred -> Cred -> LT.Text -> LT.Text -> [(LT.Text,FilePath)] -> IO ()
emailT provider auth from to subject email attachments = do
    let
      ciphers  = [cipher_AES128_SHA1,cipher_AES256_SHA1,cipher_RC4_128_MD5,cipher_RC4_128_SHA1]
      params   = defaultParamsClient{pCiphers = ciphers}
    g <- RNG.makeSystem
    mimeMail <- mimeMsg g from to subject email attachments
    h <- connectTo (LT.unpack $ server provider) (PortNumber (fromIntegral (port provider)))
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
    tWrite con $ BL.pack $ encode (LT.unpack $ user auth)
    tWaitFor con "334"
    tWrite con $ BL.pack $ encode (LT.unpack $ pass auth)
    tWaitFor con "235"
    tWrite con $ BL.pack $ LT.unpack ("MAIL FROM:<"<>(mail from)<>">")
    tWaitFor con "250"
    tWrite con $ BL.pack $ LT.unpack ("RCPT TO:<"<>(mail to)<>">")
    tWaitFor con "250"
    tWrite con "DATA"
    tWaitFor con "354"
    tWrite con $ BL.pack $ LT.unpack $ mimeMail
    tWrite con "\r\n."
    tWaitFor con "250"
    tWrite con "QUIT"
    tWaitFor con "221"
    bye con
--EOF
