#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import Network
import Network.TLS
import Network.TLS.Extra
import System.IO
import Text.Printf
import Codec.Binary.Base64.String
import Network.Mail.Mime
import Control.Monad (when)
import Data.List     (isPrefixOf)
import qualified Crypto.Random.AESCtr       as RNG
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

main :: IO ()
main = emailT "smtp.gmail.com" 587

cWrite :: Handle -> String -> IO ()
cWrite h s  = do
    hPrintf h "%s\r\n" s
    printf    "> %s\n" s

tWrite :: Context -> BL.ByteString -> IO ()
tWrite ctx bts  = do
    sendData ctx $ bts `BL.append` "\r\n"
    let
      s = BL.unpack bts
    printf    "> %s\n" s

tWaitFor :: Context -> BC.ByteString -> IO ()
tWaitFor ctx bts = do
    dat <- recvData ctx
    BC.putStrLn dat
    when (not $ bts `BC.isPrefixOf` dat) (tWaitFor ctx bts)

cWaitFor :: Handle -> String -> IO ()
cWaitFor h str = do
    ln <- hGetLine h
    putStrLn ln
    when (not $ str `isPrefixOf` ln) (cWaitFor h str)

emailT :: String -> Int -> IO ()
emailT host port = do
    let
      ciphers = [cipher_AES128_SHA1,cipher_AES256_SHA1,cipher_RC4_128_MD5,cipher_RC4_128_SHA1]
      params  = defaultParamsClient{pCiphers = ciphers}
    g <- RNG.makeSystem
    h <- connectTo host (PortNumber (fromIntegral port))
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
    tWrite con $ BL.pack $ encode "skh.unvrs"
    tWaitFor con "334"
    tWrite con $ BL.pack $ encode "theeSh2A"
    tWaitFor con "235"
    tWrite con "MAIL FROM:<skh.unvrs@gmail.com>"
    tWaitFor con "250"
    tWrite con "RCPT TO:<skh.unvrs@gmail.com>"
    tWaitFor con "250"
    tWrite con "DATA"
    tWaitFor con "354"
    tWrite con "From:skh.unvrs@gmail.com\nTo:skh.unvrs@gmail.com\nSubject:sujet\nCorps du message\r\n."
    tWaitFor con "250"
    tWrite con "QUIT"
    tWaitFor con "221"
    bye con
--EOF
