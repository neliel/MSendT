{-# LANGUAGE OverloadedStrings #-}

-- Type   : module
-- Crée le: 17 Fév. 2013 à 16h46
-- Auteur : Sarfraz Kapasi
-- License: GPLv3

module MSendT
( Provider (..)
, Auth     (..)
, Cred     (..)
, emptyAuth
, emptyCred
, gmail
, mimeMsg
, emailT
) where

import Network
import Network.TLS
import Network.TLS.Extra
import System.Cmd          (rawSystem)
import System.Random
import System.IO
import System.FilePath
import Text.Printf
import Control.Applicative ( (<$>) )
import Control.Monad       (unless)
import Data.Monoid         ( (<>) )
import Data.List           (isPrefixOf)
import qualified Crypto.Random.AESCtr        as RNG (makeSystem)
import qualified Data.ByteString.Char8       as BC
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.UTF8   as BLU (fromString,toString)
import qualified Data.ByteString.Base64.Lazy as BE  (encode)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as T

--- Data Part ---

data Provider = Provider {server :: T.Text, port :: Int}
data Auth     = Auth     {user   :: T.Text, pass :: T.Text}
data Cred     = Cred     {name   :: T.Text, mail :: T.Text, company :: T.Text}

emptyAuth :: Auth
emptyAuth = Auth
  { user = ""
  , pass = ""
  }

emptyCred :: Cred
emptyCred = Cred { name    = ""
                 , mail    = ""
                 , company = ""
                 }

gmail :: Provider
gmail = Provider {server = "smtp.gmail.com", port = 587}

--- MIME Part ---

mimeMsgT :: RandomGen a => a -> Cred -> Cred -> T.Text -> T.Text -> (T.Text,T.Text)
mimeMsgT g from to subject body =
  ("From: " <> name from <> " <" <> mail from <> ">" <> "\n"
  <> "To: " <> name to <> " <" <> mail to <> ">" <> "\n"
  <> "Subject: " <> subject <> "\n"
  <> "MIME-Version: 1.0" <> "\n"
  <> "Content-Type: multipart/mixed; boundary=" <> boundary <> "\n\n"
  <> "--" <> boundary <> "\n"
  <> "Content-Type: text/plain; charset=utf-8" <> "\n"
  <> "Content-Transfer-Encoding: 8bit" <> "\n\n"
  <> body <> "\n\n", boundary)
 where
  boundary = T.pack $ take 20 $ randomRs ('a','z') g

mimeMsgA :: T.Text -> [(T.Text,FilePath)] -> IO T.Text
mimeMsgA boundary attachments =
  if null attachments
     then return ""
     else T.concat <$> mapM (mimeA boundary) attachments

mimeA :: T.Text -> (T.Text,FilePath) -> IO T.Text
mimeA boundary attachment =
  BL.readFile (snd attachment) >>=
  (\x -> return $ "--" <> boundary <> "\n"
  <> "Content-Type: " <> fst attachment <> "\n"
  <> "Content-Disposition: attachment; filename=\"" <> T.pack (takeFileName $ snd attachment) <> "\"" <> "\n"
  <> "Content-Transfer-Encoding: base64" <> "\n\n"
  <> T.pack (BLU.toString $ BE.encode x) <> "\n\n")

mimeMsgC :: T.Text -> T.Text
mimeMsgC boundary =
  "--" <> boundary <> "--"

mimeMsg :: RandomGen a => a -> Cred -> Cred -> T.Text  -> T.Text -> [(T.Text,FilePath)] -> IO T.Text
mimeMsg g from to subject body attachments = do
  msgA <- mimeMsgA (snd msgT) attachments
  return $ fst msgT <> msgA <> mimeMsgC (snd msgT)
 where
  msgT = mimeMsgT g from to subject body

--- Mail Part ----

cWrite :: Handle -> String -> IO ()
cWrite h s  = do
    hPrintf h "%s\r\n" s
    printf    "> %s\n" s

cWaitFor :: Handle -> String -> IO ()
cWaitFor h str = do
    ln <- hGetLine h
    putStrLn ln
    unless (str `isPrefixOf` ln) (cWaitFor h str)

tWrite :: Context -> BL.ByteString -> IO ()
tWrite ctx bts  = do
    sendData ctx $ bts <> "\r\n"
    BL.putStrLn ("> " <> bts)

tWaitFor :: Context -> BC.ByteString -> IO ()
tWaitFor ctx bts = do
    dat <- recvData ctx
    BC.putStrLn dat
    unless (bts `BC.isPrefixOf` dat) (tWaitFor ctx bts)

emailT :: Provider -> Auth -> Cred -> Cred -> T.Text -> T.Text -> [(T.Text,FilePath)] -> IO ()
emailT provider auth from to subject email attachments = do
    let
      ciphers  = [cipher_AES128_SHA1,cipher_AES256_SHA1,cipher_RC4_128_MD5,cipher_RC4_128_SHA1]
      params   = defaultParamsClient{pCiphers = ciphers}
    g <- RNG.makeSystem
    mimeMail <- mimeMsg g from to subject email attachments
    h <- connectTo (T.unpack $ server provider) (PortNumber (fromIntegral (port provider)))
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
    tWrite con $ BE.encode $ BLU.fromString $ T.unpack $ user auth
    tWaitFor con "334"
    tWrite con $ BE.encode $ BLU.fromString $ T.unpack $ pass auth
    tWaitFor con "235"
    tWrite con $ BLU.fromString $ T.unpack ("MAIL FROM:<"<> mail from <>">")
    tWaitFor con "250"
    tWrite con $ BLU.fromString $ T.unpack ("RCPT TO:<"<> mail to <>">")
    tWaitFor con "250"
    tWrite con "DATA"
    tWaitFor con "354"
    tWrite con $ BLU.fromString $ T.unpack mimeMail
    tWrite con "\r\n."
    tWaitFor con "250"
    tWrite con "QUIT"
    tWaitFor con "221"
    bye con
--EOF
