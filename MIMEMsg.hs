#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- Type   : module
-- Crée le: 17 Fév. 2013 à 15h23
-- Auteur : Sarfraz Kapasi
-- License: GPLv3

module MIMEMsg
( mimeMsg
) where

import Control.Applicative ( (<$>) )
import System.Random
import System.FilePath
import MailData
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.ByteString.Base64.Lazy

mimeMsgT :: RandomGen a => a -> Cred -> Cred -> String -> String -> (String,String)
mimeMsgT g from to subject body =
  ("From: " ++ name from ++ " <" ++ mail from ++ ">" ++ "\n"
  ++ "To: " ++ name to ++ " <" ++ mail to ++ ">" ++ "\n"
  ++ "Subject: " ++ subject ++ "\n"
  ++ "MIME-Version: 1.0" ++ "\n"
  ++ "Content-Type: multipart/mixed; boundary=" ++ boundary ++ "\n\n"
  ++ "--" ++ boundary ++ "\n"
  ++ "Content-Type: text/plain; charset=utf-8" ++ "\n"
  ++ "Content-Transfer-Encoding: 8bit" ++ "\n\n"
  ++ body ++ "\n\n", boundary)
 where
  boundary = take 20 $ randomRs ('a','z') g

mimeMsgA :: String -> [(String,FilePath)] -> IO String
mimeMsgA boundary attachments =
  if null attachments
     then return ""
     else concat <$> mapM (mimeA boundary) attachments

mimeA :: String -> (String,FilePath) -> IO String
mimeA boundary attachment =
  BL.readFile (snd attachment) >>=
  (\x -> return $ "--" ++ boundary ++ "\n"
  ++ "Content-Type: " ++ fst attachment ++ "\n"
  ++ "Content-Disposition: attachment; filename=\"" ++ takeFileName (snd attachment) ++ "\"" ++ "\n"
  ++ "Content-Transfer-Encoding: base64" ++ "\n\n"
  ++ BL.unpack (encode x) ++ "\n\n")

mimeMsgC :: String -> String
mimeMsgC boundary =
  "--" ++ boundary ++ "--"

mimeMsg :: RandomGen a => a -> Cred -> Cred -> String  -> String -> [(String,FilePath)] -> IO String
mimeMsg g from to subject body attachments = do
  msgA <- mimeMsgA (snd msgT) attachments
  return $ fst msgT ++ msgA ++ mimeMsgC (snd msgT)
 where
  msgT = mimeMsgT g from to subject body

--EOF
