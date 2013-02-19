#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

-- Type   : module
-- Crée le: 17 Fév. 2013 à 20h15
-- Auteur : Sarfraz Kapasi
-- License: GPLv3

module TexTmp
( genLettre
) where

import System.Directory
import System.Cmd
import MailData

headerPart1 dest = unlines
              ["\\documentclass[12pt,a4paper]{lettre}"
              ,"\\usepackage[utf8]{inputenc}"
              ,"\\usepackage[T1]{fontenc}"
              ,"\\usepackage{lmodern}"
              ,"\\usepackage[french]{babel}"
              ,"\\institut{sig}"
              ,"\\begin{document}"
              ,("\\begin{letter}{" ++dest++ "} % Destinataire")
              ]
headerPart2 obj = unlines
               ["\\def\\concname{Objet :~}"
               ,("\\conc{"++obj++"} % Objet de la lettre")
               ,"\\opening{Madame, Monsieur,} % Ouverture"
               ]
footer = unlines
         ["\\closing{Dans l'attente d'une r\\'{e}ponse, je vous prie de croire, Madame, Monsieur, \\`{a} l'assurance de ma consid\\'{e}ration distingu\\'{e}e,} % Salutations"
         ,"\\end{letter}"
         ,"\\end{document}"
         ]


genLettre :: FilePath -> Cred -> String -> String -> IO ()
genLettre path to subject body =
  writeFile path $ headerPart1 (name to) ++ headerPart2 subject ++ body ++ footer

--EOF
