module CharacterSets where

import qualified Data.HashSet as HS

-- following whitespace after tokens or comments
space_chars :: String
space_chars = " \n" --todo: put these back \t\r\v\f\160\5760\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288

-- underline + letters + digits
uld :: HS.HashSet Char
uld = HS.fromList $ '_' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
digits :: HS.HashSet Char
digits = HS.fromList $ ['0'..'9']
letters :: HS.HashSet Char
letters = HS.fromList $ ['a'..'z'] ++ ['A'..'Z']

-- leaves out \
graphic_character :: HS.HashSet Char
graphic_character = HS.fromList
  $  "ABCDEFGHIJKLMNOPQRSTUVWXYZАБВÃДЕЖЗИЙКЛМНОПСТУÔÕЦØЩЪЫЬÝÞ" --upperCase
  ++ "0123456789" -- digits
  ++ "\"#&'()*+,-./:;<=>[]_|" -- special
  ++ "\t\v\f \160\5760\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288" -- space excludes new-lines because 13.2 125 "The end of a line is always a seperator"
  ++ "abcdefghijklmnopqrstuvwxyzЯабвгдежзийклмнопðстуфхцøщъыьýþя" -- lowerCase
  ++ "!$%@?^`{}~¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»���¿×÷�" -- other special

-- leaves out \ and single quote
graphic_character_s :: HS.HashSet Char
graphic_character_s = HS.fromList
  $  "ABCDEFGHIJKLMNOPQRSTUVWXYZАБВÃДЕЖЗИЙКЛМНОПСТУÔÕЦØЩЪЫЬÝÞ" --upperCase
  ++ "0123456789" -- digits
  ++ "\"#&()*+,-./:;<=>[]_|" -- special
  ++ "\t\v\f \160\5760\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288" -- space excludes new-lines because 13.2 125 "The end of a line is always a seperator"
  ++ "abcdefghijklmnopqrstuvwxyzЯабвгдежзийклмнопðстуфхцøщъыьýþя" -- lowerCase
  ++ "!$%@?^`{}~¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»���¿×÷�" -- other special

-- leaves out \ and double quote
graphic_character_d :: HS.HashSet Char
graphic_character_d = HS.fromList
  $  "ABCDEFGHIJKLMNOPQRSTUVWXYZАБВÃДЕЖЗИЙКЛМНОПСТУÔÕЦØЩЪЫЬÝÞ" --upperCase
  ++ "0123456789" -- digits
  ++ "#&'()*+,-./:;<=>[]_|" -- special
  ++ "\t\v\f \160\5760\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288" -- space excludes new-lines because 13.2 125 "The end of a line is always a seperator"
  ++ "abcdefghijklmnopqrstuvwxyzЯабвгдежзийклмнопðстуфхцøщъыьýþя" -- lowerCase
  ++ "!$%@?^`{}~¡¢£¤¥¦§¨©ª«¬®¯°±²³´µ¶·¸¹º»���¿×÷�" -- other special

letterOrDigit :: HS.HashSet Char
letterOrDigit = HS.fromList
  $  "ABCDEFGHIJKLMNOPQRSTUVWXYZАБВÃДЕЖЗИЙКЛМНОПСТУÔÕЦØЩЪЫЬÝÞ" --upperCase
  ++ "0123456789" -- digits
  ++ "abcdefghijklmnopqrstuvwxyzЯабвгдежзийклмнопðстуфхцøщъыьýþя" -- lowerCase

-- A separator is either a space character (except in comments and literals), a format effector, or the end of a line.
-- end of file is also a separator
delimitersNl :: HS.HashSet Char
delimitersNl = HS.fromList "&'()*+,-./:;<=>|[]"

whitespace :: HS.HashSet Char
whitespace = HS.fromList "\t\n\v\f\r \160\5760\8192\8193\8194\8195\8196\8197\8198\8199\8200\8201\8202\8239\8287\12288"
