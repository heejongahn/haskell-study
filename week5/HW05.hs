{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map, insert, update, foldlWithKey, elemAt, toList)
import Data.Bits (xor)
import Data.Word (Word8)
import Data.Maybe (fromJust)
import Data.List (sortBy)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getXor :: (Word8, Word8) -> Word8
getXor (a, b) = xor a b

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret originalPath corruptedPath = do
  original <- BS.readFile originalPath
  let originalWords = BS.unpack original

  corrupted <- BS.readFile corruptedPath
  let corruptedWords = BS.unpack corrupted

  let xorList = map getXor $ zip originalWords corruptedWords
  let result = BS.pack $ filter (\x -> x /= 0) xorList

  return result

-- Exercise 2 -----------------------------------------

decrpytWords :: [Word8] -> [Word8] -> [Word8]
decrpytWords [] [] = []
decryptWords [] _ = []
decryptWords _ [] = []
decryptWords (x:xs) (y:ys) = (xor x y) : decryptWords xs ys

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key decryptedPath = do
  let keyWords = BS.unpack key
  let encryptedPath = decryptedPath ++ ".enc"

  encrypted <- BS.readFile encryptedPath
  let encryptedWords = BS.unpack encrypted
  let paddedKeyWords = foldl (++) [] (take (length encryptedWords) $ repeat keyWords)

  let decryptedWords = decryptWords paddedKeyWords encryptedWords

  BS.writeFile decryptedPath $ BS.pack decryptedWords

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
  content <- BS.readFile path

  let parsed = decode content
  return parsed

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimPath transactionPath = do
  victims <- parseFile victimPath :: IO (Maybe [TId])
  transactions <- parseFile transactionPath :: IO (Maybe [Transaction])

  return (Just (filter (\t -> elem (tid t) (fromJust victims)) (fromJust transactions)))

-- Exercise 5 -----------------------------------------

initMap :: Map String Integer -> [Transaction] -> Map String Integer
initMap m [] = m
initMap m ((Transaction from to _ _):ts) =
  initMap (insert from 0 (insert to 0 m)) ts

getFlowIter :: Map String Integer -> [Transaction] -> Map String Integer
getFlowIter m [] = m
getFlowIter m ((Transaction from to amount _):ts) =
  getFlowIter (update (\x -> Just(x-amount)) from m') ts
    where m' = update (\x -> Just(x+amount)) to m

getFlow :: [Transaction] -> Map String Integer
getFlow t = getFlowIter (initMap Map.empty t) t

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal m =
  fst (foldlWithKey compare (elemAt 0 m) m)
    where compare = (\(name, money) k v -> if v > money then (k, v) else (name, money))

-- Exercise 7 -----------------------------------------

seperate :: Map String Integer -> ([(String, Integer)], [(String, Integer)])
seperate m = ((sortBy compare payers), (sortBy compare payees))
  where
    payers = filter (\(k, v) -> v > 0) $ toList m
    payees = filter (\(k, v) -> v < 0) $ toList m
    compare (k1, v1) (k2, v2)
        | v1 < v2 = LT
        | v1 == v2 = EQ
        | v1 > v2 = GT

settle :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction] -> [Transaction]
settle [] [] _ ts = ts
settle ((k1, v1):r1) ((k2, v2):r2) (tid:tids) ts
  | v1 > (abs v2)  = settle ((k1, v1+v2):r1) r2 tids ((Transaction k1 k2 (abs v2) tid):ts)
  | v1 == (abs v2) = settle r1 r2 tids ((Transaction k1 k2 v1 tid):ts)
  | v1 < (abs v2)  = settle r1 ((k2, v1+v2):r2) tids ((Transaction k1 k2 v1 tid):ts)

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids = settle payers payees tids []
  where (payers, payees) = seperate m

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path content = BS.writeFile path $ encode content

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "week5/dog-original.jpg"
                        "week5/dog.jpg"
                        "week5/transactions.json"
                        "week5/victims.json"
                        "week5/new-ids.json"
                        "week5/new-transactions.json"
  putStrLn crim

