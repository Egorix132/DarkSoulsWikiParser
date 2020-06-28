{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( parseDSWiki
    ) where
        
import           Conduit
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Lazy (toStrict)
import           Data.List
import           Data.Maybe
import           GHC.IO.Encoding
import           Network.HTTP.Conduit
import qualified Network.HTTP.Simple
import           Text.HTML.Scalpel
import           Text.HTML.TagSoup 



parseDSWiki :: IO ()
parseDSWiki =  do
    setLocaleEncoding utf8
    parseResPage "https://darksouls.fandom.com/wiki/Special:Search?search=&fulltext=Search&scope=internal&ns0=1&ns14=1#"
    
    
parseResPage :: String -> IO ()
parseResPage url = do
    html <- getResponseBody url
    let pages = nub $ fromJust $ findLinksFromResults html
    pagesHtml <- sequence $ map getResponseBody pages
    appendFile "descriptions.txt" $ unlines $ catMaybes $ parsePages pagesHtml
    let nextPage = fromJust $ findNextPage html
    if null nextPage
        then return ()
        else parseResPage $ head $ nextPage

    
parsePages :: [String] -> [Maybe String]
parsePages pages = map parsePage pages

parsePage :: String -> Maybe String
parsePage page = 
    if null description 
    then Nothing
    else Just $ (unlines description) ++ "\n" 
        where mbDescr = findDescription page
              description = if isJust mbDescr 
                  then filter (not . null) $ fromJust mbDescr 
                  else []
      

findDescription :: String -> Maybe [String]
findDescription s = scrapeStringLike s results
    where
        results :: Scraper String [String]
        results = texts (("div" @: [hasClass "quote"]) // "dl" // "dd" `atDepth` 1 // "span" `atDepth` 1 // "i" `atDepth` 1)

findLinksFromResults :: String -> Maybe [String]
findLinksFromResults s = scrapeStringLike s results
    where
        results :: Scraper String [String]
        results = attrs "href" $ (TagString "a") @: [hasClass "result-link"]

findNextPage :: String -> Maybe [String]
findNextPage s = scrapeStringLike s results
    where
        results :: Scraper String [String]
        results = attrs "href" $ (TagString "a") @: [hasClass "paginator-next", hasClass "button", hasClass "secondary"]

getResponseBody :: String -> IO (String)
getResponseBody url = do
      lazyBody <- simpleHttp url
      return $ B8.unpack $ toStrict lazyBody