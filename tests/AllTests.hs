{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Monad (join)
import DBPedia -- to ensure the DBPedia.hs file compiles
import Database.HSparql.ConnectionTest
import Database.HSparql.QueryGeneratorTest
import Network.HTTP.Types (status200, status204, status400, hContentLength)
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Test.Framework
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (empty, toStrict, fromStrict)
import Wikidata -- to ensure the Wikidata.hs file compiles

main :: IO ()
main =
  do
    _ <- forkIO startServer
    ropts <- interpretArgsOrExit []
    defaultMainWithOpts tests ropts
  where
    tests =
      Database.HSparql.ConnectionTest.testSuite
        ++ Database.HSparql.QueryGeneratorTest.testSuite

getQuerystringQuery :: Request -> Maybe ByteString
getQuerystringQuery request = join $ lookup "query" $ queryString request

getBodyQuery :: Request -> IO ByteString
getBodyQuery request = do
  lazyBody <- strictRequestBody request
  return $ toStrict lazyBody

hasTooManyContentLengthHeaders :: Request -> Bool
hasTooManyContentLengthHeaders request = length (filter (\h -> fst h == hContentLength) (requestHeaders request) ) > 1

responseForQuery ::ByteString -> Response
responseForQuery query = case query of
    "PREFIX dbprop: <http://dbpedia.org/resource/> PREFIX dbpedia: <http://dbpedia.org/property/> PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?x1 WHERE { ?x0 dbpedia:genre dbprop:Web_browser . ?x0 foaf:name ?x1 . }" ->
      selectResponse
    "PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX dct: <http://purl.org/dc/elements/1.1/> SELECT ?x1 ?x2 WHERE { ?x0 foaf:name \"Bob\" . << ?x0 foaf:age ?x1 >> dct:source ?x2 . }" ->
      selectReifiedTripleResponse
    "PREFIX dbpedia: <http://dbpedia.org/resource/> PREFIX dbprop: <http://dbpedia.org/property/> ASK { ?x0 dbprop:genre dbpedia:Web_browser . }" ->
      askResponse
    "PREFIX dbpedia: <http://dbpedia.org/resource/> PREFIX dbprop: <http://dbpedia.org/property/> PREFIX foaf: <http://xmlns.com/foaf/0.1/> PREFIX example: <http://www.example.com/> CONSTRUCT { ?x0 example:hasName ?x1 . } WHERE { ?x0 dbprop:genre dbpedia:Web_browser . ?x0 foaf:name ?x1 . }" ->
      constructResponse
    "PREFIX dbpedia: <http://dbpedia.org/resource/> DESCRIBE dbpedia:Edinburgh WHERE {  }" ->
      describeResponse
    "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> PREFIX wd: <http://www.wikidata.org/entity/> INSERT DATA { wd:Q1568346 rdfs:label \"test case\" . }" ->
      updateResponse
    raw_req ->
      error $ "Unexpected URI: \n\n" ++ show raw_req
    where
        selectResponse = responseFile status200 [("Content-Type", "application/sparql-results+xml")] "tests/fixtures/sparql_select_response.xml" Nothing
        selectReifiedTripleResponse = responseFile status200 [("Content-Type", "application/sparql-results+xml")] "tests/fixtures/sparql_select_reified_triple_response.xml" Nothing
        askResponse = responseFile status200 [("Content-Type", "text/plain")] "tests/fixtures/sparql_ask_response.text" Nothing
        constructResponse = responseFile status200 [("Content-Type", "text/turtle")] "tests/fixtures/sparql_construct_response.ttl" Nothing
        describeResponse = responseFile status200 [("Content-Type", "text/turtle")] "tests/fixtures/sparql_describe_response.ttl" Nothing
        updateResponse = responseLBS status204 [] empty

duplicateErrorResponse :: Response
duplicateErrorResponse = responseLBS status400 [] "Duplicate Content-Length header"

app :: Application
app request respond = do
    query <- case getQuerystringQuery request of
      Just query -> pure query
      Nothing -> getBodyQuery request
    respond $ if hasTooManyContentLengthHeaders request then duplicateErrorResponse else responseForQuery query

startServer :: IO ()
startServer = run 3000 app
