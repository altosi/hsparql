{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Database.HSparql.ConnectionTest ( testSuite ) where

import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.HUnit

import qualified Data.Map as Map
import qualified Data.RDF as RDF

import Database.HSparql.Connection
import Database.HSparql.QueryGenerator
import Data.Text

testSuite :: [Test.Framework.Test]
testSuite = [
    testGroup "Database.HSparql.Connection tests" [
        testCase "selectQuery" test_selectQuery
      , testCase "selectReifiedTripleQuery" test_selectReifiedTripleQuery
      , testCase "askQuery" test_askQuery
      , testCase "constructQuery" test_constructQuery
      , testCase "describeQuery" test_describeQuery
      , testCase "updateQuery" test_updateQuery
    ]
  ]

test_selectQuery :: IO ()
test_selectQuery =
  let expectedBVars = Just [ [ Bound $ RDF.lnode $ RDF.plainLL "Kazehakase" "en" ]
                           , [ Bound $ RDF.lnode $ RDF.plainLL "Netscape Browser" "en" ]
                           , [ Bound $ RDF.lnode $ RDF.plainLL "SlimBrowser" "en" ]
                           ]
  in do
    bvars <- selectQuery endPoint query
    assertEqual "bound variables" expectedBVars bvars

    where endPoint = "http://127.0.0.1:3000"
          query = do
              resource <- prefix "dbprop" (iriRef "http://dbpedia.org/resource/")
              dbpprop  <- prefix "dbpedia" (iriRef "http://dbpedia.org/property/")
              foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")

              x    <- var
              name <- var

              _ <- triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
              _ <- triple x (foaf .:. "name") name

              select [SelectVar name]

test_selectReifiedTripleQuery :: IO ()
test_selectReifiedTripleQuery =
  let expectedBVars = Just [ [ Bound $ RDF.LNode $ RDF.TypedL "42" "http://www.w3.org/2001/XMLSchema#integer"
                             , Bound $ RDF.LNode $ RDF.PlainL "Rumors"
                             ] ]
  in do
    bvars <- selectQuery endPoint query
    assertEqual "bound variables" expectedBVars bvars

    where endPoint = "http://127.0.0.1:3000"
          query = do
              foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
              dct      <- prefix "dct" (iriRef "http://purl.org/dc/elements/1.1/")

              bob <- var
              age <- var
              src <- var

              triple_ bob (foaf .:. "name") ("Bob" :: Text)
              triple_ (embeddedTriple bob (foaf .:. "age") age) (dct .:. "source") src

              selectVars [age, src]

test_askQuery :: IO ()
test_askQuery = do
  bool <- askQuery endPoint query
  assertBool "invalid" bool

  where endPoint = "http://127.0.0.1:3000"
        query = do
            resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
            dbprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")

            x <- var
            ask <- askTriple x (dbprop .:. "genre") (resource .:. "Web_browser")

            return AskQuery { queryAsk = [ask] }

test_constructQuery :: IO ()
test_constructQuery =
  let expectedGraph :: RDF.RDF RDF.TList
      expectedGraph = RDF.mkRdf expectedTriples Nothing (RDF.PrefixMappings Map.empty)
      expectedTriples = [ RDF.Triple (RDF.unode "http://dbpedia.org/resource/Kazehakase")
                                     (RDF.unode "http://www.example.com/hasName")
                                     (RDF.lnode $ RDF.plainLL "Kazehakase" "en") ]
  in do
    graph <- constructQuery endPoint query :: IO (RDF.RDF RDF.TList)
    assertBool "RDF does not include the constructed triple" $ RDF.isIsomorphic expectedGraph graph

    where endPoint = "http://127.0.0.1:3000"
          query = do
              resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
              dbpprop  <- prefix "dbprop" (iriRef "http://dbpedia.org/property/")
              foaf     <- prefix "foaf" (iriRef "http://xmlns.com/foaf/0.1/")
              example  <- prefix "example" (iriRef "http://www.example.com/")

              x    <- var
              name <- var

              construct <- constructTriple x (example .:. "hasName") name
              _ <- triple x (dbpprop .:. "genre") (resource .:. "Web_browser")
              _ <- triple x (foaf .:. "name") name

              return ConstructQuery { queryConstructs = [construct] }

test_describeQuery :: IO ()
test_describeQuery =
  let expectedNode = RDF.unode "http://dbpedia.org/resource/Edinburgh"
  in do
    graph <- describeQuery endPoint query :: IO (RDF.RDF RDF.TList)
    assertBool "RDF does not include the required node" $ RDF.rdfContainsNode graph expectedNode

    where endPoint = "http://127.0.0.1:3000"
          query = do
              resource <- prefix "dbpedia" (iriRef "http://dbpedia.org/resource/")
              uri <- describeIRI (resource .:. "Edinburgh")
              return DescribeQuery { queryDescribe = uri }

test_updateQuery :: IO ()
test_updateQuery =
  do
    success <- updateQuery endpoint query
    assertEqual "success" True success
  where endpoint = "http://127.0.0.1:3000"
        query = do
            rdfs <- prefix "rdfs" (iriRef "http://www.w3.org/2000/01/rdf-schema#")
            wd <- prefix "wd" (iriRef "http://www.wikidata.org/entity/")
            updateTriple_ (wd .:. "Q1568346") (rdfs .:. "label") (pack "test case")
            insertData
