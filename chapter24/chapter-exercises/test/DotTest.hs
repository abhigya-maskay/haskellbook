{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module DotTest where

import qualified Data.Map as M
import qualified Data.Text as T
import Dot
import Test.Hspec
import TestUtil (parseResultShouldBe)
import Text.RawString.QQ

example1 :: T.Text
example1 =
  [r|
digraph G {
 main -> parse -> execute;
 main -> init;
 main -> cleanup;
 execute -> make_string;
 }
|]

example1Graph :: Graph
example1Graph = graph where
  nodes = M.fromList [
    ("main", M.empty),
    ("parse", M.empty),
    ("execute", M.empty),
    ("init", M.empty),
    ("cleanup", M.empty),
    ("execute", M.empty),
    ("make_string", M.empty)
    ]
  edges = M.fromList [
    (("main", "parse"), M.empty),
    (("parse", "execute"), M.empty),
    (("main", "init"), M.empty),
    (("main", "cleanup"), M.empty),
    (("execute", "make_string"), M.empty)
    ]
  graph = Graph Directed "G" nodes edges

testDot :: IO ()
testDot = hspec $ do
  describe "parseEdges" $ do
    it "Should properly parse single edges" $ do
      parseResultShouldBe parseEdges "main -> init;" ["main", "init"]
    it "Should properly parse multiple edges" $ do
      parseResultShouldBe parseEdges "main -> init -> parse;" ["main", "init", "parse"]
  describe "createEdges" $ do
    it "Should handle single edges" $ do
      createEdges ["main", "init"] `shouldBe` [("main", "init")]
    it "Should handle multiple edges" $ do
      createEdges ["main", "init", "parse"] `shouldBe` [("main", "init"), ("init", "parse")]
  describe "createGraph" $ do
    it
      "Should handle creating Simple Graphs"
      $ do
        let nodeList = [["main", "init", "parse"]]
            nodes =
              M.fromList
                [ ("main", M.empty),
                  ("init", M.empty),
                  ("parse", M.empty)
                ]
            edges =
              M.fromList
                [ (("main", "init"), M.empty),
                  (("init", "parse"), M.empty)
                ]
        createGraph Directed "G" nodeList `shouldBe` Graph Directed "G" nodes edges
    describe "parseGraph" $ do
      it "should handle simple dot graphs" $ do
        parseResultShouldBe parseGraph example1 example1Graph
