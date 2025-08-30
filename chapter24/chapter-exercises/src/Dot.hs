{-# LANGUAGE OverloadedStrings #-}
module Dot where

import qualified Data.Map as M
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative
import Data.List (nub)

data GraphType = Directed | Undirected deriving (Show, Eq)
type Attributes = M.Map T.Text T.Text
type GraphName = T.Text

type NodeId = T.Text
type EdgeId = (NodeId, NodeId)

type Nodes = M.Map NodeId Attributes
type Edges = M.Map EdgeId Attributes

data Graph = Graph !GraphType !GraphName !Nodes !Edges deriving (Show, Eq)

skipWhitespace :: Parser ()
skipWhitespace = skipMany (char ' ' <|> char '\n')

idChars :: String
idChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "_"

isIdChars :: Char -> Bool
isIdChars = inClass idChars

parseId :: Parser NodeId
parseId = T.pack <$> many1 (satisfy isIdChars)

parseConnector :: Parser T.Text
parseConnector = string " -- " <|> string " -> "

parseEdges :: Parser [NodeId]
parseEdges = do
  _ <- skipWhitespace
  nodes <- parseId `sepBy1` parseConnector
  _ <- skipMany (char ';')
  _ <- skipWhitespace
  return $ nodes

createEdges :: [NodeId] -> [EdgeId]
createEdges [] = []
createEdges [_] = []
createEdges [x,x'] = [(x,x')]
createEdges (x:x':xs) = (x,x') : createEdges (x':xs)

parseGraphType :: Parser GraphType
parseGraphType = Undirected <$ string "graph" <|> Directed <$ string "digraph"

createGraph :: GraphType -> GraphName -> [[NodeId]] -> Graph
createGraph graphType graphName nodeLists = Graph graphType graphName nodes edges where
  nodes = M.fromList . flip zip (repeat M.empty) . nub . concat $ nodeLists
  edges = M.fromList . flip zip (repeat M.empty) . nub . concatMap createEdges $ nodeLists

parseGraph :: Parser Graph
parseGraph = do
  _ <- optional skipWhitespace
  graphType <- parseGraphType
  _ <- char ' '
  graphName <- parseId
  _ <- optional skipWhitespace
  _ <- char '{'
  nodeLists <- many1 parseEdges
  _ <- optional skipWhitespace
  _ <- char '}'
  _ <- optional skipWhitespace
  _ <- endOfInput
  return $ createGraph graphType graphName nodeLists
