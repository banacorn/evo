{-# LANGUAGE OverloadedStrings #-}

module Census.Parser where

import              Census.Types

import              Control.Monad.Trans.Resource

import              Data.Attoparsec.ByteString
import              Data.ByteString (ByteString)
import              Data.Conduit
import              Data.Conduit.Attoparsec
import              Prelude hiding (take)

trainingDataParser :: Conduit ByteString (ResourceT IO) (Maybe Labeled)
trainingDataParser = do
    conduitParserEither (choice [parseEmptyLine, parseLabeled]) =$= awaitForever go
    where   go (Left s) = error $ show s
            go (Right (_, p)) = yield p

testDataParser :: Conduit ByteString (ResourceT IO) (Maybe Unlabeled)
testDataParser = do
    conduitParserEither (choice [parseEmptyLine, parseUnlabeled]) =$= awaitForever go
    where   go (Left s) = error $ show s
            go (Right (_, p)) = yield p


parseEmptyLine :: Parser (Maybe a)
parseEmptyLine = string "\n" >> return Nothing

parseAttribute :: Parser Attribute
parseAttribute = takeTill (\x -> x == 0x2c || x == 0xa)

parseUnlabeled :: Parser (Maybe Unlabeled)
parseUnlabeled = do
    attrs <- parseAttribute `sepBy` string ", "
    if "?" `elem` attrs then 
        return Nothing 
    else 
        return (Just attrs)

parseLabeled :: Parser (Maybe Labeled)
parseLabeled = do
    attrs <- parseAttribute `sepBy` string ", "
    if "?" `elem` attrs then 
        return Nothing 
    else 
        return (Just (Labeled (init attrs) (last attrs)))
