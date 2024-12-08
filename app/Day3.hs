module Day3 (part1, part2) where

import Control.Monad (ap, liftM)
import Data.Char (isDigit)
import qualified Data.List as List

-- multiplication instruction
newtype MulInst = MulInst (Int, Int) deriving (Show)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) = ap

  pure :: a -> Parser a
  pure a = Parser {runParser = \s -> Just (a, s)}

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f =
    Parser
      { runParser = \s -> do
          (a, s') <- runParser p s
          runParser (f a) s'
      }

  return :: a -> Parser a
  return = pure

parseLiteral :: String -> Parser ()
parseLiteral literal =
  Parser
    { runParser = \s -> do
        case List.stripPrefix literal s of
          Just s' -> Just ((), s')
          Nothing -> Nothing
    }

parseInt :: Parser Int
parseInt =
  Parser
    { runParser = \s -> case span isDigit s of
        ([], _) -> Nothing
        (ds, s') -> Just (read ds :: Int, s')
    }

parseMulInst :: Parser MulInst
parseMulInst = do
  parseLiteral "mul("
  n1 <- parseInt
  parseLiteral ","
  n2 <- parseInt
  parseLiteral ")"
  return (MulInst (n1, n2))

-- parse out as many MulInsts as possible, ignoring invalid chars
collectMulInsts :: [MulInst] -> String -> [MulInst]
collectMulInsts acc s =
  case runParser parseMulInst s of
    Just (mulInst, s') -> collectMulInsts (mulInst : acc) s'
    Nothing -> case s of
      _ : s' -> collectMulInsts acc s'
      [] -> acc

runMulInst :: MulInst -> Int
runMulInst (MulInst (n1, n2)) = n1 * n2

part1 :: String -> Int
part1 contents =
  sum $ map runMulInst mulInsts
  where
    mulInsts = collectMulInsts [] contents

part2 :: String -> Int
part2 contents =
  undefined
