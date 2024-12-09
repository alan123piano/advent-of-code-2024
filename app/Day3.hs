module Day3 (part1, part2) where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad (ap, liftM)
import Data.Char (isDigit)
import qualified Data.List as List

data Inst = MulInst (Int, Int) | DoInst | DontInst deriving (Show)

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
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

instance Alternative Parser where
  empty :: Parser a
  empty = Parser {runParser = const Nothing}

  (<|>) :: Parser a -> Parser a -> Parser a
  p1 <|> p2 =
    Parser
      { runParser = \s -> case (runParser p1 s, runParser p2 s) of
          (Just r1, _) -> Just r1
          (_, Just r2) -> Just r2
          (Nothing, Nothing) -> Nothing
      }

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

parseMulInst :: Parser Inst
parseMulInst = do
  parseLiteral "mul("
  n1 <- parseInt
  parseLiteral ","
  n2 <- parseInt
  parseLiteral ")"
  return (MulInst (n1, n2))

parseDoInst :: Parser Inst
parseDoInst = do
  parseLiteral "do()"
  return DoInst

parseDontInst :: Parser Inst
parseDontInst = do
  parseLiteral "don't()"
  return DontInst

parseInst :: Parser Inst
parseInst = parseMulInst <|> parseDoInst <|> parseDontInst

-- parse out as many expressions as possible, skipping chars on parse failures
collectExprs :: Parser a -> String -> [a]
collectExprs p =
  reducer []
  where
    reducer acc s =
      case runParser p s of
        Just (a, s') -> reducer (acc ++ [a]) s'
        Nothing -> case s of
          _ : s' -> reducer acc s'
          [] -> acc

runInsts :: [Inst] -> Int
runInsts =
  reducer True (0 :: Int)
  where
    reducer mulEnabled sumAcc insts =
      case insts of
        (inst : insts') -> case inst of
          MulInst (n1, n2) -> reducer mulEnabled (if mulEnabled then sumAcc + n1 * n2 else sumAcc) insts'
          DoInst -> reducer True sumAcc insts'
          DontInst -> reducer False sumAcc insts'
        [] -> sumAcc

part1 :: String -> Int
part1 contents =
  runInsts insts
  where
    insts = collectExprs parseMulInst contents

part2 :: String -> Int
part2 contents =
  runInsts insts
  where
    insts = collectExprs parseInst contents
