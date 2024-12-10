module FinalProject01 where

import Control.Applicative(liftA, liftA2, liftA3)
import Data.List

import CFGParsing

bottomUp :: (Eq nt, Eq t) => CFG nt t -> [t] -> [[ParseStep nt t]]
bottomUp cfg input =
  let (nts, ts, start, rules) = cfg in
  let startingConfig = ([], input) in
  let goalConfig = ([NoBar start], []) in
  parser [shift, reduce] rules startingConfig goalConfig
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.
-------------------------------------------------------------------------------

-- These functions are placeholders to work with 'bottomUp' in Part 1.3. 
-- You should replace 'undefined' in these functions with your own code.

-- Warm up
isRuleCNF :: RewriteRule nt t -> Bool
isRuleCNF rule = case rule of
  NTRule _ xs -> length xs == 2
  TRule _ _   -> True
  NoRule      -> True

isCNF :: CFG nt t -> Bool
isCNF (_, _, _, rules) = all isRuleCNF rules


-- END OF WARM UP

-- CFG Parsing
-- Now onto the actual project: constructing your CFG parsers. Each parser that you’ll implement
-- will make use of a common computational core, a “parser base” function that abstracts away from
-- the differences among bottom-up, top-down, and left-corner parsing. The parser-base function has
-- parameters for (i) the starting configuration, (ii) the goal configuration, and (iii) the transition steps.
-- So, you’ll need to write separate functions that will supply these parameters to the parser-base so
-- it can be used in bottom-up, top-down, or left-corner mode. We’ll break this down into a few steps.

-- C. Write functions
-- shift :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
-- reduce :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]

-- which execute the shift and reduce transition steps of a bottom-up parser. Each transition-step
-- function is of the same type: they take a list of rewrite rules and a configuration, and return a list
-- of possible parse steps. If no parse steps are possible, the result should be the empty list.
-- Having the transition-step functions all be the same type allows them to be easily worked with as
-- a group. For example, you can use map to apply a list of transition-step functions to the same
-- argument. (This is a useful hint for later.)
-- Note: The formatting for the functions below is provided via a “pretty-print” module that makes
-- the output of these CFG parsing functions a little more readable, like a parse table. You can ignore
-- the details of this module.

shift :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
shift rules (stack, input) = case input of
  [] -> []
  (x:xs) -> case find (\rule -> case rule of { TRule _ t -> t == x; _ -> False }) rules of
    Just (TRule nt _) -> [ParseStep Shift (TRule nt x) (NoBar nt : stack, xs)]
    _ -> []

reduce :: (Eq nt, Eq t) => [RewriteRule nt t] -> Config nt t -> [ParseStep nt t]
reduce rules (stack, input) = case stack of
  [] -> []
  (NoBar nt : stack') -> case find (\rule -> case rule of { NTRule nt' _ -> nt' == nt; _ -> False }) rules of
    Just (NTRule nt' [t1, t2]) -> [ParseStep Reduce (NTRule nt' [t1, t2]) (Bar nt' : stack', input)]
    _ -> []
    
-- Now onto the fun part. Your bottom-up parser will be split across two functions: parser and
-- bottomUp. bottomUp is the actual bottom-up parser, but it is essentially a wrapper for parser (the
-- parser-base function), supplying it with the starting configuration, the goal configuration, and the
-- transition steps (i.e. shift and reduce). I have provided bottomUp for you. Your task here is to
-- write the function

-- parser :: (Eq nt, Eq t) =>
-- [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]] ->
-- [RewriteRule nt t] -> Config nt t -> Config nt t ->
-- [[ParseStep nt t]]

-- When you put the two pieces together, bottomUp will take two arguments, a CFG and a list of
-- terminal symbols, and it will return all of the possible bottom-up parses of the given list of terminal
-- symbols under the given CFG. If the list of terminal symbols cannot be parsed by the given CFG,
-- the result should be the empty list

parser :: (Eq nt, Eq t)
       => [[RewriteRule nt t] -> Config nt t -> [ParseStep nt t]]
          -- ^ List of transition steps. ^
       -> [RewriteRule nt t]  -- Rules from the CFG.
       -> Config nt t         -- Starting configuration.
       -> Config nt t         -- Goal configuration.
       -> [[ParseStep nt t]]  -- List of possible parses.
parser transitions rules start goal = case transitions of 
  [] -> []
  (t:ts) -> case t rules start of
    [] -> parser ts rules start goal
    (x:xs) -> case x of
      ParseStep _ _ (_, []) -> [x] : parser ts rules start goal
      ParseStep _ _ newConfig -> let next = parser transitions rules newConfig goal in
        map (\y -> x : y) next ++ parser ts rules start goal
