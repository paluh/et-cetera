{-# LANGUAGE TypeOperators #-}

module System.EtCetera.Internal.Boomerangs where
-- common boomerangs used by et-cetera library

import           Control.Category ((.), id)
import           Data.List (foldr)
import           Data.Monoid ((<>))
import           Prelude hiding ((.), id)
import           Text.Boomerang.Combinators (manyl, opt, push, rCons, rList, rList1, rNil)
import           Text.Boomerang.Error (ErrorMsg(..), mkParserError, ParserError(..))
import           Text.Boomerang.HStack (arg, hdMap, (:-)(..))
import           Text.Boomerang.Pos (incMajor, incMinor)
import           Text.Boomerang.Prim (Boomerang(..), Parser(..))
import           Text.Boomerang.String (char, lit, satisfy, StringBoomerang, StringError)
import qualified Text.Boomerang.String
import           System.EtCetera.Internal.Prim (Prs(..), runPrs)

oneOf :: String -> StringBoomerang r (Char :- r)
oneOf l = satisfy (`elem` l)

noneOf :: String -> StringBoomerang r (Char :- r)
noneOf l = satisfy (not . (`elem` l))

word :: String -> StringBoomerang r (String :- r)
word = foldr (\x r -> rCons . char x . r) rNil

whiteSpace :: StringBoomerang r r
whiteSpace = lit " " <> lit "\t"

ignoreWhen :: (Char -> Bool) -> StringBoomerang r r
ignoreWhen p = Boomerang
  (Parser $ \tok pos ->
       case tok of
         []        -> mkParserError pos [EOI "input"]
         (c:cs)
             | p c ->
                 [Right ((id, cs),
                          if c == '\n'
                            then incMajor 1 pos
                            else incMinor 1 pos)]
             | otherwise ->
                 mkParserError pos [SysUnExpect $ show c]
  )
  (\r -> [(id, r)])

eol :: StringBoomerang r r
eol = lit "\n"

parseString :: Prs StringError String () (r :- ())
             -> String
             -> Either StringError r
parseString p =
  Text.Boomerang.String.parseString
    (Boomerang
      (runPrs p)
      (error ("Text.Boomerang.EtCetera.Prim.parseString:" <>
              "Boomerang.Prim.parseString evaluated serializer")))

-- > satisfy isUpper <?> 'an uppercase character'
(<?>) :: Prs (ParserError p) String a b ->
         (String -> String) ->
         Prs (ParserError p) String a b
(Prs (Parser prs)) <?> toMsg =
  Prs . Parser $ pf
 where
  pf tok pos =
    map (either (\(ParserError mPos errs) ->
                   Left $ ParserError mPos (Expect (toMsg tok) : errs))
                Right)
          (prs tok pos)

infix  0 <?>

