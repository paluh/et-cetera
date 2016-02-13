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
import           Text.Boomerang.Prim (Boomerang(..), Parser(..), xmaph, xpure)
import           Text.Boomerang.String (anyChar, char, lit, satisfy, StringBoomerang, StringError)
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

-- quite exensive but correct string quotation
-- probably can be written in three lines...
-- maybe some day... ;-)

data QuotedStringPart = Escaped String | Literal String
  deriving (Eq, Show)
type QuotedString = [QuotedStringPart]

quotedString' :: StringBoomerang r (QuotedString :- r)
quotedString' =
  lit "\"" .
    ((opt (rCons . esc) . chrsEscRec) <>
     (opt (rCons . chrs) . escChrsRec) <>
     (rCons . esc . rNil) <>
     (rCons . chrs . rNil)
     ) .
  lit "\""
 where
  chrsEscRec :: StringBoomerang r (QuotedString :- r)
  chrsEscRec = rCons . chrs . rCons . esc . (chrsEscRec <> rNil)

  escChrsRec :: StringBoomerang r (QuotedString :- r)
  escChrsRec = rCons . esc . rCons . chrs . (escChrsRec <> rNil)

  chrs :: StringBoomerang r (QuotedStringPart :- r)
  chrs =
    xmaph Literal fromLiteral (rList1 (noneOf "\\\""))
   where
    fromLiteral qsp =
      case qsp of
        Literal s -> Just s
        otherwise -> Nothing

  esc :: StringBoomerang r (QuotedStringPart :- r)
  esc =
    xmaph Escaped fromEscaped (rList1 (lit "\\" . anyChar))
   where
    fromEscaped qsp =
      case qsp of
        Escaped s -> Just s
        otherwise -> Nothing

escape :: StringBoomerang (QuotedString :- r) (String :- r)
escape = xpure (arg (:-) u) q
 where
  u = concatMap u'
  u' (Escaped s) = s
  u' (Literal s) = s

  q (s :- r)  = Just (q' s :- r)
  q' []       = []
  q' ('\\':r) = qCons (Escaped "\\") (q' r)
  q' ('"':r)  = qCons (Escaped "\"") (q' r)
  q' (c:r)    = qCons (Literal [c]) (q' r)

  qCons :: QuotedStringPart -> QuotedString -> QuotedString
  qCons (Literal [c]) (Literal t : s) = Literal (c:t) : s
  qCons (Literal h) (Literal t : s) = Literal (h ++ t) : s
  qCons (Escaped [c]) (Escaped t : s) = Escaped (c:t) : s
  qCons (Escaped h) (Escaped t : s) = Escaped (h ++ t) : s
  qCons e s = e : s

quotedString = escape . quotedString'

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

