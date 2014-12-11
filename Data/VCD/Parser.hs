{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Section numbers refer to
-- <http://www.eda.org/verilog-ams/htmlpages/tc-docs/lrm/1364/1364.A.pdf this IEEE 1364-2000 draft>.
-- Check the <src/Data-VCD-Parser.html source> to see exactly what is
-- accepted by these parsers.

module Data.VCD.Parser where

import Prelude

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

import Data.VCD.Types

-- | §18.2.1 /identifier_code/
idCode :: Parser IdCode
idCode = AP.takeWhile1 (\ c -> '!' <= c && c <= '~')

-- | Verilog identifier
identifier :: Parser Identifier
identifier = AP.char '\\' *> idCode
    <|> BS.cons <$> AP.satisfy (AP.inClass "A-Z_a-z")
        <*> AP.takeWhile (AP.inClass "$0-9A-Z_a-z")

-- | §18.2.1 /value/
four :: Parser Four
four = O <$ AP.char '0'
    <|> I <$ AP.char '1'
    <|> Z <$ AP.satisfy (\ c -> c == 'Z' || c == 'z')
    <|> X <$ AP.satisfy (\ c -> c == 'X' || c == 'x')

-- | §18.2.1 refactored /scalar_value_change/ and /vector_value_change/
--
-- Quote from §18.2.2:
--
-- * Dumps of value changes to scalar variables shall not have any white
--   space between the value and the identifier code.
-- * Dumps of value changes to vectors shall not have any white space
--   between the base letter and the value digits, but they shall have one
--   white space between the value digits and the identifier code.
value :: Parser Value
value = VScalar <$> four <|> (vec <* AP.space) where
    vec = VVector <$ AP.satisfy (\ c -> c == 'b' || c == 'B') <*> ynam1'
        <|> VReal <$ AP.satisfy (\ c -> c == 'r' || c == 'R') <*> AP.double
    ynam1' = do !a <- four; ynam [a] where -- many1' reversed
        ynam acc = do {!a <- four; ynam (a : acc)} `mplus` return acc

-- | §18.2.2 /value_change/
change :: Parser Change
change = flip Change <$> value <*> idCode where

-- | §18.2.3.4 /scope_type/
scopeType :: Parser ScopeType
scopeType = Begin <$ "begin"
	<|> Fork <$ "fork"
	<|> Function <$ "function"
	<|> Module <$ "module"
	<|> Task <$ "task"

-- | §18.2.3.5 /time_unit/
timeUnit :: Parser TimeUnit
timeUnit = S <$ AP.char 's'
	<|> MS <$ "ms"
	<|> US <$ "us"
	<|> NS <$ "ns"
	<|> PS <$ "ps"
	<|> FS <$ "fs"

-- | §18.2.3.8 /var_type/
varType :: Parser VarType
varType = Event <$ "event"
	<|> Integer <$ "integer"
	<|> Parameter <$ "parameter"
	<|> Real <$ "real"
	<|> Reg <$ "reg"

	<|> Supply0 <$ "supply0"
	<|> Supply1 <$ "supply1"
	<|> Time <$ "time"

	<|> Tri <$ "tri"
	<|> TriAnd <$ "triand"
	<|> TriOr <$ "trior"
	<|> TriReg <$ "trireg"
	<|> Tri0 <$ "tri0"
	<|> Tri1 <$ "tri1"

	<|> WAnd <$ "wand"
	<|> Wire <$ "wire"
	<|> WOr <$ "wor"

-- | §18.2.3.8 /reference/
reference :: Parser Reference
reference = (,) <$> identifier <*> AP.option Nothing
    ( Just <$ AP.skipSpace <* AP.char '[' <* AP.skipSpace
        <*> AP.eitherP indices AP.decimal <* AP.skipSpace <* AP.char ']' ) where
    indices :: Parser (Int, Int)
    indices = (,) <$> AP.decimal <* AP.skipSpace
        <* AP.char ':' <* AP.skipSpace <*> AP.decimal

-- | §18.2.1 common to /declaration_command/ and parts of /simulation_command/
block :: ByteString -> Parser a -> Parser a
block keyword inside = AP.string keyword *> AP.skipSpace *>
    inside <* AP.skipSpace <* "$end"

-- | §18.2.1 @$comment@, @$date@ and @$version@
--
-- Preserves in-text newlines but trims leading and trailing spaces.
blockText :: ByteString -> Parser ByteString
blockText keyword = BS.pack <$ AP.string keyword <* AP.skipSpace
    <*> AP.manyTill AP.anyChar (AP.skipSpace <* "$end")

-- | §18.2.1 /declaration_command/
declaration :: Parser Declaration
declaration = block "$var" ( DVar <$> varType
        <* AP.space <* AP.skipSpace <*> AP.decimal
        <* AP.space <* AP.skipSpace <*> idCode
        <* AP.space <* AP.skipSpace <*> reference )
    <|> block "$scope" ( DScope <$> scopeType
        <* AP.space <* AP.skipSpace <*> identifier )
    <|> block "$upscope" (pure DUpScope)
    <|> block "$timescale" ( DTimeScale <$> AP.decimal
        <* AP.skipSpace <*> timeUnit )
    <|> DText Comment <$> blockText "$comment"
    <|> DText Date <$> blockText "$date"
    <|> DText Version <$> blockText "$version"

-- | §18.2.3.3 @$enddefinitions $end@
endDefinitions :: Parser ()
endDefinitions = block "$enddefinitions" (pure ())

-- | §18.2.1 many /declaration_command/s terminated by @$enddefinitions $end@
declarations :: Parser [Declaration]
declarations = AP.sepBy declaration AP.skipSpace
    <* AP.space <* AP.skipSpace <* endDefinitions

-- | §18.2.1 /simulation_command/
simulation :: Parser Simulation
simulation = SChange <$> change
    <|> STime <$ AP.char '#' <*> AP.decimal
    <|> block "$dumpall" (SDump All <$> changes)
    <|> block "$dumpoff" (SDump Off <$> changes)
    <|> block "$dumpon" (SDump On <$> changes)
    <|> block "$dumpvars" (SDump Vars <$> changes)
    <|> SComment <$> blockText "$comment" where
    changes = AP.sepBy change AP.skipSpace

-- | §18.2.1 many /simulation_command/s
simulations :: Parser [Simulation]
simulations = AP.sepBy simulation AP.skipSpace

-- | §18.2.1 /value_change_dump_definitions/
valueChangeDump :: Parser ([Declaration], [Simulation])
valueChangeDump = (,) <$> declarations <* AP.skipSpace <*> simulations

