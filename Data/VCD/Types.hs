{-# LANGUAGE CPP #-}

#define INSTANCES_USUAL Eq, Ord, Enum, Show

module Data.VCD.Types where

import Prelude

import Data.ByteString.Char8 (ByteString)
import Data.Word

-- * Declaration

data VarType
    = Event | Integer | Parameter | Real | Reg
    | Supply0 | Supply1 | Time
    | Tri | TriAnd | TriOr | TriReg | Tri0 | Tri1
    | WAnd | Wire | WOr
    deriving (INSTANCES_USUAL)

type IdCode = ByteString
type Identifier = ByteString
type Reference = (Identifier, Maybe (Either (Int, Int) Int))

data ScopeType = Begin | Fork | Function | Module | Task
    deriving (INSTANCES_USUAL)

data TextType = Comment | Date | Version
    deriving (INSTANCES_USUAL)

data TimeUnit = S | MS | US | NS | PS | FS
    deriving (INSTANCES_USUAL)

data Declaration
    = DVar VarType Int IdCode Reference
    | DScope ScopeType Identifier
    | DUpScope
    | DText TextType ByteString
    | DTimeScale Int TimeUnit
    deriving (Show)

-- * Simulation

data Four = O | I | Z | X
    deriving (INSTANCES_USUAL) -- TODO: Unbox?

data Value
    = VScalar Four
    -- TODO: use Vector?
    | VVector [Four] -- ^ LSB-first
    | VReal Double
    deriving (Show)

data Change = Change IdCode Value
    deriving (Show)

type Time = Word64
type Duration = Word64

data DumpType = All | Off | On | Vars
    deriving (INSTANCES_USUAL)

data Simulation
    = SChange Change
    | STime Time
    | SDump DumpType [Change]
    | SComment ByteString
    deriving (Show)

