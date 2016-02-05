{-# LANGUAGE ExistentialQuantification #-}

-- | Most uses of this library involve assembly for some Instruction Set Architecture
-- An ISA typically describes how a CPU operates given numerical input in the form of your program.

module Asthmatic
    ( Arch,
      Instruction,
      Register,
      CPUType,
      SubExp,
      Condition,
      RegisterPurpose
    )
    where

import Data.Binary

-- | CPUs typically fall into one of these categories.
-- An example of RISC would be ARM.
-- x86 for CISC
-- Something like your GPU's instruction set could be VLIW
data CPUType = RISC | CISC | VLIW

-- | Many ISAs use little computations inside the instruction,
-- for instance for memory addressing.
data SubExp = forall a. Register a => Reg a 
            | Num Int
            | Dereference SubExp
            | Add SubExp SubExp
            | Sub SubExp SubExp
            | Mul SubExp SubExp
            | Div SubExp SubExp

-- | We'll make use of our SubExp type for handling conditions.
data Condition = Eq SubExp SubExp              -- a = b
               | NotEq SubExp SubExp           -- a != b
               | GreaterThan SubExp SubExp     -- a > b
               | LessThan SubExp SubExp        -- a < b
               | True                          -- Constant true
               | False                         -- Constant false

-- | Arch types are just a simple top level type describing the architecture.
-- For instance with x86 you might have:
-- @
-- data X86 = I386 | X86_64
-- instance Arch X86 where
--     wordSize I386 = 32
--     wordSize X86_64 = 64
--     cpuType = CISC
-- @
class (Show a) => Arch a where
    -- | Word size in *bits*
    wordSize :: a -> Int
    cpuType :: a -> CPUType

-- | Instructions take different types of operands
data Operand = forall a. Register a => OpReg a  -- Register operand
             | OpIMM Int                        -- Immediate operand
             | OpMem SubExp                     -- Memory reference
             | OpPCRelative SubExp

-- | Instruction types enumerate CPU instructions. Go figure.
-- Requirements for implementing the Instruction class are somewhat draconian.
-- It requires instances for Show, Read, and Binary.
-- So, it needs to be able to parse and print the assembly in text form,
-- as well as assemble and disassemble from/to the type form.
-- We also need to know if the instruction is conditional,
-- and if it modifies program flow in any way.
class (Show a, Read a, Binary a) => Instruction a where
    -- | Sometimes ISAs have varying instruction sizes.
    -- This would be that instruction's size in bytes, for simplicity.
    instructionSize :: a -> Int
    -- | Return the condition this instruction needs met in order to execute, if any.
    conditional :: a -> Maybe Condition
    -- | Return a branch location if the aforementioned condition is true.

-- | Some registers have the benefit of having a set purpose in life.
-- A module abstracting an architecture should probably know that.
data RegisterPurpose = ProgramCounter
                     | Pointer -- stack pointer is the most obvious example
                     | General
                     | Constant
                     | Vector -- for SIMD
                     | Flag
                     | Debug

-- | Registers, they're handy.
class (Show a, Read a, Enum a) => Register a where
    -- | Size of register in bits
    registerSize :: a -> Int
    -- | Many times, registers can have parts of their contents accessed as separate registers.
    -- For instance, with x86, the lower 16-bits of EAX can be accessed with AX
    subRegisters :: a -> [a]
    -- | So those subRegisters must have a superRegister then.
    -- The super register of AX would be EAX, and technically RAX if in x86_64
    superRegisters :: a -> [a]
    -- | The register's purpose. pointer, flags register, debug, GPR...
    purpose :: a -> RegisterPurpose
