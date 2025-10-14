{-# OPTIONS_GHC -Wno-orphans #-}
module Bytecode.BcTypes () where

import Data.Binary (Binary, get, put, getWord8, putWord8)
-- Binary: The typeclass we'll implement (like Show or Eq)
-- get: Function to deserialize (read bytes -> data)
-- put: Function to serialize (write data -> bytes)
-- getWord8: Read a single byte (0-255)
-- putWord8: Write a single byte (0-255)
import IR.Types

instance Binary Instruction where
    -- Serialization: Instruction -> Bytes
    put instr = case instr of
        -- Stack operations (0-9)
        PushInt n -> putWord8 0 >> put n
        PushBool b -> putWord8 1 >> put b
        Pop -> putWord8 2
        PushFloat f -> putWord8 3 >> put f
        -- Variable operations (10-19)
        GetLocal n -> putWord8 10 >> put n
        SetLocal n -> putWord8 11 >> put n
        -- Arithmetic operations (20-29)
        AddInt -> putWord8 20
        SubInt -> putWord8 21
        MulInt -> putWord8 22
        DivInt -> putWord8 23
        AddFloat -> putWord8 24
        SubFloat -> putWord8 25
        MulFloat -> putWord8 26
        DivFloat -> putWord8 27
        -- Comparison operations (30-39)
        EqInt -> putWord8 30
        NeqInt -> putWord8 31
        LtInt -> putWord8 32
        GtInt -> putWord8 33
        LeInt -> putWord8 34
        GeInt -> putWord8 35
        -- Logical operations (40-49)
        AndBool -> putWord8 40
        OrBool -> putWord8 41
        -- Control flow (50-59)
        Jump n -> putWord8 50 >> put n
        JumpIfFalse n -> putWord8 51 >> put n
        Call n -> putWord8 52 >> put n
        Return -> putWord8 53
        -- Utility (60-69)
        Halt -> putWord8 60
        -- Float Comparison operations (70-75)
        EqFloat -> putWord8 70
        NeqFloat -> putWord8 71
        LtFloat -> putWord8 72
        GtFloat -> putWord8 73
        LeFloat -> putWord8 74
        GeFloat -> putWord8 75
        -- Unary operations (80-82)
        NegInt -> putWord8 80
        NegFloat -> putWord8 81
        NotBool -> putWord8 82

    get = do
        tag <- getWord8
        case tag of
            -- Stack operations
            0 -> PushInt <$> get
            1 -> PushBool <$> get
            2 -> return Pop
            3 -> PushFloat <$> get
            -- Variable operations
            10 -> GetLocal <$> get
            11 -> SetLocal <$> get
            -- Arithmetic operations
            20 -> return AddInt
            21 -> return SubInt
            22 -> return MulInt
            23 -> return DivInt
            24 -> return AddFloat
            25 -> return SubFloat
            26 -> return MulFloat
            27 -> return DivFloat
            -- Comparison operations
            30 -> return EqInt
            31 -> return NeqInt
            32 -> return LtInt
            33 -> return GtInt
            34 -> return LeInt
            35 -> return GeInt
            -- Logical operations
            40 -> return AndBool
            41 -> return OrBool
            -- Control flow
            50 -> Jump <$> get
            51 -> JumpIfFalse <$> get
            52 -> Call <$> get
            53 -> return Return
            -- Utility
            60 -> return Halt
            -- Float Comparison operations
            70 -> return EqFloat
            71 -> return NeqFloat
            72 -> return LtFloat
            73 -> return GtFloat
            74 -> return LeFloat
            75 -> return GeFloat
            -- Unary operations
            80 -> return NegInt
            81 -> return NegFloat
            82 -> return NotBool
            -- Unknown tag
            _ -> fail $ "Invalid instruction tag: " ++ show tag

instance Binary CompiledFunction where
    put (CompiledFunction name params locals instructions) = do
        put name
        put params
        put locals
        put instructions

    get = do
        name <- get
        params <- get
        locals <- get
        instructions <- get
        return $ CompiledFunction name params locals instructions

instance Binary IRProgram where
    put (IRProgram funcs main) = do
        putWord8 0x48 -- 'H'
        putWord8 0x45 -- 'E'
        putWord8 0x4c -- 'L'
        putWord8 0x4c -- 'L'
        put main
        put funcs

    get = do
        m1 <- getWord8
        m2 <- getWord8
        m3 <- getWord8
        m4 <- getWord8
        if [m1, m2, m3, m4] /= [0x48, 0x45, 0x4c, 0x4c]
            then fail $ "Invalid magic number: expected 'HELL', got " ++ show [m1, m2, m3, m4]
            else return ()

        main <- get
        funcs <- get
        return $ IRProgram funcs main

instance Binary Value where
    put val = case val of
        VInt n  -> do
            putWord8 0
            put n
        VBool b -> do
            putWord8 1
            put b
        VFloat f -> do
            putWord8 2
            put f

    get = do
        tag <- getWord8
        case tag of
            0 -> VInt <$> get
            1 -> VBool <$> get
            2 -> VFloat <$> get
            _ -> fail $ "Invalid value tag: " ++ show tag
