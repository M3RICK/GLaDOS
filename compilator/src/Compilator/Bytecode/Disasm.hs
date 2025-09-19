module Compilator.Bytecode.Disasm (disasmModule) where
import Compilator.Bytecode.Opcode

disasmModule :: Module -> String
disasmModule m = unlines (concatMap pp (modFuns m))
  where
    pp f = ("FUNC "++funName f++"/"++show (funArity f)) :
           map (("  "++) . show) (funCode f) ++ ["END"]
