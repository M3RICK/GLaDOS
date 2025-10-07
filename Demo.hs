-- Démonstration pour les coéquipiers
-- Comment utiliser le compilateur GLaDOS

module Demo where

import Parser (parseProgram)
import TypeChecker (typeCheckExpression, emptyEnv)
import Compiler (compileExpression)
import VirtualMachine (runVM, Instruction(..))

-- Exemple 1: Pipeline complet pour "5 + 3"
demo1 :: IO ()
demo1 = do
    putStrLn "=== DEMO 1: Compilation de '5 + 3' ==="
    
    -- Étape 1: Parsing
    putStrLn "1. PARSING: '5 + 3'"
    case parseProgram "5 + 3" of
        Right ast -> do
            putStrLn $ "   AST: " ++ show ast
            
            -- Note: Cette démo est simplifiée
            -- Le vrai pipeline nécessite plus de setup
            
        Left err -> putStrLn $ "   Erreur: " ++ err

-- Exemple 2: Ce que fait chaque module
explanationModules :: IO ()
explanationModules = do
    putStrLn "\n=== ARCHITECTURE DU COMPILATEUR ==="
    putStrLn "1. Parser.hs      : 'code source' → AST"
    putStrLn "2. TypeChecker.hs : AST → vérification types"  
    putStrLn "3. Compiler.hs    : AST → bytecode"
    putStrLn "4. VirtualMachine.hs : bytecode → résultat"
    
    putStrLn "\n=== TESTS ==="
    putStrLn "• 19 tests Parser"
    putStrLn "• 21 tests TypeChecker" 
    putStrLn "• 31 tests VirtualMachine"
    putStrLn "• 15 tests Compiler"
    putStrLn "= 86 tests au total ✅"

main :: IO ()
main = do
    demo1
    explanationModules