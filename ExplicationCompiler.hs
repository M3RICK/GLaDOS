-- EXPLICATION : À QUOI SERT LE COMPILER ?
-- Le Compiler est le traducteur entre le monde humain et le monde machine

module ExplicationCompiler where

import Parser (Expression(..), ArithOp(..), BoolOp(..))
import Compiler (compileExpression)
import VirtualMachine (Instruction(..), Value(..), runVM)

-- 🎯 RÔLE DU COMPILER : TRADUCTEUR AST → BYTECODE

-- 1. Ce que voit l'humain : "5 + 3 * 2"
-- 2. Ce que voit le Parser : EArith (EInt 5) Add (EArith (EInt 3) Mul (EInt 2))
-- 3. Ce que fait le Compiler : [Push 5, Push 3, Push 2, Mul, Add, Halt]
-- 4. Ce que comprend la VM : Instructions à exécuter sur une pile

exempleCompilation :: IO ()
exempleCompilation = do
    putStrLn "🧠 RÔLE DU COMPILER : TRADUCTEUR AST → BYTECODE"
    putStrLn "=================================================="
    putStrLn ""
    
    let expr = EArith (EInt 5) Add (EArith (EInt 3) Mul (EInt 2))
    
    putStrLn "1. EXPRESSION HUMAINE:"
    putStrLn "   5 + 3 * 2"
    putStrLn ""
    
    putStrLn "2. AST (ce que voit le Parser):"
    putStrLn $ "   " ++ show expr
    putStrLn ""
    
    putStrLn "3. BYTECODE (ce que produit le Compiler):"
    let bytecode = compileExpression expr
    putStrLn $ "   " ++ show bytecode
    putStrLn ""
    
    putStrLn "4. RÉSULTAT (ce que calcule la VM):"
    case runVM (bytecode ++ [Halt]) of
        Right result -> putStrLn $ "   " ++ show result
        Left err -> putStrLn $ "   Erreur: " ++ show err

-- 🔄 POURQUOI SÉPARER COMPILER ET VM ?

avantagesArchitecture :: IO ()
avantagesArchitecture = do
    putStrLn "\n🔄 POURQUOI SÉPARER COMPILER ET VM ?"
    putStrLn "=================================="
    putStrLn ""
    
    putStrLn "✅ AVANTAGES DE CETTE ARCHITECTURE:"
    putStrLn "1. RÉUTILISABILITÉ:"
    putStrLn "   • Le Compiler peut cibler différentes VMs"
    putStrLn "   • La VM peut exécuter du bytecode d'autres langages"
    putStrLn ""
    
    putStrLn "2. OPTIMISATION:"
    putStrLn "   • Le Compiler peut optimiser le bytecode"
    putStrLn "   • La VM peut optimiser l'exécution"
    putStrLn ""
    
    putStrLn "3. SÉPARATION DES RESPONSABILITÉS:"
    putStrLn "   • Compiler = Génération de code"
    putStrLn "   • VM = Exécution de code"
    putStrLn ""
    
    putStrLn "4. DÉBOGAGE:"
    putStrLn "   • On peut inspecter le bytecode généré"
    putStrLn "   • On peut tracer l'exécution dans la VM"

-- 🎯 EXEMPLES CONCRETS DE COMPILATION

exemplesConcrets :: IO ()
exemplesConcrets = do
    putStrLn "\n🎯 EXEMPLES DE COMPILATION"
    putStrLn "=========================="
    putStrLn ""
    
    let exemples = [
            ("Nombre", EInt 42, "Simple: empiler la valeur"),
            ("Addition", EArith (EInt 5) Add (EInt 3), "Empiler 5, empiler 3, additionner"),
            ("Expression complexe", EArith (EArith (EInt 2) Mul (EInt 3)) Add (EInt 4), "Multiplication puis addition"),
            ("Comparaison", EBoolOp (EInt 10) Gt (EInt 5), "Empiler 10, empiler 5, comparer")
         ]
    
    mapM_ montrerCompilation exemples
  where
    montrerCompilation (nom, expr, explication) = do
        putStrLn $ "• " ++ nom ++ ":"
        putStrLn $ "  Expression: " ++ show expr
        putStrLn $ "  Explication: " ++ explication
        putStrLn $ "  Bytecode: " ++ show (compileExpression expr)
        case runVM (compileExpression expr ++ [Halt]) of
            Right result -> putStrLn $ "  Résultat: " ++ show result
            Left err -> putStrLn $ "  Erreur: " ++ show err
        putStrLn ""

-- 🏗️ COMPARAISON AVEC D'AUTRES APPROCHES

comparaisonApproches :: IO ()
comparaisonApproches = do
    putStrLn "🏗️ NOTRE APPROCHE vs AUTRES APPROCHES"
    putStrLn "====================================="
    putStrLn ""
    
    putStrLn "📊 INTERPRÉTEUR DIRECT (plus simple):"
    putStrLn "   Parser → Interpréteur → Résultat"
    putStrLn "   ❌ Plus lent (re-parse à chaque fois)"
    putStrLn "   ❌ Difficile à optimiser"
    putStrLn ""
    
    putStrLn "🚀 NOTRE APPROCHE (compilateur):"
    putStrLn "   Parser → Compiler → Bytecode → VM → Résultat"
    putStrLn "   ✅ Plus rapide (bytecode pré-compilé)"
    putStrLn "   ✅ Optimisations possibles"
    putStrLn "   ✅ Bytecode réutilisable"
    putStrLn "   ✅ Architecture professionnelle"
    putStrLn ""
    
    putStrLn "⚡ COMPILATEUR NATIF (plus complexe):"
    putStrLn "   Parser → Compiler → Code machine → CPU"
    putStrLn "   ✅ Plus rapide encore"
    putStrLn "   ❌ Très complexe à implémenter"
    putStrLn "   ❌ Dépendant de l'architecture CPU"

main :: IO ()
main = do
    exempleCompilation
    avantagesArchitecture 
    exemplesConcrets
    comparaisonApproches
    
    putStrLn "\n🎓 RÉSUMÉ:"
    putStrLn "Le Compiler traduit notre AST en bytecode optimisé"
    putStrLn "La VM exécute ce bytecode efficacement"
    putStrLn "= Architecture de compilateur professionnel ! 🚀"