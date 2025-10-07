-- EXPLICATION SIMPLE : POURQUOI C'EST PAS COMPLIQUÉ
-- Chaque module fait une seule chose simple

module ExplicationSimple where

-- 🔍 PARSER : "Je lis du texte et je fais un arbre"
-- Exemple : "5 + 3" → Arbre(+, 5, 3)
exempleParser :: String
exempleParser = "Parser = Lire du texte → Faire un arbre"

-- 🔍 TYPE CHECKER : "Je vérifie que les types collent"
-- Exemple : 5 + true → ERREUR! (int + bool impossible)
exempleTypeChecker :: String  
exempleTypeChecker = "TypeChecker = Vérifier que int+int, bool&&bool, etc."

-- ⚙️ VIRTUAL MACHINE : "Je suis une calculatrice avec une pile"
-- Exemple : Push 5, Push 3, Add → Résultat: 8
exempleVM :: String
exempleVM = "VM = Calculatrice avec pile: Push 5, Push 3, Add = 8"

-- 🔧 COMPILER : "Je traduis l'arbre en instructions pour la VM"  
-- Exemple : Arbre(+, 5, 3) → [Push 5, Push 3, Add]
exempleCompiler :: String
exempleCompiler = "Compiler = Traduire arbre → instructions VM"

-- 🎯 AU FINAL : C'est juste 4 étapes simples !
pipelineSimple :: [String]
pipelineSimple = [
    "1. Parser: Texte → Arbre",
    "2. TypeChecker: Arbre → Vérifie types", 
    "3. Compiler: Arbre → Instructions",
    "4. VM: Instructions → Résultat"
    ]

main :: IO ()
main = do
    putStrLn "🎯 POURQUOI C'EST PAS COMPLIQUÉ"
    putStrLn "================================"
    putStrLn ""
    
    putStrLn "📚 CHAQUE MODULE = UNE TÂCHE SIMPLE:"
    putStrLn $ "• " ++ exempleParser
    putStrLn $ "• " ++ exempleTypeChecker  
    putStrLn $ "• " ++ exempleVM
    putStrLn $ "• " ++ exempleCompiler
    putStrLn ""
    
    putStrLn "🔄 PIPELINE COMPLET:"
    mapM_ putStrLn $ map ("  " ++) pipelineSimple
    putStrLn ""
    
    putStrLn "🎯 EXEMPLE CONCRET:"
    putStrLn "  Code: \"5 + 3\""
    putStrLn "  1. Parser     → EArith (EInt 5) Add (EInt 3)"
    putStrLn "  2. TypeChecker → ✅ int + int = int"
    putStrLn "  3. Compiler   → [Push 5, Push 3, Add, Halt]"
    putStrLn "  4. VM         → 8"
    putStrLn ""
    
    putStrLn "💡 C'EST COMME CONSTRUIRE UNE MAISON:"
    putStrLn "  • Parser = Lire le plan"
    putStrLn "  • TypeChecker = Vérifier que c'est solide"  
    putStrLn "  • Compiler = Traduire en instructions de construction"
    putStrLn "  • VM = Construire la maison"
    putStrLn ""
    
    putStrLn "🏆 RÉSULTAT: Un compilateur qui marche avec 86 tests ✅"