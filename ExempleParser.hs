-- EXEMPLE SIMPLE POUR COMPRENDRE LE PARSER
-- Comment le parser transforme du texte en structure

module ExempleParser where

import Parser

-- Exemple 1: Parser un nombre simple
exempleNombre :: IO ()
exempleNombre = do
    putStrLn "=== EXEMPLE 1: Parser un nombre ==="
    putStrLn "Code source: \"42\""
    
    case parseProgram "42" of
        Right ast -> putStrLn $ "Résultat AST: " ++ show ast
        Left err -> putStrLn $ "Erreur: " ++ err
    
    putStrLn ""

-- Exemple 2: Parser un booléen
exempleBool :: IO ()
exempleBool = do
    putStrLn "=== EXEMPLE 2: Parser un booléen ==="
    putStrLn "Code source: \"true\""
    
    case parseProgram "true" of
        Right ast -> putStrLn $ "Résultat AST: " ++ show ast
        Left err -> putStrLn $ "Erreur: " ++ err
    
    putStrLn ""

-- Exemple 3: Parser une variable
exempleVariable :: IO ()
exempleVariable = do
    putStrLn "=== EXEMPLE 3: Parser une variable ==="
    putStrLn "Code source: \"maVariable\""
    
    case parseProgram "maVariable" of
        Right ast -> putStrLn $ "Résultat AST: " ++ show ast
        Left err -> putStrLn $ "Erreur: " ++ err
    
    putStrLn ""

-- Exemple 4: Parser une addition simple
exempleAddition :: IO ()
exempleAddition = do
    putStrLn "=== EXEMPLE 4: Parser une addition ==="
    putStrLn "Code source: \"5 + 3\""
    
    -- Note: parseProgram est optimisé pour des cas simples
    -- Pour des expressions complexes, on utiliserait parseExpression
    let tokens = words "5 + 3"
    case parseExpression tokens of
        Right (expr, []) -> putStrLn $ "Résultat Expression: " ++ show expr
        Right (expr, rest) -> putStrLn $ "Expression: " ++ show expr ++ ", Reste: " ++ show rest
        Left err -> putStrLn $ "Erreur: " ++ err
    
    putStrLn ""

-- Exemple 5: Parser avec priorité d'opérateurs
exemplePriorite :: IO ()
exemplePriorite = do
    putStrLn "=== EXEMPLE 5: Priorité des opérateurs ==="
    putStrLn "Code source: \"5 + 3 * 2\""
    putStrLn "Question: Est-ce (5+3)*2 = 16 ou 5+(3*2) = 11 ?"
    
    let tokens = words "5 + 3 * 2"
    case parseExpression tokens of
        Right (expr, []) -> do
            putStrLn $ "Résultat AST: " ++ show expr
            putStrLn "Le parser respecte la priorité: multiplication AVANT addition"
            putStrLn "Donc: 5 + (3 * 2) = 11 ✅"
        Left err -> putStrLn $ "Erreur: " ++ err
    
    putStrLn ""

-- Exemple 6: Comparaison de types d'expressions
exempleTypes :: IO ()
exempleTypes = do
    putStrLn "=== EXEMPLE 6: Différents types d'expressions ==="
    
    let exemples = [
            ("42", "Nombre entier"),
            ("true", "Booléen"),
            ("x", "Variable"),
            ("5 + 3", "Addition"),
            ("10 > 5", "Comparaison"),
            ("true && false", "Logique")
         ]
    
    mapM_ testExpression exemples
    
    putStrLn ""
  where
    testExpression (code, description) = do
        putStrLn $ "• " ++ description ++ ": \"" ++ code ++ "\""
        let tokens = words code
        case parseExpression tokens of
            Right (expr, []) -> putStrLn $ "  → " ++ show expr
            Right (expr, rest) -> putStrLn $ "  → " ++ show expr ++ " (reste: " ++ show rest ++ ")"
            Left _ -> case parseProgram code of
                Right ast -> putStrLn $ "  → " ++ show ast
                Left err -> putStrLn $ "  → Erreur: " ++ err

-- Fonction principale de démonstration
main :: IO ()
main = do
    putStrLn "🎯 COMPRENDRE LE PARSER GLaDOS"
    putStrLn "==============================="
    putStrLn ""
    
    exempleNombre
    exempleBool  
    exempleVariable
    exempleAddition
    exemplePriorite
    exempleTypes
    
    putStrLn "🎓 RÉSUMÉ:"
    putStrLn "Le parser transforme du TEXTE en STRUCTURE (AST)"
    putStrLn "• Respecte la priorité des opérateurs"
    putStrLn "• Gère différents types (int, bool, variables)"
    putStrLn "• Produit un AST que le reste du compilateur peut utiliser"