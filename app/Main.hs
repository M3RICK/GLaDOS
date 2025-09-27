
-- Projet GLaDOS - Main executable
-- Auteur : Copilot (Epitech style)
-- Description : Point d'entrée principal, démonstration du parser

module Main (main) where

import Parser (parseProgram)


main :: IO ()
main = do
        putStrLn "Parser demo:"
        let testInputs = ["42", "true", "false", "abc"]
        mapM_ printParse testInputs
    where
        printParse s =
            case parseProgram s of
                Left err -> putStrLn $ s ++ " => Parse failed: " ++ err
                Right ast -> putStrLn $ s ++ " => Parse succeeded: " ++ show ast