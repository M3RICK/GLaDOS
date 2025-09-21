module Evaluator.Helpers (isTrue, liftError) where
import Types

-- A voir si on ajoute plus tard d autres cas vrai ou faux
isTrue :: LispVal -> Bool
isTrue (Bool False) = False
isTrue _            = True

liftError :: Either String a -> Either String a
liftError = id  -- pour l instant ca sert a rien mais, ca va evoluer
