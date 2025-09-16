module Environment where
import qualified Data.Map as Map
import Types

emptyEnv :: Env
emptyEnv = Map.empty

lookupVar :: String -> Env -> Either String LispVal
lookupVar variableName currentEnv = 
    case Map.lookup variableName currentEnv of
        Nothing -> Left $ "*** ERROR: variable " ++ variableName ++ " is not bound."
        Just value -> Right value