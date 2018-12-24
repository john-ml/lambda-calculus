{-# LANGUAGE LambdaCase #-}
import LC
import Control.Monad.State
{-
import System.IO
import Parser
import Data.Char
import Data.Void
import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char
import Data.Function
import Data.Map
import Data.List (groupBy)

--church :: Int -> Term
--church n = Λ (Λ (iterate (Var 1 :$) (Var 0) !! n))

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine
-}
import Control.Monad.Except

test :: IO ()
test = do
  -- fun<A, B> (f: A -> B) (x: A) = f(x)
  tryInfer (Type 0 :--> Type 0 :--> Var 1 --> Var 0 :--> Var 2 :--> Var 1 :$ Var 0)
  -- fun<A, B, C> (f: B -> C) (g: A -> B) (x: A) = f(g(x))
  tryInfer (Type 0 :--> Type 0 :--> Type 0 :-->
            Var 1 --> Var 0 :-->
            Var 3 --> Var 2 :-->
            Var 4 :-->
            Var 2 :$ (Var 1 :$ Var 0))
  where
    tryInfer e =
      case runExcept (infer e) of
        Left s -> putStrLn s
        Right t -> putStrLn $ withNames t

{-
type Parser = Parsec Void String
data Command = Query Term | Binding String Term | Use [String]

userInput :: Parser Command
userInput = try directive <|> try definition <|> Query <$> term where
  directive = symbol ":" *> word >>= \case
    "use" -> Use <$> some word
    s -> fail $ "Unknown command '" ++ s ++ "'"
  definition = Binding <$> word <* lexeme (string "=") <*> term

data StateType = StateType
  { bindings :: Map String Term
  , imports :: [String]
  }
type Env = StateT StateType IO

blocks :: String -> [String]
blocks = (concat <$>) . groupBy ((<) `on` indent) . lines where
  indent = length . takeWhile isSpace

runLine :: String -> Env ()
runLine "" = return ()
runLine ('#' : _) = return ()
runLine s =
  case runParser userInput "" s of
    Left e -> lift . putStrLn $ errorBundlePretty e
    Right (Query e) -> do
      m <- bindings <$> get
      lift . putStrLn . pretty m . evaluate $ fill m e
    Right (Binding s' e) -> do
      m <- bindings <$> get
      modify (\ st -> st { bindings = insert s' (simplify 100 (fill m e)) m })
    Right (Use files) -> do
      forM_ files $ (mapM_ runLine . blocks =<<) . lift . readFile
      modify (\ st -> st { imports = imports st ++ files })

repl :: Env ()
repl = forever $ do
  loaded <- imports <$> get
  s <- lift . prompt $ unwords loaded ++ "> "
  runLine s
-}

main :: IO ()
main = void $ test --runStateT repl (StateType empty [])
