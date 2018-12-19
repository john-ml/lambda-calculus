import LC
import Parser
import Data.Char
import Data.Void
import Text.Megaparsec hiding (empty)
import Text.Megaparsec.Char
import Data.Function
import System.IO
import Data.Map
import Control.Monad.State

church n = Λ (Λ (iterate (Lit 1 :$) (Lit 0) !! n))

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

test = do
  print . evaluate $ church 2 :$ church 3
  let ω = Λ (Lit 0 :$ Lit 0)
  let s = Λ (Λ (Λ (Lit 2 :$ Lit 0 :$ (Lit 1 :$ Lit 0))))
  let c = Λ (Λ (Λ (Lit 2 :$ Lit 0 :$ Lit 1)))
  print s
  print c
  print . evaluate $ ω :$ ω
  let m = fromList [("abc", Lit 0)]
  print . fill m $ Λ (Hole "abc")
  parseTest term "(λ λ 1 (1 (1 0))) 2 0"

type Parser = Parsec Void String
data Command
  = Query Term
  | Binding String Term
  | Use [String]

userInput :: Parser Command
userInput = try directive <|> try definition <|> Query <$> term where
  directive = do
    symbol ":"
    s <- word
    case s of
      "use" -> Use <$> some word
      _ -> fail $ "Unknown command '" ++ s ++ "'"
  definition = Binding <$> word <* lexeme (string "=") <*> term

-- TODO keep track of which files have been loaded
type Env = StateT (Map String Term) IO

runLine :: String -> Env ()
runLine "" = return ()
runLine s =
  case runParser userInput "" s of
    Left e -> lift . putStrLn $ errorBundlePretty e
    Right (Query e) -> get >>= lift . print . evaluate . flip fill e
    Right (Binding s e) -> do
      m <- get
      modify $ insert s (fill m e)
    Right (Use files) -> forM_ files $ \ f -> do
      lift (readFile f) >>= mapM_ runLine . lines

repl :: Env ()
repl = do
  s <- lift $ prompt "> "
  runLine s
  repl

main = runStateT repl empty
