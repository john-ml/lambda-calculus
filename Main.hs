import LC
import Parser
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Function
import System.IO
import Data.Map

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

repl :: IO ()
repl = do
  s <- prompt "> "
  case runParser term "" s of
    Left e -> putStrLn . errorBundlePretty $ e
    Right e -> print . evaluate $ e
  repl

main = repl
