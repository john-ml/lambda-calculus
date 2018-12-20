import LC
import Parser
import Data.Char
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Function

church n = Λ (Λ (iterate (Lit 1 :$) (Lit 0) !! n))

main = do
  print . evaluate $ church 2 :$ church 3
  let ω = Λ (Lit 0 :$ Lit 0)
  let s = Λ (Λ (Λ (Lit 2 :$ Lit 0 :$ (Lit 1 :$ Lit 0))))
  let c = Λ (Λ (Λ (Lit 2 :$ Lit 0 :$ Lit 1)))
  print s
  print c
  print . evaluate $ ω :$ ω
  let ops = [ Operator [Hole 10, Exact "+", Hole 9] 10
            , Operator [Hole 20, Exact "*", Hole 19] 20
            , Operator [Hole 20, Exact "/", Hole 19] 20
            , Operator [Hole 30, Exact "^", Hole 29] 30
            , Operator [Exact "if", Hole 5, Exact "then", Hole 5, Exact "else", Hole 5] 5
            , Operator [Hole 0, Exact "=", Hole 0] 0
            ]
  let pred c = not (isSpace c) && not (c `elem` "()")
  let p = lexeme . (flip Mixfix [] <$>) . some $ satisfy pred
  let parse = parseTest (mixfix ops p)
  parse "a * b + c ^ d * e"
  parse "a * (b + c)"
  parse "a * (b + c + d)"
  parse "a + b ^ b' ^ b'' + c * e * f"
  parse "(if p then a + b else a * b) / e"
  parse "a + (if p then 3 else 4)"
  parse "a * b = c"
  parse "(a + b) * (c + d) = a * c + a * d + b * c + b * d"
  parseTest term "(λ λ 1 (1 (1 0))) 2 0"
