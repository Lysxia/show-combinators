import Text.Show.Combinators

data MyType a
  = C a a                   -- a regular constructor
  | a :+: a                 -- an infix constructor
  | R { f1 :: a, f2 :: a }  -- a record
  deriving Show

infixl 4 :+:

showsMyType :: (a -> PrecShowS) -> MyType a -> PrecShowS
showsMyType showA (C a b) = showCon "C" `showApp` showA a `showApp` showA b
showsMyType showA (c :+: d) = showInfix ":+:" 4 (showA c) (showA d)
showsMyType showA (R {f1 = e, f2 = f}) =
  showRecord "R" ("f1" `showField` showA e &| "f2" `showField` showA f)

-- Just making sure this typechecks
_showsMyType' :: Show a => MyType a -> PrecShowS
_showsMyType' (C a b) = showCon "C" @| a @| b
_showsMyType' (c :+: d) = showInfix' ":+:" 4 c d
_showsMyType' (R {f1 = e, f2 = f}) =
  showRecord "R" ("f1" .=. e &| "f2" .=. f)

check :: Show a => (a -> PrecShowS) -> a -> IO ()
check show' x =
  if s == s' then
    return ()
  else
    fail $ show (s, s')
  where
    s = show x
    s' = show' x 0 ""

main :: IO ()
main = do
  check smt1 (C () ())
  check smt2 (C (C () ()) (() :+: ()))
  check smt2 ((() :+: ()) :+: (() :+: ()))
  check smt2 (R (C () ()) (C () ()))
  where
    smt1 = showsMyType (flip showsPrec)
    smt2 = showsMyType smt1
