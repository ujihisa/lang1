import qualified Text.Parsec as P
import Control.Monad (forM_, when)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Data.Map (Map(..))
import Data.Maybe (fromJust)

data AST = Plus AST AST | Minus AST AST |
  Lt AST AST |
  Mult AST AST | Call1 String AST |
  IfThenElse AST AST AST |
  Let String AST AST |
  Value Int | Var String deriving Show
data Stmt = Stmt String String AST deriving Show

main = do
  -- print $ (Call1 "print" (Plus (Mult (Value 2) (Call1 "print" (Value 4))) (Call1 "f" (Value 4))))
  -- print $ parseStmt "main args: (print (+ (* 2 (print 3)) (f 4)))"
  src <- readFile "lang1.l1"
  let stmts = map parseStmt (lines src)
  forM_ stmts $ \(Stmt name argname ast) -> do
    print (name, argname)
  print $ map compile stmts
  -- very main
  run $ M.fromList $ map compile stmts

data Inst = IPlus | IMult | ICall String | IPush Int | IRef String |
  ILt |
  INeg |
  IZeroJump Int |
  IJump Int |
  ILabel Int |
  ISetEnv String
  deriving (Show, Eq)
compile :: Stmt -> (String, [Inst])
compile (Stmt name argname ast) = (name, ISetEnv argname : compile' ast)
compile' :: AST -> [Inst]
compile' (Plus a b) = compile' a ++ compile' b ++ [IPlus]
compile' (Minus a b) = compile' a ++ compile' b ++ [INeg, IPlus]
compile' (Call1 name ast) = compile' ast ++ [ICall name]
compile' (Mult a b) = compile' a ++ compile' b ++ [IMult]
compile' (Lt a b) = compile' a ++ compile' b ++ [ILt]
compile' (Value n) = [IPush n]
compile' (Var name) = [IRef name]
compile' (Let name val ast) = compile' val ++ [ISetEnv name] ++ compile' ast
compile' (IfThenElse cond thenAst elseAst) =
  let label1 = 1; label2 = 2 in
  compile' cond ++ [IZeroJump label1] ++ compile' thenAst ++
  [IJump label2, ILabel label1] ++ compile' elseAst ++ [ILabel label2]

run :: M.Map String [Inst] -> IO ((), ([Int], M.Map String Int))
run instmap = flip S.runStateT ([], M.empty) $ do
  push (-1) -- args
  run' instmap [ICall "main"]
run' instmap (IPlus:xs) = do
  y <- pop
  x <- pop
  push $ x + y
  run' instmap xs
run' instmap (IMult:xs) = do
  y <- pop
  x <- pop
  push $ x * y
  run' instmap xs
run' instmap (ILt:xs) = do
  y <- pop
  x <- pop
  -- liftIO $ print (x, y, if x < y then 1 else 0)
  push $ if x < y then 1 else 0
  run' instmap xs
run' instmap ((ICall name):xs) = do
  -- liftIO $ print $ "<<" ++ name ++ ">> called."
  if name == "print" then do
    x <- pop
    liftIO $ print x
    push x
  else do
    -- liftIO $ print $ "<<" ++ name ++ ">> called."
    (stack, env) <- S.get
    run' instmap (fromJust $ M.lookup name instmap)
    x <- pop
    S.put (tail stack, env)
    push x
  run' instmap xs
run' instmap ((IPush i):xs) =
  push i >> run' instmap xs
run' instmap ((IRef name):xs) = do
  -- liftIO $ print $ "<<" ++ name ++ ">> referred."
  getenv name
  run' instmap xs
run' instmap (INeg:xs) = do
  x <- pop
  push (-x)
  run' instmap xs
run' instmap ((ISetEnv name):xs) = do
  -- liftIO $ print $ "<<" ++ name ++ ">> setenv."
  setenv name
  run' instmap xs
run' instmap ((IJump label):xs) = do
  run' instmap $ dropWhile (/= ILabel label) xs
run' instmap ((IZeroJump label):xs) = do
  x <- pop
  if x == 0
     then run' instmap $ dropWhile (/= ILabel label) xs
     else run' instmap xs
run' instmap ((ILabel _):xs) =
  run' instmap xs
run' instmap [] = return ()

push x = do
  (memo, env) <- S.get
  S.put (x : memo, env)

pop = do
  (h:t, env) <- S.get
  S.put (t, env)
  return h

setenv name = do
  v <- pop
  (s, env) <- S.get
  S.put (s, M.insert name v env)

getenv name = do
  (_, env) <- S.get
  push $ fromJust $ M.lookup name env

parseStmt :: String -> Stmt
parseStmt xs = either (error . show) id $ P.parse parseStmt' "parseExpr" xs
parseStmt' = do
  name <- P.many1 P.letter
  P.skipMany1 P.space
  argname <- P.many1 P.letter
  P.char ':'
  P.skipMany1 P.space
  ast <- parseExpr
  return $ Stmt name argname ast

parseExpr =
  letp P.<|>
  ifp P.<|>
  uncurry Plus `fmap` call2 "+" P.<|>
  uncurry Minus `fmap` call2 "-" P.<|>
  uncurry Mult `fmap` call2 "*" P.<|>
  uncurry Lt `fmap` call2 "<" P.<|>
  call1 P.<|>
  Var `fmap` P.many1 P.letter P.<|>
  (Value . read) `fmap` P.many P.digit

letp = P.try $ do
  P.string "(let"
  P.skipMany1 P.space
  name <- P.many1 P.letter
  P.skipMany1 P.space
  val <- parseExpr
  P.skipMany1 P.space
  expr <- parseExpr
  P.char ')'
  return $ Let name val expr

ifp = P.try $ do
  P.string "(if"
  P.skipMany1 P.space
  cond <- parseExpr
  P.skipMany1 P.space
  thenAst <- parseExpr
  P.skipMany1 P.space
  elseAst <- parseExpr
  P.char ')'
  return $ IfThenElse cond thenAst elseAst

call1 = P.try $ do
  P.char '('
  name <- P.many P.letter
  P.skipMany1 P.space
  x <- parseExpr
  P.char ')'
  return $ Call1 name x

call2 op = P.try $ do
  P.char '('
  P.string op
  P.skipMany1 P.space
  x <- parseExpr
  P.skipMany1 P.space
  y <- parseExpr
  P.char ')'
  return (x, y)
