import qualified Text.Parsec as P
import Control.Monad (forM_, when)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Data.Map (Map(..))
import Data.Maybe (fromJust)

data AST = Plus AST AST | Mult AST AST | Call1 String AST |
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
  ISetEnv String deriving Show
compile :: Stmt -> (String, [Inst])
compile (Stmt name argname ast) = (name, ISetEnv argname : compile' ast)
compile' :: AST -> [Inst]
compile' (Plus a b) = compile' a ++ compile' b ++ [IPlus]
compile' (Call1 name ast) = compile' ast ++ [ICall name]
compile' (Mult a b) = compile' a ++ compile' b ++ [IMult]
compile' (Value n) = [IPush n]
compile' (Var name) = [IRef name]

run :: M.Map String [Inst] -> IO ((), ([Int], M.Map String Int))
run instmap = flip S.runStateT ([], M.empty) $ do
  push (-1) -- args
  run' instmap (ICall "main")
run' instmap IPlus = do
  x <- pop
  y <- pop
  push $ x + y
run' instmap IMult = do
  x <- pop
  y <- pop
  push $ x * y
run' instmap (ICall name) = do
  -- liftIO $ print $ "<<" ++ name ++ ">> called."
  if name == "print" then do
    x <- pop
    liftIO $ print x
    push x
  else do
    -- liftIO $ print $ "<<" ++ name ++ ">> called."
    mapM_ (run' instmap) (fromJust $ M.lookup name instmap)
run' instmap (IPush i) = push i
run' instmap (IRef name) = do
  -- liftIO $ print $ "<<" ++ name ++ ">> referred."
  getenv name
run' instmap (ISetEnv name) = do
  -- liftIO $ print $ "<<" ++ name ++ ">> setenv."
  setenv name

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
  uncurry Plus `fmap` call2 "+" P.<|>
  uncurry Mult `fmap` call2 "*" P.<|>
  call1 P.<|>
  Var `fmap` P.many1 P.letter P.<|>
  (Value . read) `fmap` P.many P.digit

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
