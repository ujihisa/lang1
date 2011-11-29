import qualified Text.Parsec as P
import Control.Monad (forM_, when)
import qualified Control.Monad.Trans.State as S
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import Data.Maybe (fromJust)

data AST =
  Plus AST AST |
  Minus AST AST |
  Lt AST AST |
  Mult AST AST |
  Call1 String AST |
  IfThenElse AST AST AST |
  Let String AST AST |
  Value Int |
  Var String deriving Show
data Stmt = Stmt String String AST deriving Show

main = do
  src <- readFile "lang1.l1"
  let stmts = map parseStmt (lines src)
  -- forM_ stmts $ \(Stmt name argname ast) -> do
  --  print (name, argname)
  -- mapM_ print $ map compile stmts
  mapM_ print $ map (optimize . compile) stmts
  -- very main
  run (-1) $ M.fromList $ map (optimize . compile) stmts

data Inst = IPlus | IMult | ICall String | IPush Int |
  ITailCall String |
  ILt | INeg | IZeroJump Int | IJump Int | ILabel Int |
  ISetLocal String | IGetLocal String deriving (Show, Eq)

compile :: Stmt -> (String, [Inst])
compile (Stmt name argname ast) =
  let insts = fst $ flip S.runState 0 $ compile' ast in
  (name, ISetLocal argname : insts)

compile' :: AST -> S.State Int [Inst]
compile' (Plus a b) = do
  x <- compile' a
  y <- compile' b
  return $ x ++ y ++ [IPlus]
compile' (Minus a b) = do
  x <- compile' a
  y <- compile' b
  return $ x ++ y ++ [INeg, IPlus]
compile' (Call1 name ast) = do
  x <- compile' ast
  return $ x ++ [ICall name]
compile' (Mult a b) = do
  x <- compile' a
  y <- compile' b
  return $ x ++ y ++ [IMult]
compile' (Lt a b) = do
  x <- compile' a
  y <- compile' b
  return $ x ++ y ++ [ILt]
compile' (Value n) = return [IPush n]
compile' (Var name) = return [IGetLocal name]
compile' (Let name val ast) = do
  x <- compile' val
  y <- compile' ast
  return $ x ++ [ISetLocal name] ++ y
compile' (IfThenElse cond thenAst elseAst) = do
  label1 <- next
  label2 <- next
  x <- compile' cond
  y <- compile' thenAst
  z <- compile' elseAst
  return $ x ++ [IZeroJump label1] ++ y ++ [IJump label2, ILabel label1] ++ z ++ [ILabel label2]
next = do
  x <- (+ 1) `fmap` S.get
  S.put x
  return x

optimize :: (String, [Inst]) -> (String, [Inst])
optimize (x, is) = (x, optimize' is)

optimize' :: [Inst] -> [Inst]
optimize' [] = []
optimize' (ICall x : xs)
  | all labelOrJump xs = ITailCall x : optimize' xs
optimize' (ICall x : IJump l : xs)
  | all labelOrJump $ dropWhile (/= ILabel l) xs = ITailCall x : IJump l : optimize' xs
optimize' (x:xs) = x : optimize' xs

labelOrJump (ILabel _) = True
labelOrJump (IJump _) = True
labelOrJump _ = False

run :: Int -> M.Map String [Inst] -> IO ((), ([Int], M.Map String Int))
run args instmap = flip S.runStateT ([], M.empty) $ do
  push args
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
run' instmap ((ITailCall name):xs) = do
  -- liftIO $ print $ "<<<TailCall>>> " ++ name
  if name == "print" then do
    -- delegation
    run' instmap ((ICall name):xs)
    run' instmap xs
  else do
    run' instmap (fromJust $ M.lookup name instmap)
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
run' instmap ((IGetLocal name):xs) = do
  -- liftIO $ print $ "<<" ++ name ++ ">> referred."
  getenv name
  run' instmap xs
run' instmap (INeg:xs) = do
  x <- pop
  push (-x)
  run' instmap xs
run' instmap ((ISetLocal name):xs) = do
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
