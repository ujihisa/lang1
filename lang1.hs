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
  -- putStrLn "stackmachine"
  -- mapM_ (\s -> print (compile s)) stmts
  run (-1) $ M.fromList $ map (optimize . compile) stmts
  -- putStrLn "registermachine"
  -- mapM_ (\s -> print $ compileReg s) stmts
  -- mapM_ (\s -> print $ (optimizeReg . compileReg) s) stmts
  runReg (-1) $ M.fromList $ map (optimizeReg . compileReg) stmts
main1 = do
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
  ISetLocal String | IGetLocal String | ISetLocalKeep String
  deriving (Show, Eq)

data InstReg =
  IRegMov Register Register |
  IRegAdd Register Register |
  IRegNeg Register |
  IRegLt Register Register |
  IRegMult Register Register |
  IRegMovVal Int Register |
  IRegCall1 String Register |
  IRegTailCall1 String Register |
  IRegZeroJump Register Int |
  IRegJump Int |
  IRegLabel Int |
  IRegVar_ String Register
  deriving (Show, Eq)

newtype Register = Register Int deriving Eq
instance Show Register where
  show (Register r) = 'r' : show r

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

compileReg :: Stmt -> (String, [InstReg])
compileReg (Stmt name argname ast) =
  let insts = fst $ S.runState f (0, [1..10]) in
  (name, insts)
  where
    f = do
      (x, l) <- compileReg' ast
      return $ map (replaceVar (Register 0) argname) x ++ [IRegMov l (Register 0)]
compileReg' :: AST -> S.State (Int, [Int]) ([InstReg], Register)
compileReg' (Plus a b) = do
  (x, l1) <- compileReg' a
  (y, l2) <- compileReg' b
  freeRegister l1
  return (x ++ y ++ [IRegAdd l1 l2], l2)
compileReg' (Minus a b) = do
  (x, l1) <- compileReg' a
  (y, l2) <- compileReg' b
  freeRegister l1
  return (x ++ y ++ [IRegNeg l2, IRegAdd l1 l2], l2)
compileReg' (Lt a b) = do
  (x, l1) <- compileReg' a
  (y, l2) <- compileReg' b
  freeRegister l1
  return (x ++ y ++ [IRegLt l1 l2], l2)
compileReg' (Mult a b) = do
  (x, l1) <- compileReg' a
  (y, l2) <- compileReg' b
  freeRegister l1
  return (x ++ y ++ [IRegMult l1 l2], l2)
compileReg' (Call1 name a) = do
  (x, l) <- compileReg' a
  return (x ++ [IRegCall1 name l], l)
compileReg' (IfThenElse cond thenAst elseAst) = do
  label1 <- nextLabel
  label2 <- nextLabel
  (x, l1) <- compileReg' cond
  (y, l2) <- compileReg' thenAst
  (z, l3) <- compileReg' elseAst
  freeRegister l1
  freeRegister l2
  return (x ++ [IRegZeroJump l1 label1] ++ y ++ [IRegMov l2 l1, IRegJump label2, IRegLabel label1] ++ z ++ [IRegMov l3 l1, IRegLabel label2], l1)
compileReg' (Value i) = do
  l <- newRegister
  return ([IRegMovVal i l], l)
compileReg' (Let name val expr) = do
  (x, l1) <- compileReg' val
  (y, l2) <- compileReg' expr
  let y2 = map (replaceVar l1 name) y
  freeRegister l1
  return (x ++ y2, l2)
compileReg' (Var name) = do
  l <- newRegister
  return ([IRegVar_ name l], l)
replaceVar l1 name1 (IRegVar_ name2 l2)
  | name1 == name2 = IRegMov l1 l2
replaceVar _ _ inst = inst
newRegister = do
  (label, (i:is)) <- S.get
  S.put (label, is)
  return $ Register i
freeRegister (Register i) = do
  (label, is) <- S.get
  S.put (label, i:is)
nextLabel = do
  (label, is) <- S.get
  S.put (label + 1, is)
  return $ label + 1

optimizeReg :: (String, [InstReg]) -> (String, [InstReg])
optimizeReg (x, is) = (x, optimizeReg' is)
optimizeReg' :: [InstReg] -> [InstReg]
optimizeReg' [] = []
optimizeReg' (IRegCall1 n r : IRegMov r1 r2 : xs)
  | xs == [] || all labelOrJumpReg (init xs) =
    IRegTailCall1 n r : optimizeReg' xs
optimizeReg' (IRegCall1 n r : IRegMov r1 r2 : IRegJump l : xs)
  | all labelOrJumpReg $ init $ dropWhile (/= IRegLabel l) xs =
    IRegTailCall1 n r : IRegJump l : optimizeReg' xs
optimizeReg' (x:xs) = x : optimizeReg' xs

labelOrJumpReg (IRegLabel _) = True
labelOrJumpReg (IRegJump _) = True
labelOrJumpReg _ = False

optimize :: (String, [Inst]) -> (String, [Inst])
optimize (x, is) = (x, optimize' is)

optimize' :: [Inst] -> [Inst]
optimize' [] = []
optimize' (ICall x : xs)
  | all labelOrJump xs = ITailCall x : optimize' xs
optimize' (ICall x : IJump l : xs)
  | all labelOrJump $ dropWhile (/= ILabel l) xs = ITailCall x : IJump l : optimize' xs
optimize' (ISetLocal x : IGetLocal y : xs)
  | x == y = ISetLocalKeep x : optimize' xs
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
run' instmap ((ISetLocalKeep name):xs) = do
  value <- peek
  setenv name value
  run' instmap xs
run' instmap ((ISetLocal name):xs) = do
  value <- pop
  setenv name value
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

peek = (head . fst) `fmap` S.get

setenv name value = do
  (s, env) <- S.get
  S.put (s, M.insert name value env)

getenv name = do
  (_, env) <- S.get
  push $ fromJust $ M.lookup name env

runReg :: Int -> M.Map String [InstReg] -> IO ((), [M.Map Int Int])
runReg args instmap = flip S.runStateT [M.empty] $ do
  runReg' instmap [IRegMovVal args (Register 0), IRegCall1 "main" (Register 0)]
runReg' instmap ((IRegMov r1 r2):xs) = do
  a <- getRegister r1
  setRegister r2 a
  runReg' instmap xs
runReg' instmap ((IRegAdd r1 r2):xs) = do
  a <- getRegister r1
  b <- getRegister r2
  setRegister r2 (a + b)
  runReg' instmap xs
runReg' instmap ((IRegNeg r1):xs) = do
  a <- getRegister r1
  setRegister r1 (negate a)
  runReg' instmap xs
runReg' instmap ((IRegLt r1 r2):xs) = do
  a <- getRegister r1
  b <- getRegister r2
  setRegister r2 (if a < b then 1 else 0)
  runReg' instmap xs
runReg' instmap ((IRegMult r1 r2):xs) = do
  a <- getRegister r1
  b <- getRegister r2
  setRegister r2 (a * b)
  runReg' instmap xs
runReg' instmap ((IRegMovVal i r1):xs) = do
  setRegister r1 i
  runReg' instmap xs
runReg' instmap ((IRegCall1 name r1):xs) = do
  a <- getRegister r1
  if name == "print" then do
    liftIO $ print a
  else do
    backupRegister
    setRegister (Register 0) a
    runReg' instmap (fromJust $ M.lookup name instmap)
    b <- getRegister (Register 0)
    clearRegister
    setRegister r1 b
  runReg' instmap xs
runReg' instmap ((IRegTailCall1 name r1):xs) = do
  if name == "print" then do
    -- delegation
    runReg' instmap ((IRegCall1 name r1):xs)
    runReg' instmap xs
  else do
    a <- getRegister r1
    setRegister (Register 0) a
    runReg' instmap (fromJust $ M.lookup name instmap)
runReg' instmap ((IRegZeroJump r1 label):xs) = do
  a <- getRegister r1
  if a == 0
     then runReg' instmap $ dropWhile (/= IRegLabel label) xs
     else runReg' instmap xs
runReg' instmap ((IRegJump label):xs) = do
  runReg' instmap $ dropWhile (/= IRegLabel label) xs
runReg' instmap ((IRegLabel _):xs) = do
  runReg' instmap xs
runReg' instmap [] = return ()

getRegister (Register r) = do
  (m:ms) <- S.get
  return $ fromJust $ M.lookup r m
setRegister (Register r) v = do
  (m:ms) <- S.get
  S.put $ M.insert r v m : ms
backupRegister = do
  ms <- S.get
  S.put $ M.empty : ms
clearRegister = do
  (_:ms) <- S.get
  S.put ms

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
