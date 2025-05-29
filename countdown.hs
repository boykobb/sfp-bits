{- Програмата решава следната задача. По зададено на стандартния вход цяло положително число N
   и (на следващия ред) множество S от такива числа да се намерят и отпечатат всички изрази, за
   които е изпълнено следното:
       • имат за стойност числото N;
       • образувани са с помощта на операциите + – * / и числата от множеството S;
       • всяко число от S може да бъде използвано най-много по веднъж;
       • всяка от операциите може да не участва в израза или да участва повече от веднъж;
       • всяка приложена операция трябва да има за резултат цяло положително число.

   Разработена е от Греъм Хътън и това подробно е показано в гл. 9 от книгата му
   Programming in Haskell (http://cs.nott.ac.uk/~pszgmh/pih.html).
   Примери:
        20  \n  1 2 3 7           (7 решения)
        46  \n  1 2 3 5 7         (20 решения)
       765  \n  1 3 7 10 25 50    (49 решения)                                                 -}

-- Конструктори (константи) за операциите
data Op = Add | Sub | Mul | Div

-- Израз е или число, или операция, приложена към два аргумента изрази
data Expr = Val Int | App Expr Op Expr

solutions ns n = [e | nb <- subbags ns, (e,m) <- results nb, m == n] where
-- наредените подмножества от членовете на редица
  subbags xs = [zs | ys <- subsequences xs, zs <- permutations ys] where
  -- подредиците (подмножествата, вкл. празното и това от всички членове) на редица:
    subsequences []     = [[]]
    subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)
  -- пермутациите на редица:
    permutations []     = [[]]
    permutations (x:xs) = [zs | ys <- permutations xs, zs <- interleave x ys] where
    -- x се поставя по веднъж на всяко от n+1-те места в редица:
      interleave x xs = (x:xs) : case xs of [] -> []; y:ys -> map (y:) (interleave x ys)
-- за дадена редица от числа – всички изрази, всеки със стойността си:
  results []  = []
  results [n] = [(Val n, n) | n>0]
  results ns  = [res | (ls,rs) <- nesplit ns, lx <- results ls, ry <- results rs, res <- combine lx ry] where
  -- всички възможни разделяния на редица на две части:
    nesplit xs = [splitAt i xs | i <- [1..length xs-1]]
  -- всички възможни изрази от два дадени аргумента, всеки със стойността си:
    combine (l,x) (r,y) = [(App l o r, apply x o y) | o <- [Add,Sub,Mul,Div], valid x o y] where
      -- проверка за допустимост и отхвърляне на излишните възможности:
      valid x o y = case o of
        Add -> x <= y
        Sub -> x > y
        Mul -> x /= 1 && y /= 1 && x <= y
        Div -> y /= 1 && x `mod` y == 0
      -- числова стойност на прост израз (едно действие):
      apply x o y = case o of
        Add -> x+y
        Sub -> x-y
        Mul -> x*y
        Div -> x `div` y

-- В главната функция се извършват всички В/И действия. Действията в do се извършват последователно.
main = do line <- getLine                               -- getLine има монадна стойност (IO String)
          n <- return (read line)                       -- „опаковане“ на резултата на return line в монада
          line <- getLine                               -- различна от горната променлива със същото име
          ns <- return (map read (words line))          -- монада от списъка от образуващите числа
          putStr (unlines (map show (solutions ns n)))  -- String -> IO ()

-- За удобство на четене на резултата: образуване на низ с инфиксен запис на израз.
instance Show Expr where
  show (Val x)     = show x
  show (App x o y) = "(" ++ show x ++ co ++ show y ++ ")" where 
    co = case o of
      Add -> "+"
      Sub -> "-"
      Mul -> "*"
      Div -> "/"
