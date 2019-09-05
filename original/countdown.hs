import System.Environment (getArgs)
import Data.List ((\\))
import Data.Maybe (listToMaybe)
import Control.Monad (guard)

data Term = Op Operation Term Term | Val Integer
data Operation = Operation {fun :: (Integer -> Integer -> Maybe Integer), name :: String}

instance Show Term where
  show (Val x) = show x
  show (Op op l r) = "(" ++ (show l) ++ (name op) ++ show r ++ ")"

size :: Term -> (Integer, Integer)
size t = (nums t, depth t)

nums :: Term -> Integer
nums (Val _) = 1
nums (Op _ l r) = nums l + nums r

depth :: Term -> Integer
depth (Val _) = 0
depth (Op _ l r) = max (depth l) (depth r)

operations = [  Operation (\x y -> guard (x >= y) >> return (x + y)) "+"
             ,  Operation (\x y -> guard (x >= y) >> return (x * y)) "*"
             ,  Operation (\x y -> guard (x > y) >> return (x - y)) "-"
             ,  Operation (\x y -> guard (x `mod` y == 0) >> return (x `div` y)) "/"
             ]
eval :: Term -> Maybe Integer
eval (Val v) = Just v
eval (Op o l r) = do l' <- eval l
                     r' <- eval r
                     (fun o) l' r'


terms :: [Integer] -> [Term]
terms nums =  do  n <- nums -- choose a number
                  [Val n] -- one solution
              ++
              do  op <- operations -- choose an operation
                  (l, r) <- subset_split nums -- choose a split
                  guard $ l /= []
                  guard $ r /= []
                  ls <- (terms l) -- choose a term for the left side
                  guard (eval ls /= Nothing)
                  rs <- (terms r) -- choose a term for the right side
                  guard (eval rs /= Nothing)
                  [Op op ls rs] -- one solution
                  
subsets :: [Integer] -> [[Integer]]
subsets []  = [[]]
subsets (x:xs) = subsets xs ++ map (x:) (subsets xs)

subset_split nums = do l <- subsets nums
                       return (l, nums \\ l)
                       
solve :: [Integer] -> Integer -> [Term]
solve nums target = let possible_solutions = map (\x -> (x, eval x)) (terms nums)
                        solutions = filter (\x -> (snd x) == (Just target)) possible_solutions
                    in map fst solutions

monotonic' :: Term -> [Term] -> [Term]
monotonic' m (l:ls) = if size l <= size m then l:(monotonic' l ls) else monotonic' m ls
monotonic' m [] = []

monotonic [] = []
monotonic (l:ls) = l:(monotonic' l ls)

main = do args <- getArgs
          let nums = (read (args !! 0))::[Integer]
              target = (read (args !! 1))::Integer
          mapM print $ monotonic $ solve nums target
