import System.Environment (getArgs)
import Data.List ((\\), permutations, )
import Data.Maybe (listToMaybe)
import Control.Monad (guard)

data Term = Term [Integer] [Operation]
data Operation = Operation {fun :: (Integer -> Integer -> Maybe Integer), name :: String}

instance Show Term where
  show (Term [i] []) = show i
  -- show (Term (is) (os)) = show is ++ "||" ++ (show $ map name os)
  show (Term (is) (os)) =  "(" ++ show (Term (init is) (init os)) ++ (name $ last os) ++  show (last is)  ++ ")"
  -- show (Term _ _) = undefined

operations = [  Operation (\x y -> return (x + y)) "+"
             ,  Operation (\x y -> return (x * y)) "*"
             ,  Operation (\x y -> guard (x > y) >> return (x - y)) "-"
             ,  Operation (\x y -> guard (x `mod` y == 0) >> return (x `div` y)) "/"
             ]

equals :: Integer -> Term -> (Bool, Int)
equals g (Term [l] []) = (l == g, 0)
equals g (Term (i1:i2:is) ((Operation f _):ops)) = let res = f i1 i2
                                                   in case res of
                                                        Nothing -> (False, undefined)
                                                        Just v -> if v == g
                                                                  then (True, length is)
                                                                  else equals g (Term (v:is) ops)

shortenTermBy :: Term -> Int -> Term
shortenTermBy t 0 = t
shortenTermBy (Term is op) n = (Term (reverse $ drop n $ reverse is) (reverse $ drop n $ reverse op))

{-# INLINE nest #-}
nest :: Int -> (a -> a) -> a -> a
nest 0 _ x = x
nest n f x = f (nest (n-1) f x)

variateRep :: Int -> [a] -> [[a]]
variateRep n x =
   if n<0 then [] else nest n (\y -> concatMap (\z -> map (z:) y) x) [[]]

terms :: [Integer] -> [Term]
terms nums = do p <- permutations nums
                ops <- variateRep (length nums - 1) operations
                return $ Term p ops

solve :: [Integer] -> Integer -> [Term]
solve nums target = let possible_solutions = map (\x -> (x, equals target x)) (terms nums)
                        solutions = filter (\x -> ((fst.snd) x)) possible_solutions
                        shortSols = map (\(x, (True, n)) -> shortenTermBy x n) solutions
                        -- shortSols = map (\(x, (True, n)) -> x ) solutions
                    in shortSols

size (Term n _) = length n

monotonic' :: Term -> [Term] -> [Term]
monotonic' m (l:ls) = if size l <= size m then l:(monotonic' l ls) else monotonic' m ls
monotonic' m [] = []

monotonic [] = []
monotonic (l:ls) = l:(monotonic' l ls)

main = do args <- getArgs
          let nums = (read (args !! 0))::[Integer]
              target = (read (args !! 1))::Integer
          mapM print $ monotonic $ solve nums target
