import Music
import System.Random
import Data.List
import Data.Maybe

data DetGrammar a = DetGrammar a [(a,[a])] deriving Show -- deterministic grammmar

detGenerate :: Eq a => DetGrammar a -> [[a]]
detGenerate x@(DetGrammar start rules) = [start] : map (concatMap f) (detGenerate x) where f v = fromMaybe [v] (lookup v rules)

data Grammar a = Grammar a (Rules a) deriving Show
data Rules a   = Uni [Rule a] | Sto [(Rule a, Prob)] deriving (Eq, Ord,Show)
data Rule a    = Rule {lhs :: a, rhs :: a} deriving (Eq, Ord, Show)
type Prob      = Float

generate :: RandomGen g => Grammar [Char] -> g -> [[Char]]
generate (Grammar start rules) g = if checkProbs $ stoRules then map fst $ iterate (gen stoRules) (start, g) else error "Probabilities aren't adding upto one" where
  stoRules = toStoRules rules

gen :: RandomGen g => Rules [Char] -> ([Char],g) -> ([Char],g)
gen (Sto r) (initial, gen) = foldl (\(x,y) z -> let (m,n) = onSingle rules ([z],y) in (x++m,n)) ("",gen) initial where
  rules = map f $ groupBy (\x y -> (lhs $ fst $ x) == (lhs $ fst $ y)) r
  f l = let (x,y) = unzip l in zip x $ scanl1 (+) y

onSingle :: (Eq a, RandomGen g) => [[(Rule a, Prob)]] -> (a, g) -> (a, g)
onSingle [] x = x
onSingle (x:xs) v = if lhs (fst (head x)) == fst v then applyRule x v else onSingle xs v where
  applyRule rules (i, gen) = let (rVal, newGen) = randomR (0.0,1.0) gen in (rhs $ fst $ head $ filter ((rVal <) . snd) rules, newGen)

toStoRules :: (Eq a, Ord a) => Rules a -> Rules a
toStoRules (Sto x)   = Sto $ sort x
toStoRules (Uni x)   = Sto $ insertProbability $ groupBy (\x y -> lhs x == lhs y) $ sort x where
  insertProbability [] = []
  insertProbability (x:xs) = map (\y -> (y , 1 / fromIntegral (length x))) x ++ insertProbability xs

checkProbs :: Eq a => Rules a -> Bool
checkProbs (Sto x) = all check $ groupBy (\x y -> (lhs $ fst x) == (lhs $ fst y)) x where
  check x = abs (1 - (foldr ((+) . snd) 0 x)) < 0.001

-- use hashmaps

x = [Rule {lhs = "x", rhs = "zy"}, Rule {lhs = "y", rhs = "zs"}, Rule {lhs = "s", rhs = "xy"}, Rule {lhs = "x", rhs = "yx"}]

y = Grammar "x" $ Uni x
