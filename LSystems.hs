import Music

data DetGrammar a = DetGrammar a [(a,[a])] deriving Show -- deterministic grammmar

detGenerate :: Eq a => DetGrammar a -> [[a]]
detGenerate = undefined
