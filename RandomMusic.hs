import Music
import System.Random
import System.Random.Distributions
import qualified Data.MarkovChain as M

toAbsP1 :: Float -> AbsPitch
toAbsP1 x = round (40 * x + 30)

tn :: Dur
tn = 1/32

mkNote1 :: AbsPitch -> Music Pitch
mkNote1 = note tn . pitch

mkLine1 :: [AbsPitch] -> Music Pitch
mkLine1 = line . (take 32) . map mkNote1

m1 :: Music Pitch -- Linear Distribution
m1  = mkLine1 $ map toAbsP1 $ f (mkStdGen 11) where f z = let (x,y) = linear z in x : f y

ps :: [Pitch]
ps = [(C,4),(D,4),(E,4)]

mkNote3 :: Pitch -> Music Pitch
mkNote3 = note tn

mkLine3 :: [Pitch] -> Music Pitch
mkLine3 = line . take 64 . map mkNote3

mc :: [Pitch] -> Int -> Music Pitch
mc p n = mkLine3 (M.run n p 0 (mkStdGen 11))
