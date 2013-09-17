type Octave = Int
type Pitch = (PitchClass, Octave)
type Dur = Rational

qn :: Dur
qn = 1/4 -- quarter note

data PitchClass = Cff | Cf | C | Dff | Cs | Df | Css | D | Eff | Ds | Ef | Fff | Dss | E | Es | Ff | F | Gff | Ess | Fs | Gf | Fss | G | Aff | Gs | Af | Gss | A | Bff | As | Bf | Ass | B | Bs | Bss

-- data Primitive = Note Dur Pitch | Rest Dur

data Primitive a = Note Dur a | Rest Dur

data Music a = Prim (Primitive a)
             | Music a :+: Music a -- Sequential 
             | Music a :=: Music a -- Parallel
             | Modify Control (Music a)

infixr 5 :+:, :=:

data Control = Tempo Rational
             | Transpose AbsPitch
             | Instrument InstrumentName
             | Phrase [PhraseAttribute]
             | Player PlayerName

data PhraseAttribute
type AbsPitch = Int
type PlayerName = String

data InstrumentName = Guitar | Violin | Piano

note :: Dur -> a -> Music a
note = (Prim .) . Note

rest :: Dur -> Music a
rest = Prim . Rest

tempo :: Dur -> Music a -> Music a
tempo = Modify . Tempo

-- tranpose and similar stuff

cff :: Octave -> Dur -> Music Pitch
cff x y = note y (Cff, x) 

-- Similar funcs for other Pitchclasses
pcToInt :: PitchClass -> Int
pcToInt x = case x of
            Cff -> -2
            Cf  -> -1 -- include other pitch classes

absPitch :: Pitch -> AbsPitch
absPitch (pc, octv) = 12 * octv + pcToInt pc

pitch :: AbsPitch -> Pitch
pitch ap = let (octv, n) = divMod 12 ap in ([C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B] !! n,octv)

trans :: Int -> Pitch -> Pitch
trans i p = pitch $ absPitch p + i

line :: [Music a] -> Music a
-- line = fold (:+:) (rest 0)
line = foldr1 (:+:) 

chord :: [Music a] -> Music a
-- chord = fold (:=:) (rest 0) 
chord = foldr1 (:=:)
