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

timesM :: Int -> Music a -> Music a
timesM 0 _ = rest 0 
timesM x y = y :+: timesM (x-1) y

addDur :: Dur -> [Dur -> Music a] -> Music a
addDur x y = line [f x | f <- y]

graceNote :: Int -> Music Pitch -> Music Pitch
graceNote x (Prim (Note d p)) = note (d/8) (trans x p) :+: note (7*d/8) p
gracenote _ _ = error "Gracenote can only be added to a note"

delayM :: Dur -> Music a -> Music a
delayM = (:+:) . rest

repeatM :: Music a -> Music a
repeatM x = x :+: repeatM x

lineToList :: Music a -> [Music a]
lineToList (Prim (Rest 0)) = []
lineToList (x :+: xs)      = x : lineToList xs
lineToList _               = error "lineToList: Args not created by func line"

invert :: Music Pitch -> Music Pitch
invert = undefined

dur :: Music a -> Dur
dur x = case x of
        Prim (Note d _)    -> d
        Prim (Rest d)      -> d
        m1 :+: m2          -> dur m1 + dur m2
        m1 :=: m2          -> dur m1 `max` dur m2
        Modify (Tempo r) m -> dur m / r
        Modify _ m         -> dur m

revM :: Music a -> Music a
revM x@(Prim _)   = x
revM (Modify t m) = Modify t (revM m)
revM (m1 :+: m2)  = revM m2 :+: revM m1 
revM (m1 :=: m2)  | d1 > d2   = revM m1 :=: (rest (d1-d2) :+: revM m2)
                  | otherwise = revM m2 :=: (rest (d2-d1) :+: revM m1)
                  where d1 = dur m1
                        d2 = dur m2

cut :: Dur -> Music a -> Music a
cut d m | d <=0              = rest 0
cut d x@(Prim (Note y z))    = if d > y then x else note d z
cut d x@(Prim (Rest y))      = if d > y then x else rest d
cut d (m1 :+: m2)            = if d1 > d then m1 :+: cut (d1-d) m2 else cut d m1 where d1 = dur m1
cut d (m1 :=: m2)            = cut d m1 :=: cut d m2
cut d x@(Modify (Tempo r) m) = Modify (Tempo r) $ cut (r*d) m
cut d (Modify t m)           = Modify t $ cut d m  

(/=:) :: Music a -> Music a -> Music a
x /=: y = cut (min (dur x) (dur y)) (x :=: y)

-- trill

grace :: Int -> Rational -> Music Pitch -> Music Pitch
grace i d (Prim (Note x y)) = note (x*d) (trans i y) :+: note ((1-d)*x) y
grace i d _                 = error "grace: grance can only be added to a note"

-- grace2
-- Percussion
