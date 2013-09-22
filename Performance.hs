module Performance where

import Music

type Performance = [Event]

data Event = Event {eTime   :: PTime,
                    eInst   :: InstrumentName,
                    ePitch  :: AbsPitch,
                    eDur    :: DurT,
                    eVol    :: Volume,
                    eParams :: [Double]} deriving (Eq, Ord, Show)

type PTime  = Rational
type DurT   = Rational
type Volume = Integer 

data Context a = Context {cTime   :: PTime,
                          cPlayer :: Player a,
                          cInst   :: InstrumentName,
                          cDur    :: DurT,
                          cKey    :: Key,
                          cVol    :: Volume} deriving Show

perform :: PMap a -> Context a -> Music a -> Performance
perform pm c@Context{cTime=t, cPlayer=pl, cDur=dt, cKey=k} m = case m of
        Prim (Note d p)         -> playNote pl c d p
        Prim (Rest d)           -> []
        m1 :+: m2               -> let c' = c {cTime = t + dur m1 * dt} in perform pm c m1 ++ perform pm c' m2
        m1 :+: m2               -> merge (perform pm c m1) (perform pm c m2)
        Modify (Tempo r) m      -> perform pm (c {cDur = dt/r}) m
        Modify (Transpose p) m  -> perform pm (c {cKey = k+p}) m
        Modify (Instrument i) m -> perform pm (c {cInst =i}) m
        Modify (Player pn) m    -> perform pm (c {cPlayer = pm pn}) m
        Modify (Phrase pa) m    -> interpPhrase pl pm c pa m

type PMap a = PlayerName -> Player a
type Key    = AbsPitch

metro :: Int -> Dur -> DurT
metro s d = 60 / (fromIntegral s * d)

merge :: Performance -> Performance -> Performance
merge a@(e1 : es1) b@(e2 : es2) = if eTime e1 < eTime e2 then e1 : merge es1 b else e2 : merge a es2
merge [] es2                    = es2
merge es1 []                    = es1

data Player a = MkPlayer {pName        :: PlayerName,
                          playNote     :: NoteFun a,
                          interpPhrase :: PhraseFun a,
                          notatePlayer :: NotateFun a} deriving Show

type NoteFun a   = Context a -> Dur -> a -> Performance
type PhraseFun a = PMap a -> Context a -> [PhraseAttribute] -> Music a -> (Performance, DurT)
type NotateFun a = ()

data NoteHead = DiamondHead deriving (Eq, Ord, Show)

data NoteAttribute = Volume Int | Fingering Integer | Dynamics String | Params [Double] deriving (Eq, Show)

type Music1 = Music Note1
type Note1  = (Pitch, [NoteAttribute])

defPlayer :: Player Note1 -- default player
defPlayer = MkPlayer {pName = "Default", playNote = defPlayNote defNasHandler, interpPhrase = defInterpPhrase defPasHandler, notatePlayer = ()}
