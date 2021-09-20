import Findall
-- This Curry program has been written by Michael Hanus and Pierre Rety.
--
-- The user has to provide a melody, and the program proposes an accompaniment
-- composed of chords. More precisely, for each bar the program yields
-- one or several chord choices (frequently, one major chord and one minor
-- chord), among which the musician can choose according to his/her
-- sensibility.

-- The main program as well as three examples lie at the end of this file.
-- The main function is called 'run'.

-- Some optimizations to improve the efficiency could be done.

------------------------ Restrictions ------------------------------------

-- The current program can only deal with melodies written in the C major
-- scale (or A minor), without accidentals. This could easily be extended
-- to any tonality by performing a translation over the integers that
-- encode notes.

-- The current program can only deal with bars of type 4/4.

------------------------ Data type declarations ----------------------------

-- The notes (C major scale, R denotes a rest)
data Note = C | D | E | F | G | A | B | R

-- The chords
data Chord = C_maj | D_maj | E_maj | F_maj | G_maj | G_maj7 | A_maj | B_maj |
             C_min | D_min | E_min | F_min | G_min | A_min | B_min


-------------------------- Auxiliary functions -----------------------------

-- Absolute value.
abs :: Int -> Int
abs x = if x<0 then 0-x else x

-- The minimum of two integers.
min :: Int -> Int -> Int
min x y = if x<y then x else y

-- The minimal value in a non empty list of integers.
minValueInList :: [Int] -> Int
minValueInList (x:l) = foldr min x l

-- Maps a two argument function on two lists.
map2 :: (t1->t2->t)->[t1]->[t2]->[t]
map2 f xs ys = map (uncurry f) (zip xs ys)

------------------------- Generation ---------------------------------------

-- Generates a list of (chord, duration). This list corresponds to
-- one melody bar.

-- A duration is an integer in the interval [1..8]. The duration 1
-- is a eighth note, so the duration 8 is a whole note, i.e. a note that
-- covers an entire 4/4 bar.
-- Thus the duration sum of the generated list is equal to 8.

-- There are three generation functions : the first generates chords
-- covering the entire bar, the second generates chords covering the
-- half-bars, and the third generates chords covering the quarter-bars.

one_chord_bar :: [(Chord, Int)]
one_chord_bar = [(aChord, 8)]

two_chord_bar :: [(Chord, Int)]
two_chord_bar = [(aChord, 4), (aChord, 4)]

four_chord_bar :: [(Chord, Int)]
four_chord_bar = [(aChord, 2), (aChord, 2), (aChord, 2), (aChord, 2)]

-- Versions without demand-driven generation of the search space:
--two_chord_bar  | [(aChord, 4), (aChord, 4)] =:= bar
--               = bar
--  where bar free
--four_chord_bar | [(aChord, 2), (aChord, 2), (aChord, 2), (aChord, 2)] =:= bar
--               = bar
--  where bar free

-- Generates the major and minor chords of the C major scale,
-- (all their notes must belong to the C major scale).
aChord :: Chord
aChord = C_maj
aChord = F_maj
aChord = G_maj
aChord = G_maj7
aChord = D_min
aChord = E_min
aChord = A_min


------------------------- Dissonance checking ------------------------------

-- Given a chord bar (a list of (chord, duration); let x be its length)
-- as well as a melody bar (a list of (note,duration)),
-- returns this chord bar if it fits the melody bar, i.e.
-- the dissonance between each chord of the chord bar and the
-- corresponding part of the melody is not too great (local criterion),
-- and the sum of these dissonances is not too great (global criterion).
-- Otherwise returns nothing.
--
-- The global criterion applies if x = 2 (resp. x = 4), in this case the
-- dissonance sum must not be greater than 6 (resp 4).
--
-- The melody is first split into x parts, then it is
-- compacted to put together into one note the identical consecutive notes, in
-- order to consider the melody parts containing only one or several times
-- the same note as a particular case.
checkBarDiss :: [(Chord, Int)] -> [(Note, Int)] -> ([(Chord, Int)],Int)
-- This formulation does not work due to restrictions in the current parser:
checkBarDiss chordbar melodybar
      | (x == 1 || dissonance_sum <= 4 || (x == 2 && dissonance_sum <= 6))
      = (map fst chord_and_dissonance_bar , dissonance_sum)

 where x = length chordbar
       durations = map snd chordbar
       split_melody = splitBar durations melodybar
       compacted_melody = map2 compact_melody split_melody durations
       chord_and_dissonance_bar
                    = map2 checkDiss_wrt_one_chord chordbar compacted_melody
       dissonance_sum = foldr (+) 0 (map snd chord_and_dissonance_bar)

-- checkBarDiss chordbar melodybar =
--    checkBarDiss_aux chordbar chord_and_dissonance_bar
--  where chord_and_dissonance_bar
--            = map2 checkDiss_wrt_one_chord
--                   chordbar (compact_melodies melodybar (map snd chordbar))
-- 
-- checkBarDiss_aux chordbar chord_and_dissonance_bar
--       | (x == 1 || dissonance_sum <= 4 || (x == 2 && dissonance_sum <= 6))
--       = (map fst chord_and_dissonance_bar , dissonance_sum)
--  where x = length chordbar
--        dissonance_sum = foldr (+) 0 (map snd chord_and_dissonance_bar)
-- 
-- compact_melodies melodybar durations =
--   map2 compact_melody split_melody durations
--     where split_melody = splitBar durations melodybar


-- Given a (chord, duration) as well as a melody part of the same duration
-- (given by a non empty list of (note, duration)),
-- returns this (chord, duration) as well as the dissonance between it and
-- the melody part if this dissonance is not greater than some bound.
-- Otherwise returns nothing.

checkDiss_wrt_one_chord :: (Chord,Int) -> [(Note,Int)] -> ((Chord,Int), Int)

-- Particular cases : the melody part contains only one note.
checkDiss_wrt_one_chord (chord,td) [(C,_)]
        | ((chord == F_maj) || (chord == A_min)) = ((chord,td),0)
checkDiss_wrt_one_chord (chord,td) [(D,_)]
        | ((chord == G_maj) || (chord == D_min)) = ((chord,td),0)
checkDiss_wrt_one_chord (chord,td) [(E,_)]
        | ((chord == C_maj)) = ((chord,td),0)
checkDiss_wrt_one_chord (chord,td) [(F,_)]
        | ((chord == D_min)) = ((chord,td),0)
checkDiss_wrt_one_chord (chord,td) [(G,_)]
        | ((chord == C_maj) || (chord == E_min)) = ((chord,td),0)
checkDiss_wrt_one_chord (chord,td) [(A,_)]
        | ((chord == F_maj) || (chord == A_min)) = ((chord,td),0)
checkDiss_wrt_one_chord (chord,td) [(B,_)]
        | ((chord == G_maj) || (chord == E_min)) = ((chord,td),0)


-- The general case.
-- If the melody part is [(n1,d1),...,(n_p,d_p)] and if diss_i denotes
-- the dissonance between the chord and the note n_i,
-- the dissonance between the chord and the melody part is
-- 3*(diss_1*d1 + ... + diss_p*d_p) + (diss_1 or diss_2).
-- By adding diss_1 or diss_2, the first non rest note has a stronger
-- weight (diss_2 instead of diss_1 when the first note is a rest).
--
-- The bound is d1 + ... + d_p, among which the d_i's corresponding to
-- rests are replaced by 0. Indeed, the dissonance between any chord
-- and the rest being equal to 0, we must not consider rests when
-- computing the bound.
-- Note that if there is no rest, the bound is equal to the duration of
-- the chord.
-- checkDiss_wrt_one_chord (t,td) ((n1,d1):rem)
--      | (not(rem == [])) && (diss <= computedBound) = ((t,td),diss)
-- 
--  where diss1 = dissonance(n1,t)
--        partialDissList = map (\(n,d)->dissonance(n,t)*d) rem
--        partialDiss = foldr (+) (diss1*d1) partialDissList
--        diss = if n1 == R
--               then 3*(partialDiss) + dissonance(fst (head rem),t)
--               else 3*(partialDiss) + diss1
--        computedBound = bound ((n1,d1):rem)
checkDiss_wrt_one_chord (t,td) ((n1,d1):rem) =
     checkDiss_wrt_one_chord_aux (t,td) ((n1,d1):rem) partialDiss
 where partialDiss = foldr (+) (dissonance(n1,t)*d1)
                           (map (\p->dissonance((fst p),t)*(snd p)) rem)
--                           (map (\(n,d)->dissonance(n,t)*d) rem)

checkDiss_wrt_one_chord_aux (t,td) ((n1,d1):rem) partialDiss
   | (not(rem == [])) && (diss <= bound ((n1,d1):rem)) = ((t,td),diss)
  where diss = if n1 == R
               then 3*partialDiss + dissonance(fst (head rem),t)
               else 3*partialDiss + dissonance(n1,t)


-- Computes the dissonance bound w.r.t. a melody part.
bound :: [(Note, Int)] -> Int
bound [] = 0
bound ((n1,d1):rem) = if n1 == R then bound rem
                                 else d1 + bound rem


-- Computes the dissonance between a note n and a chord t.
-- This is the dissonance of the minimum interval between n and
-- the notes of t. An interval between two notes is the minimal distance
-- that separates them modulo the octave.
dissonance :: (Note, Chord) -> Int
dissonance(n,t) = interval2diss (minInterval n t)

-- Returns the minimal interval between the note n and the notes held in
-- the chord t.
minInterval :: Note -> Chord -> Int
minInterval n t = minValueInList (map (notes2interval n) (chord2notes t) )

-- Computes the interval between two notes. This is the minimal number
-- of 1/2 tones that separate them modulo the octave, i.e. modulo 12
-- when working with notes converted into integers. The interval between
-- any note and the rest R is 0.
-- n1 and n2 must not be both a rest.
notes2interval :: Note -> Note -> Int
notes2interval n1 n2 = if dist>50 then 0  -- rest case.
                   else if dist>6 then 12-dist else dist
    where dist = abs(note2int n1 - note2int n2)

-- converts a note into an integer.
note2int :: Note -> Int
note2int C = 0
note2int D = 2
note2int E = 4
note2int F = 5
note2int G = 7
note2int A = 9
note2int B = 11
note2int R = 99

-- Converts an interval (a number of 1/2 tones) into a dissonance.
-- The unison produces no dissonance,
-- the 1 interval as well as the 2 interval produce a dissonance.
-- An interval greater than 2 produces no dissonance.
-- Note that unlike classical music, the 6 interval (tritone) is not
-- forbidden here, and produces no dissonance.
interval2diss :: Int -> Int
interval2diss 0 = 0
interval2diss 1 = 1
interval2diss 2 = 1
interval2diss 3 = 0
interval2diss 4 = 0
interval2diss 5 = 0
interval2diss 6 = 0

-- Gives the list of notes held in a chord.
chord2notes :: Chord -> [Note]
chord2notes C_maj  = [C,E,G]
chord2notes F_maj  = [F,A,C]
chord2notes G_maj  = [G,B,D]
chord2notes G_maj7 = [G,B,D,F]
chord2notes D_min  = [D,F,A]
chord2notes E_min  = [E,G,B]
chord2notes A_min  = [A,C,E]




-- Split a bar of the melody (given by a list of (note, duration) in the
-- second argument) into several parts (sub-lists).
-- The first argument is the list of intended durations of various parts.
-- Thus the number of intended parts is the length of this list.
-- The total durations of the first and second argument must be equal.
-- Note that the case where a note overlaps over both
-- parts is dealt with : the call of splitBar [4, 4] [(C,3), (D,2), (E,3)]
-- returns [[(C,3),(D,1)],  [(D,1),(E,3)]].
splitBar :: [Int] -> [(Note, Int)] -> [[(Note, Int)]]
splitBar [] [] = []
splitBar (d:drem) ((n,t):mrem) =
   if d==t then ([(n,t)]: splitBar drem mrem)
   else
    if t>d then
    -- in this case, the note n is too long : it is shared between two parts
          ([(n,d)]: splitBar drem ((n,t-d):mrem))
    else -- t <= d
          ( ((n,t):(head sp)) : tail sp)
          where sp = splitBar ((d-t):drem) mrem


-- Compacts a melody part given by a non empty list of (note, duration).
-- It puts together into one note the identical consecutive notes of the
-- melody part.
-- It removes the other notes of the melody part if the identical consecutive
-- notes last at least more than the 3/4 of the melody part.
-- The second argument is the length of the given melody part (this avoids
-- to compute it again).

-- For example, [(A,3),(A,1)] is replaced by [(A,4)],
-- however [(A,2),(A,1),(B,1)] is also replaced by [(A,4)].
-- Indeed, from a musical point of view, (B,1) is a short note,
-- just used here as a transition to reach the next melody part.
-- Thus, it must not affect the chords. This is why this note is removed.
compact_melody :: [(Note,Int)] -> Int -> [(Note,Int)]
compact_melody ((n,d):rem) len = compact_melody_rec n 0 ((n,d):rem) len

compact_melody_rec :: Note -> Int -> [(Note,Int)] -> Int -> [(Note,Int)]

compact_melody_rec old_n sum_d [] len =
                        if (sum_d >= three_quarters) then [(old_n, len)]
                        else [(old_n,sum_d)]
             where three_quarters = div (3*len) 4

compact_melody_rec old_n sum_d ((n,d):rem) len =
         if (n == old_n) then compact_melody_rec n (sum_d+d) rem len
         else if (sum_d >= three_quarters) then [(old_n, len)]
              else if snd (head compacted_remainder) == len
                   then compacted_remainder
                   else ((old_n, sum_d):compacted_remainder)
    where three_quarters = div (3*len) 4
          compacted_remainder = compact_melody ((n,d):rem) len


------------- Main program, encapsulated search, and printing --------------

-- Yields chords that fit the given melody bar, encapsulated in a list.
-- Chords that cover the entire bar are first sought. If there are not,
-- chord pairs, each chord of which covers a half-bar, are sought.
-- If there are not, chord 4-tuples, each chord of which covers a
-- quarter-bar, are sought.
compute_bar :: [(Note, Int)] -> [([(Chord, Int)],Int)]

-- this definition computes one/two_chord_solutions twice with the old parser:
compute_bar mbar =
  if one_chord_solutions == []
  then if two_chord_solutions == []
       then bestOf (findall (\x -> checkBarDiss four_chord_bar mbar =:= x))
       else bestOf two_chord_solutions
  else bestOf one_chord_solutions
    where one_chord_solutions =
                 findall (\x -> checkBarDiss one_chord_bar mbar =:= x)
          two_chord_solutions =
                 findall (\x -> checkBarDiss two_chord_bar mbar =:= x)


-- Filter list of pairs with a minimal (up to variance 1) snd component:
bestOf :: [(a,Int)] -> [(a,Int)]
bestOf bars = filter (\x -> (snd x)-minDiss<2) bars
 where minDiss = minValueInList (map snd bars)

-- formatting functions for bars:
format_diss_bar_chord :: ([(Chord, Int)],Int) -> String
format_diss_bar_chord (barc,diss) =
      format_bar_chord barc ++ " (" ++ posint2String diss ++ ")"
--      format_bar_chord barc

format_bar_chord [] = ""
format_bar_chord [t] = format_chord t
format_bar_chord (t1:t2:ts) = format_chord t1 ++ " " ++ format_bar_chord (t2:ts)

format_chord (chord,d) = chord2String chord ++ "/" ++ posint2String d

chord2String C_maj  = "Cmaj"
chord2String F_maj  = "Fmaj"
chord2String G_maj  = "Gmaj"
chord2String G_maj7 = "Gmaj7"
chord2String D_min  = "Dmin"
chord2String E_min  = "Emin"
chord2String A_min  = "Amin"

posint2String n = 
   if n<=9 then [chr(ord '0' + n)]
           else posint2String (n `div` 10) ++ [chr(ord '0' + n `mod` 10)]
-----


-- Prints disjunctive chord answers on one line. These are the various
-- chord propositions for one melody bar.
print_chord_alts :: [([(Chord, Int)],Int)] -> IO ()
print_chord_alts [x] = putStr (format_diss_bar_chord x) >> putStr "\n"
print_chord_alts (x:y:rem) = do
  putStr (format_diss_bar_chord x)
  putStr " || "
  print_chord_alts (y:rem)

-- The main function.
-- Prints chords that fit the given melody. A melody is a list of melody bars.
-- Each printed line corresponds to one melody bar.
run :: [[(Note, Int)]] -> IO ()
run melody = do
  putStrLn "Proposed chords:"
  mapIO_ (print_chord_alts . compute_bar) melody


----- Example : The Sounds of silence (Simon and Garfunkel) --------------

-- The chords given by Paul Simon are:
-- [[(A_min,8)], [(G_maj,8)], [(G_maj,8)],
--  [(A_min,8)], [(A_min,4),(C_maj,4)], [(F_maj,4),(C_maj,4)],
--  [(C_maj,8)], [(F_maj,4),(C_maj,4)], [(C_maj,4),(F_maj,4)],
--  [(F_maj,8)], [(F_maj,4),(C_maj,4)], [(C_maj,8)],
--  [(A_min,4),(C_maj,4)], [(G_maj,4),(A_min,4)]]
--
-- Our program finds this solution, up to a few slight differences.

-- The melody.
sounds = [
  [(R,2),(A,1),(A,1),(C,1),(C,1),(E,1),(E,1)], [(D,8)],
  [(R,2),(G,1),(G,1),(B,1),(B,1),(D,1),(D,1)], [(C,8)],
  [(R,2),(C,1),(C,1),(E,1),(E,1),(G,1),(G,1)], [(A,2),(A,1),(G,5)],
  [(R,2),(C,1),(C,1),(E,1),(E,1),(G,1),(G,1)], [(A,2),(A,1),(G,5)],
  [(R,2),(C,1),(C,1),(A,3),(A,1)], [(A,2),(A,1),(B,1),(C,3),(C,1)],
  [(C,1),(B,1),(A,2),(G,4)], [(R,2),(A,1),(G,1),(E,4)],
  [(R,1),(A,1),(A,1),(C,1),(G,4)], [(R,1),(B,3),(C,1),(A,3)]]
-- agamemnon/strict: 3.2 secs
-- agamemnon/dd:     1.36 secs


----- Example : Nicolas and Bart (J. Baez) ------------------------------

-- The chords given by J. Baez are:
-- [[(C_maj,8)], [(G_maj,8)], [(A_min,8)], [(G_maj,8)],
--  [(C_maj,8)], [(G_maj,8)], [(A_min,8)], [(G_maj,8)],
--  [(C_maj,8)], [(D_min,8)], [(G_maj7,8)], [(C_maj,8)],
--  [(C_maj,8)], [(D_min,8)], [(A_min,4),(E_maj,4)], [(A_min,8)]]
--
-- Our program finds this solution, up to one difference.
-- The last but one chord E_maj includes the foreign note G sharp.
-- However replacing E_min by E_maj is frequent, though not systematic.
-- This scenario is not included in our program. It then yields E_min
-- for this half-bar.

-- The melody.
nicolas = [
  [(E,6),(C,2)], [(D,6),(B,2)], [(C,2),(B,2),(A,4)], [(B,6),(R,2)],
  [(E,6),(C,2)], [(D,6),(B,2)], [(C,2),(B,2),(A,4)], [(D,6),(R,2)],
  [(G,6),(E,2)], [(F,6),(D,2)], [(D,2),(E,2),(F,4)], [(E,6),(R,2)],
  [(E,6),(C,2)], [(D,6),(B,2)], [(C,4),(B,4)], [(A,8)]]
-- agamemnon/strict: 1.1 secs
-- agamemnon/dd:     0.7 secs


testbar1 = [(C,2),(A,1),(G,1),(B,1),(F,1),(D,2)]

-- compute_bar(testbar1)

----- Example : Homeward bound (Simon and Garfunkel) ---------------------
-- We only consider the chorus, instead of the entire melody.

-- The chords given by Paul Simon are:
-- [[(C_maj,8)], [(F_maj,8)],
--  [(F_maj,8)], [(C_maj,8)],
--  [(C_maj,8)], [(F_maj,8)], [(F_maj,8)], [(C_maj,8)],
--  [(D_min,2),(C_maj,2),(B_flat_maj,2),(F_maj,2)], [(C_maj,8)],
--  [(D_min,2),(C_maj,2),(B_flat_maj,2),(F_maj,2)], [(C_maj,8)],
--  [(D_min,2),(C_maj,2),(B_flat_maj,2),(F_maj,2)], [(G_maj7,8)],
--  [(C_maj,8)], [(C_maj,8)]]

-- Our program finds this solution, except for the third bar, for which
-- Paul Simon kept on purpose the same chord as for the second bar,
-- i.e. Fmaj, though this causes a big dissonance with the melody.

-- On oberon, the runtime is 22507 ms.
-- Without demand-driven generation of the search space, there is no answer:
-- only 'managing allocation failure' after a runtime of a few minutes.

-- The chorus.
homeward = [
 [(C,4),(B,2),(C,2)], [(A,8)],
 [(R,2),(G,2),(A,2),(G,2)], [(E,1),(D,1),(C,4),(R,2)],
 [(C,4),(B,2),(C,2)], [(A,8)], [(A,8)], [(E,2),(R,3),(E,1),(E,1),(F,1)],
 [(F,2),(E,2),(D,2),(C,2)], [(E,2),(R,3),(E,1),(E,1),(F,1)],
 [(F,2),(E,2),(D,2),(C,2)], [(E,2),(R,3),(E,1),(E,1),(F,1)],
 [(F,2),(E,2),(D,2),(C,2)], [(D,2),(E,2),(D,4)],
 [(C,2),(G,6)], [(G,8)]]
-- agamemnon/strict: 65.9 secs
-- agamemnon/dd:     1.6 secs

homeward1 = [
 [(C,4),(B,2),(C,2)], [(A,8)],
 [(R,2),(G,2),(A,2),(G,2)], [(E,1),(D,1),(C,4),(R,2)],
 [(C,4),(B,2),(C,2)], [(A,8)], [(A,8)], [(E,2),(R,3),(E,1),(E,1),(F,1)],
 [(F,2),(E,2),(D,2),(C,2)]] --, [(E,2),(R,3),(E,1),(E,1),(F,1)],
-- [(F,2),(E,2),(D,2),(C,2)], [(E,2),(R,3),(E,1),(E,1),(F,1)],
-- [(F,2),(E,2),(D,2),(C,2)], [(D,2),(E,2),(D,4)],
-- [(C,2),(G,6)], [(G,8)]]
-- agamemnon/strict: 21.5 secs
-- agamemnon/dd:     0.8 secs

-- critical bars:
homeward2 = [[(F,2),(E,2),(D,2),(C,2)]]
-- dadealus/strict: 51.7 secs (no GC)
-- dadealus/dd:     0.7 secs (no GC)
-- agamemnon/strict: 20.5 secs
-- agamemnon/dd:     0.3 secs

homeward3 = [[(F,2),(E,2),(D,2),(C,2)], [(F,2),(E,2),(D,2),(C,2)]]
-- dadealus/strict: 1293 secs (including GC)
-- dadealus/dd:     1.4 secs (no GC)
-- agamemnon/strict: 42.5 secs
-- agamemnon/dd:     0.6 secs


---------------- Example : a french song -------------------------
-- The chords given by the author are:
-- [[(C_maj,4),(D_min,4)], [(A_min,2),(E_maj,2),(A_min,4)],
--  [(C_maj,4),(D_min,4)], [(F_maj,2),(G_maj,2),(C_maj,4)],
--  [(F_maj,4),(G_maj,4)], [(D_min,4),(C_maj,4)],
--  [(D_min,4),(A_min,4)], [(D_min,4),(E_maj,4)]]

-- Our program finds this solution, except that :
--
-- it finds E_min instead of E_maj, because the chord E_maj
-- (that contains the foreign note G sharp), is not included in our program,
--
-- for the last bar as well as the last but two bars, it generates one
-- chord for the whole bar, with the dissonance 7, that is to say
-- a dissonance value close to the bound 8.

-- The melody
frenchsong = [
 [(E,2),(E,1),(C,1),(D,3),(E,1)], [(C,2),(B,2),(A,1),(B,1),(A,2)],
 [(E,2),(E,1),(C,1),(D,3),(E,1)], [(C,2),(D,2),(E,1),(D,1),(E,2)],
 [(A,2),(A,1),(A,1),(G,2),(F,1),(E,1)], [(F,2),(A,2),(E,4)],
 [(D,2),(E,1),(D,1),(C,2),(B,1),(A,1)], [(C,2),(D,2),(B,4)]]
-- agamemnon/strict: 48.5 secs
-- agamemnon/dd:     2.0 secs

