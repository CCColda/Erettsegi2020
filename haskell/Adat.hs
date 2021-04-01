module Adat where

{- region OOPP -}

type OOPP = (Int, Int)

oopp_max :: OOPP -> OOPP -> OOPP
oopp_max (loo, lpp) (roo, rpp)
  | loo > roo = (loo, lpp)
  | loo == roo && lpp > rpp = (loo, lpp)
  | loo == roo && lpp == rpp = (loo, lpp)
  | otherwise = oopp_max (roo, rpp) (loo, lpp)

oopp_oo :: OOPP -> Int
oopp_oo (oo, _) = oo

oopp_pp :: OOPP -> Int
oopp_pp (_, pp) = pp

oopp_str :: OOPP -> String
oopp_str (oo, pp) =
  (\(str1, str2, pad) -> (pad str1) ++ ":" ++ (pad str2))
    (show oo, show pp, (\str -> (replicate (2 - length str) '0') ++ str))

parse_oopp :: [Char] -> OOPP
parse_oopp str = (read (take 2 str) :: Int, read (drop 2 str) :: Int)

{- region Adat -}

type Adat = (String, OOPP, String, Int, Int)

adat_varos :: Adat -> String
adat_varos (varos, _, _, _, _) = varos

adat_oopp :: Adat -> OOPP
adat_oopp (_, oopp, _, _, _) = oopp

adat_szelirany :: Adat -> String
adat_szelirany (_, _, szelirany, _, _) = szelirany

adat_szelerosseg :: Adat -> Int
adat_szelerosseg (_, _, _, szelerosseg, _) = szelerosseg

adat_homerseklet :: Adat -> Int
adat_homerseklet (_, _, _, _, homerseklet) = homerseklet