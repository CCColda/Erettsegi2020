module Main where

import Adat
import Data.List (findIndex, nub)
import System.IO (hFlush, stdout)
import System.Environment (getArgs)

{-- feloszt egy szöveget adott karakterenként --}
string_split :: Char -> String -> [String]
string_split _ "" = []
string_split separator string =
  maybe [string] (\index -> (take index string : string_split separator (drop (index + 1) string))) (findIndex (\c -> c == separator) string)

{-- értelmez egy bemeneti adatsort --}
ertelmez_adat :: String -> Adat
ertelmez_adat line =
  ( \szavak ->
      ( szavak !! 0 :: String,
        parse_oopp (szavak !! 1),
        take 3 (szavak !! 2) :: String,
        read (drop 3 (szavak !! 2)) :: Int,
        read (szavak !! 3) :: Int
      )
  )
    (string_split ' ' line)

{-- visszatér egy város utolsó mérési adatának idejére --}
utolso_meresi_adat :: [Adat] -> String -> OOPP
utolso_meresi_adat [] _ = (-1, -1)
utolso_meresi_adat [(adatvaros, oopp, _, _, _)] varos
  | adatvaros == varos = oopp
  | otherwise = (-1, -1)
utolso_meresi_adat (adat : adatbazis) varos =
  oopp_max (utolso_meresi_adat [adat] varos) (utolso_meresi_adat adatbazis varos)

{-- visszatér a napi legmagasabb hőmérsékletre --}
napi_hi :: [Adat] -> Adat
napi_hi [adat] = adat
napi_hi [adat1, adat2]
  | adat_homerseklet adat1 > adat_homerseklet adat2 = adat1
  | otherwise = adat2
napi_hi (adat : adatbazis) = napi_hi [adat, (napi_hi adatbazis)]

{-- visszatér a napi legalacsonyabb hőmérsékletre --}
napi_lo :: [Adat] -> Adat
napi_lo [adat] = adat
napi_lo [adat1, adat2]
  | adat_homerseklet adat1 < adat_homerseklet adat2 = adat1
  | otherwise = adat2
napi_lo (adat : adatbazis) = napi_lo [adat, (napi_lo adatbazis)]

{-- visszatér az adatok közül a legmagasabb és legalacsonyabb hőmérsékletre szövegként --}
napi_hilo :: [Adat] -> String
napi_hilo adatbazis =
  (\(hi, lo, adat_str) -> "Legmagasabb hőmérséklet: " ++ adat_str hi ++ "\nLegalacsonyabb hőmérséklet: " ++ adat_str lo)
    ( napi_hi adatbazis,
      napi_lo adatbazis,
      ( \adat ->
          adat_varos adat ++ " "
            ++ oopp_str (adat_oopp adat)
            ++ " "
            ++ show (adat_homerseklet adat)
            ++ " fok."
      )
    )

{-- visszatér a szélcsendes helyek adataira szövegként --}
szelcsend_hely_ido :: [Adat] -> String
szelcsend_hely_ido adatbazis =
  ( \szelcsendes ->
      case (length szelcsendes) of
        0 -> "Nem volt szélcsend a mérések idején"
        _ -> concat (map (\adat -> adat_varos adat ++ " " ++ oopp_str (adat_oopp adat) ++ "\n") szelcsendes)
  )
    (filter (\adat -> adat_szelirany adat == "000" && adat_szelerosseg adat == 0) adatbazis)

{-- visszatér az összhőmérsékletre, a mért órákra, és a mérések számára --}
osszhomerseklet :: [Adat] -> [Int] -> ([Int], Int, Float {- [hour], count homerseklet -})
osszhomerseklet [] _ = ([], 0, 0.0)
osszhomerseklet [adat] szukseges_orak
  | any (== oopp_oo (adat_oopp adat)) szukseges_orak = ([oopp_oo (adat_oopp adat)], 1, fromIntegral (adat_homerseklet adat) :: Float)
  | otherwise = ([], 0, 0.0)
osszhomerseklet (adat : adatbazis) szukseges_orak =
  (\((hours1, count1, hom1), (hours2, count2, hom2)) -> (nub (hours1 ++ hours2), count1 + count2, hom1 + hom2))
    ((osszhomerseklet [adat] szukseges_orak), (osszhomerseklet adatbazis szukseges_orak))

{-- visszatér a kiszámolt középhőmérsékletre szövegként --}
kozephomerseklet_szoveg :: ([Int], Int, Float) -> Int -> String
kozephomerseklet_szoveg (hours, count, homerseklet) szukseges_ora_szam
  | (length hours) == szukseges_ora_szam =
    "Középhőmérséklet " ++ show (round ((homerseklet / (fromInteger (toInteger count)) :: Float)))
  | otherwise = "NA"

{-- visszatér az adatok középhőmérsékletére és hőmérsékletingadozására szövegként --}
homerseklet_adat_varos :: [Adat] -> [Int] -> String
homerseklet_adat_varos adatbazis szukseges_orak =
  kozephomerseklet_szoveg (osszhomerseklet adatbazis szukseges_orak) (length szukseges_orak)
    ++ "; Hőmérséklet-ingadozás: "
    ++ show (abs ((adat_homerseklet (napi_hi adatbazis)) - (adat_homerseklet (napi_lo adatbazis))))

{-- visszatér a kiszámolt hőmérsékleti adatokra szövegként --}
homerseklet_adat :: [Adat] -> [Int] -> String
homerseklet_adat adatbazis szukseges_orak =
  ( \varos_kodok ->
      concat
        ( map
            ( \varos_kod ->
                varos_kod ++ " "
                  ++ homerseklet_adat_varos (filter (\adat -> (adat_varos adat) == varos_kod) adatbazis) szukseges_orak
                  ++ "\n"
            )
            varos_kodok
        )
  )
    (nub (map adat_varos adatbazis))

{-- visszatér a megfelelő fájladatokra (szélerősségnek megfelelő #) --}
hatos_fajladatok :: [Adat] -> [(String, String)]
hatos_fajladatok adatbazis =
  map
    (\varos_kod -> (varos_kod, varos_kod ++ "\n" ++ concat (map (\meres -> (oopp_str (adat_oopp meres)) ++ " " ++ (replicate (adat_szelerosseg meres) '#') ++ "\n") (filter (\adat -> adat_varos adat == varos_kod) adatbazis))))
    (nub (map adat_varos adatbazis))

{-- kiíratja az összes feldolgozott adatot a hozzá tartozó fájlba --}
hatos :: [Adat] -> String -> IO ()
hatos adatbazis mappa =
  mapM_
    ( \(varos_kod, fajladat) ->
        writeFile
          (mappa ++ "/" ++ varos_kod ++ ".txt")
          fajladat
    )
    (hatos_fajladatok adatbazis)

{-- kiíratja `prompt`-ot, majd visszatér a válaszra --}
input :: String -> IO String
input prompt = do
    putStr prompt
    hFlush stdout
    getLine

{-- usage: Erettsegi2020 [adatbazis_utvonal] [kimeneti_mappa] --}
main :: IO ()
main = do
  args <- getArgs

  let adatbazis_utvonal = (case (length args > 0) of
        False -> "tavirathu13.txt"
        True -> args !! 0)

  let kimeneti_mappa = (case (length args > 1) of
        False -> "hs_adat"
        True -> args !! 1)

  {-- fájl beolvasása, adatbázissá alakítása --}
  file <- readFile adatbazis_utvonal :: IO String
  let adatbazis = map ertelmez_adat (string_split '\n' file) :: [Adat]

  putStrLn ("2. feladat")

  varos <- input "Adja meg a település kódját! Település: " :: IO String
  putStrLn (varos ++ " város utolsó mérési adata: " ++ oopp_str (utolso_meresi_adat adatbazis varos))

  putStrLn ("3. feladat")
  putStrLn (napi_hilo adatbazis)

  putStrLn ("4. feladat")
  putStrLn (szelcsend_hely_ido adatbazis)

  putStrLn ("5. feladat")
  putStrLn (homerseklet_adat adatbazis [1, 7, 13, 19])

  hatos adatbazis kimeneti_mappa
  putStrLn ("A fájlok elkészültek.")