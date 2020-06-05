#!/usr/bin/env stack
--stack --resolver lts-14.27 --install-ghc runghc --stack-yaml /home/aku/kanazawa/dev/cpvoh/stack.yaml

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Main where

import           Options.Applicative
import Data.Semigroup ((<>))
import Data.List.Split (splitOn, chunksOf, wordsBy)
import Language.Fortran.Parser.Utils (readReal)
import Data.Maybe (fromJust,fromMaybe)
import Linear.V3
import Linear.Vector ((*^))
import Text.Printf (printf)
import Data.List (isInfixOf,findIndex,intercalate,group)
import System.Directory (doesFileExist)
import Linear.Matrix -- inv33, M33
import CPVO.IO
import CPVO.Data.Type
import qualified Data.Attoparsec.Text as A
import qualified Data.Text as T
import Numeric.LinearAlgebra hiding ((<>))
import Numeric.LinearAlgebra.Data
import Data.Semigroup
import System.IO (hPutStrLn,stderr)
deg2rad deg = deg * pi / 180

v3fromList :: [Double] -> V3 Double
v3fromList (a:b:c:_) = V3 a b c
v3fromList _ = V3 999 999 999

main :: IO ()
main = do
  opts <- execParser withHelp
  bOK <- doesFileExist $ _inCIF opts
  if bOK then genPOSCAR opts
         else do
           putStrLn "Error... input needed"
           putStrLn "genSurface.hs -i fort.19 -c celldm0 -s 2x2x1"
             {-
-- new lattice vector should be in a, and real space lattice vector
-- in case of CPVO, this is IBRAV 2
-- [  0   a/2 a/2
--    a/2  0  a/2
--    a/2 a/2 0   ]
-- so we should input "0.0 0.5 0.5 0.5 0 0.5 0.5 0.5 0"
-- with a = 8.114 for example
 (3><3)
 [ 8.114,   0.0,   0.0
 ,   0.0, 8.114,   0.0
 ,   0.0,   0.0, 8.114 ]

-}

type BasisVector = Matrix Double

replace old new = intercalate new . splitOn old

genPOSCAR opts = do
--  putStrLn "genPOSCAR"
  (ibrav:cell_a:_) <- fmap ((map getReal) . words) $ readFile $ _inCellDM0 opts
  inCIF0 <- readFile $ _inCIF opts
  let vNew1  = fromList $ map getReal $ splitOn ":" $ fromMaybe "0.0:0.0:0.0" $ _vectorNew opts
  let forceV = fromList $ map getReal $ splitOn ":" $ fromMaybe "0.0:0.0:0.0" $ _forceVec opts
  let vNew = vNew1 + forceV
  hPutStrLn stderr $ show forceV
  let inCIF = replace "${dynVec}" (dispv vNew) inCIF0
  sPoscar1 <- readProcess "cif2poscar.pyx" ["c","d"] inCIF
  --                                        |   +- direct not cartesian
  --                                        +- full cell not p primitive
  let (Right crystalCell) = A.parseOnly fileParser $ T.pack sPoscar1
  hPutStrLn stderr $ show opts
  let newBasis =
        if _fullCell opts then
                            case _newBasis opts of
                              Just s -> (scale cell_a . tr' . fromLists . chunksOf 3 . map getReal . words) s
                              Nothing -> translatVector crystalCell
                          else translatVector crystalCell
--  putStrLn $ "newBasis: " ++ show newBasis
--  putStrLn $ show crystalCell
--  hLine
  hPutStrLn stderr $ "newBasis: " ++ show newBasis
  hPutStrLn stderr $ "crystalCell:" ++ showCoords crystalCell
  let target = map getReal $ splitOn "x" $ _inSize opts :: [Double]
      doubleSizeInt  = map ceiling $ map (\x -> x*2-1 ) target :: [Integer]
      doubleSize@(d1:d2:d3:_)  = map fromIntegral doubleSizeInt :: [Double]
      expander = [ fromList [x,y,z] | x <- [-d1..d1]
                                    , y <- [-d2..d2]
                                    , z <- [-d3..d3]
                 ] :: [Vector Double]
--  let expander = [fromList [i,j,k] | i <- [0..1], j <- [0..1], k <- [0..1] ] :: [Vector Double]
--  let crystal1 = applyNewPos ( + fromList [1,1,1]) crystalCell
  --let css = foldr (<>) crystalCell $ map (\x -> applyNewPos (+ x) crystalCell) expander
  let expandedCrystal = expand crystalCell expander
--  putStrLn $ showCoords $ expandedCrystal
--when we want to generate coordinates from this ...
--putStrLn $ show $ (<>) (fromRows fracCart) $ translatVector crystalCell
--steps to generate new cell:
--1. expand the crystal into oldXYZ=[-1..1]
--  putStrLn $ showCoords expandedCrystal
--2. generate cartesian from 1
--  putStrLn $ show $ length $ cartesian expandedCrystal
--3. generate new fract coord from new basis vector
--    Xf = Xc * (inv B)
  let mVector = fromList $ map getReal $ words $ _moveCoord opts
--  putStrLn $ show mVector
  let frac' = (fromRows $ map (\x -> x - mVector) $ cartesian expandedCrystal) <> (inv newBasis)
--4. filter 3 for under 1
  let newAtoms = filter under1 $ zip (getAtom expandedCrystal) $ map toList $ toRows frac'
--5. generate cartesian from 4
      newCrystal = crystalCell { translatVector = newBasis
                               , atomList = map genAtom' $ group newAtoms
                               }
  hPutStrLn stderr $ "newCrystal:" ++ showCoords newCrystal
--  putStrLns (\(a,(b,c)) -> unwords [show a, show b, dispv c])
--    $ zip [1..] $ atomCoord fractional newCrystal
--  putStrLn $ dispf 6 $ translatVector newCrystal
--  putStrLns (\(a,(b,c)) -> unwords [show a, show b, dispv c])
--    $ zip [1..] $ atomCoord cartesian newCrystal
--  hLine
--  putStrLn $ dispf 6 $ (fromRows $ cartesian newCrystal) <> (inv $ scale cell_a $ diagl [1,1,1])
--  putStrLns show $ getAtom newCrystal

  site <- genSITE cell_a newCrystal $ _atomString opts
  ctrlFile <- case _ctrlTemplate opts of
                Nothing -> return ""
                Just o -> readFile o
  let ctrl = replace "#${site}" (unlines site) ctrlFile
--  putStrLn ctrl
  putStrLns id site
--  putStrLn "!genPOSCAR"

-- genSITE should only do creating showcase
genSITE cell_a c ats = do
  let p = toRows $ (fromRows $ fractional c) -- <> (inv $ scale cell_a $ diagl [1,1,1])
  let a = case ats of
            Nothing -> map (\(AtomSymbol s) -> s) $ getAtom c
            Just s -> words s
      strSITE (aa,b) = unwords ["ATOM="++aa,"POS=",dispv b]
  putStrLn $ unwords $ "#":a
  return $ map strSITE $ zip a p

dispv v = unwords $ map (printf "%.6f") $ toList v
--dispv v = unwords $ map (\x -> printf "%.6f" x) $ toList v

putStrLns s = mapM_ (putStrLn . s)

genAtom' a@((at,_):_) = Atoms at $ map (\(_,x) -> Coord "" $ Cart $ fromList x) a
genAtom' _ = ErrAtoms

under1 (_, (v1:v2:v3:_))
  |    0 <= v1 && v1 < 1
    && 0 <= v2 && v2 < 1
    && 0 <= v3 && v3 < 1 = True
  | otherwise = False
under1 _ = False

hLine = putStrLn $ replicate 70 '='

class Structure a where
  cartesian  :: a -> [Vector Double]
  fractional :: a -> [Vector Double]
  getAtom    :: a -> [Atom]
  expand     :: a -> [Vector Double] -> a
  atomCoord  :: (a -> [Vector Double]) -> a -> [(Atom,Vector Double)]

instance Structure Crystal where -- Crystal have to be saved as fractional to real lattice
  cartesian  c@(Crystal _ _ tReal _ _  ) = toRows $ (getCoords c) <> tReal
  cartesian ErrCrystal = []
  fractional ErrCrystal = []
  fractional c                           = toRows $ getCoords c
  getAtom ErrCrystal = []
  getAtom    (Crystal _ _ _       _ atL) =
    let at = map atomSpec atL
        ct = map (length . positions) atL
     in concat $ zipWith replicate ct at
  expand c vs = foldr (\x -> (<>) (applyNewPos (+x) c)) (ErrCrystal) vs
  atomCoord f c =
    let ats = getAtom c
        cs  = f c
     in zip ats cs


showCoords x = show $ fromRows $ map fromCart $ concat $ map (map toCart . positions) $ atomList x
getCoords  x = fromRows $ map fromCart $ concat $ map (map toCart . positions) $ atomList x
getCoordsNAtoms x =
  let c  = map (map toCart . positions) $ atomList x
      cs = fromRows $ map fromCart $ concat c
      atomCounts = map length c
      atoms = concat $ zipWith (\a b -> replicate a b) atomCounts $ map atomSpec $ atomList x
   in atoms

-- getAtom    x = concat $ map (map toCart . positions) $ atomList x
instance Semigroup Crystal where
  (<>) :: Crystal -> Crystal -> Crystal
  (<>) ErrCrystal a = a
  (<>) a ErrCrystal = a
  (<>) a b = a {atomList = atomList a ++ atomList b}

instance Monoid Crystal where
  mempty = ErrCrystal
  mappend = (<>)

applyToAtoms f c =
  let as' = map f $ atomList c
   in c {atomList = as'}

applyNewPos :: (Vector Double -> Vector Double) -> Crystal -> Crystal
applyNewPos f c =
  let updateCoord _ ErrCoord = ErrCoord
      updateCoord g (Coord r (Cart t)) = Coord r $ Cart $ g t
      genPos (Atoms a ps) = Atoms a $ map (updateCoord f) ps
      genPos ErrAtoms = ErrAtoms
   in applyToAtoms genPos c

skipLine :: A.Parser ()
skipLine = A.skipWhile (not . A.isEndOfLine) >> A.endOfLine

{-
  In the direct mode the positions are given by

         {\vec R} = x_1 {\vec a}_1 + x_2 {\vec a}_2 + x_3 {\vec a}_3

      where $ {\vec a}_{1...3}$ are the three basis vectors,
        and $ x_{1...3}$ are the supplied values.
  In the cartesian mode the positions are only scaled by the factor $ s$
  on the second line of the POSCAR file.

  if we have matrix A as column vector of lattice vector a,
  and matrix X as row vector of coordinate x,
  then we have:
  cartCord = X A

-}

fileParser :: A.Parser Crystal
fileParser = do
  skipLine
  latParam <- A.double
  latVec <- A.count 3 parseVec
  atSym' <- parseAtSym
  atCount <- parseAtCount
  let atSym = concat $ zipWith replicate atCount atSym'
  skipLine
  latCoord <- A.count (length atSym) parseVec
  return $ ( Crystal { bravType = 0
                     , celldm = LatConst latParam
                     , translatVector = fromColumns latVec
                     , translatVectorReciprocal = inv $ fromColumns latVec
                     , atomList = map genAtom $ group $ zip atSym latCoord
                     }
           )

genCoord s = Coord "" $ Cart s
genAtom as@((s,_):_) = Atoms (AtomSymbol s) (map (genCoord . snd) as)
genAtom _ = ErrAtoms

parseAtCount :: A.Parser [Int]
parseAtCount = do
  ls <- A.manyTill takeInt A.endOfLine
  return ls
    where
      takeInt = do
        A.try A.skipSpace
        d <- A.decimal
        return d

parseAtSym :: A.Parser [String]
parseAtSym = do
  ls <- A.manyTill A.anyChar A.endOfLine
  return $ words ls

parseVec :: A.Parser (Vector Double)
parseVec = do
  A.try A.skipSpace
  d1 <- A.double
  A.skipSpace
  d2 <- A.double
  A.skipSpace
  d3 <- A.double
  skipLine
  return $ fromList [d1,d2,d3]


main1 :: IO ()
main1 = do
    opts <- execParser withHelp
    bOK <- doesFileExist $ _inFort19CPVO opts
    if bOK then genPrimitive opts
           else do
             putStrLn "Error... input needed"
             putStrLn "genSurface.hs -i fort.19 -c celldm0 -s 2x2x1"

genPrimitive opts = do
  fFort19 <- readFile $ _inFort19CPVO opts
  fCellDM <- readFile $ _inCellDM0 opts
  let allSize@(s1:s2:s3:_)  = map (((flip (-)) 1) . floor . getReal)
                    $ splitOn "x" $ _inSize opts
  let (ibrav:cell_a:_) = map getReal $ words fCellDM
      fort19 = getAtomCoords [] $ lines fFort19
  let mLatVec = genLatticeVector ibrav [cell_a]
      target =[ (x,y,z) | x <- [0..s1]
                        , y <- [0..s2]
                        , z <- [0..s3]
              ]
  let h = concat $ map (reverse . genMirror target mLatVec [] ) fort19
  putStrLn $ unlines $ filter (not . null) $ concat $ map lines h
  let xnel = genXNEL 0 $ map words $ filter (isInfixOf "IS") h
  putStrLn $ "XNEL " ++ show xnel
  putStrLn $ show (floor $ xnel/2 :: Integer) ++ "*1.0"
  putStrLn $ "allSize = " ++ show (map (+1) allSize)
  let newCell_a = cell_a * fromIntegral (1 + maximum allSize)
  putStrLn $ "cell_a * maxSize = " ++ show newCell_a
  putStrLn $ unwords [show ibrav, show newCell_a, unwords $ drop 2 $ words fCellDM]

genXNEL res [] = res
genXNEL res ((_:sAts:sZV:_):ls) =
  genXNEL ((+) res $ foldr (*) 1.0 $ map getReal [sAts,sZV]) ls
genXNEL _ _ = -1

showVec :: Int -> V3 Double -> String
showVec n (V3 a b c) = unwords $ map (printf (concat ["  %.",show n,"f"])) [a,b,c]

genMirror _ _ r [] = r
genMirror t m res (a:cs)
  | (isInfixOf "IS" a) = let
                          (noAt:_:sisa) = words a
                          numAts = (length t * length cs)
                          newA = unwords (noAt:show numAts:sisa)
                          in genMirror t m (newA:res) cs

  | otherwise = let
                  r' = map (showVec 6)
                     $ map (genMirrorFromSingle m (v3fromLine a)) t
                  ender = ("  " ++) $ unwords $ drop 3 $ words a
                  r  = unlines $ map (++ ender) r'
                 in genMirror t m (r:res) cs

genMirrorFromSingle :: M33 Double -> V3 Double -> (Integer,Integer,Integer)
                    -> V3 Double
genMirrorFromSingle m@(V3 v1 v2 v3) l (n1,n2,n3) =
  foldr (+) l $ zipWith calcMove [v1,v2,v3] [n1,n2,n3]
    where
      calcMove v' n' = fromIntegral n' *^ v'

getAtomCoords res [] = res
getAtomCoords res xa@(x:xs) =
  let (r1,rs) = case (findIndex (isInfixOf "IS") xs) of
                  Just a -> splitAt (a+1) xa
                  Nothing -> (xa,[])
  in getAtomCoords (r1:res) rs

genLatticeVector :: Double -> [Double] -> M33 Double
genLatticeVector 2 (a:_) = (a/2) *!! V3 (V3 0.0 1.0 1.0)
                                        (V3 1.0 0.0 1.0)
                                        (V3 1.0 1.0 0.0)

genLatticeVector _ _ = identity -- unimplemented term

getReal :: String -> Double
getReal s = case readReal s of
              Just x -> x
              _ -> 0

v3fromLine l = v3fromList $ map (fromJust . readReal) $ take 3 $ words l

data Opts = Opts {
    _inCIF :: FilePath,
    _inFort19CPVO :: FilePath,
    _inCellDM0 :: FilePath,
    _inSize :: String,
    _moveCoord :: String,
    _zeroAtom :: Maybe String,
    _ctrlTemplate :: Maybe String,
    _vectorNew :: Maybe String,
    _forceVec :: Maybe String,
    _atomString :: Maybe String,
    _newBasis :: Maybe String,
    _fullCell :: Bool
                 } deriving Show

optsParser :: Parser Opts
optsParser = Opts
             <$> strOption (long "input-CIF" <> short 'i' <> metavar "CIF"
                            <> help "file input from CIF file, can be acquired from CIF database")
             <*> strOption (long "input-cpvo" <> short 'u' <> metavar "FORT19"
                            <> help "file input from fort.19 in CPVO run, usually came out after opts" <> value "fort.19")
             <*> strOption (long "input-celldm0" <> short 'c' <> metavar "CELLDM0"
                            <> help "file input from celldm0 in CPVO run, usually defined as an input" <> value "celldm0")
             <*> strOption (long "input-size" <> short 's' <> metavar "N1xN2xN3"
                            <> help "Expansion size of the supercell, based on new basis vectors: 2x2x1" <> value "1x1x1")
             <*> strOption (long "move" <> short 'm' <> metavar "X,Y,Z"
                            <> help "Move all crystal by m:(x y z), following c' = c - m" <> value "0,0,0" )
             <*> (optional $
                 strOption (long "atom-zero" <> short 'z' <> metavar "ATINDEX"
                            <> help "atom index that become init coordinate" )
                 )
             <*> (optional $
                 strOption (long "ctrl-template" <> short 't' <> metavar "CTRLTEMPLATE"
                            <> help "template for ctrl of ecalj/lm7k" )
                 )
             <*> (optional $
                 strOption (long "vector-update" <> short 'v' <> metavar "VECTORUPDATE"
                            <> help "vector value of CIF template" )
                 )
             <*> (optional $
                 strOption (long "force" <> short 'f' <> metavar "FORCEVECTOR"
                            <> help "force to apply upon dynVec" )
                 )
             <*> (optional $
                 strOption (long "atom-name" <> short 's' <> metavar "ATNAME"
                            <> help "atom strings to be pasted as ATOM=a " )
                 )
             <*> (optional $
                 strOption (long "new-basis-vector" <> short 'b' <> metavar "Va Vb Vc"
                            <> help "new real cart basis vector, read in row based" )
                 )
             <*> switch (long "full-cell" <> short 'l' <> help "generate Full cell, in case of IBRAV=2, then generate for cubic cell instead of primitive cell" )

withHelp :: ParserInfo Opts
withHelp = info
       (helper <*> optsParser)
       (fullDesc <> progDesc "interpret inline Haskell code to insert images in Pandoc output\nhttps://github.com/bergey/diagrams-pandoc"
       <> header "diagrams-pandoc - a Pandoc filter for inline Diagrams")

