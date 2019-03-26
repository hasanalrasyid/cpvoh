
module CPVO.Data where

import Numeric.LinearAlgebra.Data
import Data.Text

newtype Cart = Cart { fromCart :: Vector Double}
  deriving Read

data Atom = ZeroAtom | Atom { numberNZA :: Integer,
                    totalNA :: Int,
                    valenceElectronZV :: Double,
                    flareRadiusRCMAX :: Double,
                    massPMASS :: Double,
                    radiusRATS :: Double,
                    radiusRATS1 :: Double
                  } deriving (Read)

data Coord = ErrCoord | Coord { rest :: Text
                   , toCart :: Cart
                   } deriving (Read)

data Atoms = ErrAtoms | Atoms { atomSpec :: Atom,  positions :: [Coord]
                   } deriving (Read)

data Crystal = ErrCrystal | Crystal { bravType :: Int,
                         celldm :: (Double,Double,Double,Double,Double,Double),
                         translatVector :: Matrix Double,
                         translatVectorR :: Matrix Double,
                         atomList :: [Atoms] } deriving (Read)

type Celldm = (Double,Double,Double,Double,Double,Double)
