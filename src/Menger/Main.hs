module Main(main) where


import           Linear.Quaternion (Quaternion (..), axisAngle)
import           Linear.V3         (V3 (..))
import           Vis               (Antialiasing (..), Camera0 (..),
                                    Flavour (..), Options (..), VisObject (..),
                                    visMovie)
import           Vis.GlossColor    (Color (..), makeColor, aquamarine)

options :: Options
options = Options
    (Just $ makeColor 0.1 0.1 0.2 1.0)
    (Just $ (1024, 1024))
    (Just $ (450, 30))
    "Menger sponge"
    (Just $ Camera0 0.0 30.0 25.0)
    Smoothed

filePaths :: Int -> FilePath
filePaths i = "out/frame_" ++ showInt i 3 ++ ".bmp"

showInt :: Int -> Int -> String
showInt i n
    | i < 10     = replicate (n - 1) '0' ++ show i
    | i > 10 ^ n = error "Cannot show number " ++ show i ++ " with only " ++ show n ++ " chars."
    | otherwise  = showInt (div i 10) (n - 1) ++ show (mod i 10)

sampleRate :: Double
sampleRate = 0.5

main :: IO ()
main = visMovie
    options
    filePaths
    sampleRate
    frames
    Nothing

frames :: [VisObject Double]
frames = map (menger 10) $ [3]

cheapMenger :: Double -> Int -> VisObject Double
cheapMenger size 0     = Cube size Solid aquamarine
cheapMenger size depth = VisObjects [
        Trans (V3 s    s    0.0 ) $ cheapMenger s (depth - 1),
        Trans (V3 (-s) s    0.0 ) $ cheapMenger s (depth - 1),
        Trans (V3 s    (-s) 0.0 ) $ cheapMenger s (depth - 1),
        Trans (V3 (-s) (-s) 0.0 ) $ cheapMenger s 0,

        Trans (V3 s    0.0  s   ) $ cheapMenger s (depth - 1),
        Trans (V3 (-s) 0.0  s   ) $ cheapMenger s (depth - 1),
        Trans (V3 s    0.0  (-s)) $ cheapMenger s (depth - 1),
        Trans (V3 (-s) 0.0  (-s)) $ cheapMenger s 0,

        Trans (V3 0.0  s    s   ) $ cheapMenger s (depth - 1),
        Trans (V3 0.0  (-s) s   ) $ cheapMenger s (depth - 1),
        Trans (V3 0.0  s    (-s)) $ cheapMenger s (depth - 1),
        Trans (V3 0.0  (-s) (-s)) $ cheapMenger s 0,

        Trans (V3 s    s    s   ) $ cheapMenger s (depth - 1),
        Trans (V3 s    s    (-s)) $ cheapMenger s (depth - 1),
        Trans (V3 s    (-s) s   ) $ cheapMenger s (depth - 1),
        Trans (V3 s    (-s) (-s)) $ cheapMenger s (depth - 1),

        Trans (V3 (-s) s    s   ) $ cheapMenger s (depth - 1),
        Trans (V3 (-s) s    (-s)) $ cheapMenger s (depth - 1),
        Trans (V3 (-s) (-s) s   ) $ cheapMenger s (depth - 1),
        Trans (V3 (-s) (-s) (-s)) $ cheapMenger s 0
    ]
    where
        s :: Double
        s = size / 3

menger :: Double -> Int -> VisObject Double
menger size 0     = Cube size Solid aquamarine
menger size depth = VisObjects [
        Trans (V3 s    s    0.0 ) nextMenger,
        Trans (V3 (-s) s    0.0 ) nextMenger,
        Trans (V3 s    (-s) 0.0 ) nextMenger,
        Trans (V3 (-s) (-s) 0.0 ) nextMenger,

        Trans (V3 s    0.0  s   ) nextMenger,
        Trans (V3 (-s) 0.0  s   ) nextMenger,
        Trans (V3 s    0.0  (-s)) nextMenger,
        Trans (V3 (-s) 0.0  (-s)) nextMenger,

        Trans (V3 0.0  s    s   ) nextMenger,
        Trans (V3 0.0  (-s) s   ) nextMenger,
        Trans (V3 0.0  s    (-s)) nextMenger,
        Trans (V3 0.0  (-s) (-s)) nextMenger,

        Trans (V3 s    s    s   ) nextMenger,
        Trans (V3 s    s    (-s)) nextMenger,
        Trans (V3 s    (-s) s   ) nextMenger,
        Trans (V3 s    (-s) (-s)) nextMenger,

        Trans (V3 (-s) s    s   ) nextMenger,
        Trans (V3 (-s) s    (-s)) nextMenger,
        Trans (V3 (-s) (-s) s   ) nextMenger,
        Trans (V3 (-s) (-s) (-s)) nextMenger
    ]
    where
        nextMenger :: VisObject Double
        nextMenger = menger s (depth - 1)

        s :: Double
        s = size / 3
