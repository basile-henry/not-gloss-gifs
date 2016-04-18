module Main(main) where

import           Linear.Quaternion (Quaternion (..), axisAngle)
import           Linear.V3         (V3 (..))
import           Vis               (Antialiasing (..), Camera0 (..),
                                    Flavour (..), Options (..), VisObject (..),
                                    visMovie)
import           Vis.GlossColor    (Color (..), makeColor, azure)

data Ball  = Ball {
        n        :: Int,
        position :: V3 Double,
        angle    :: Double,
        color    :: Color,
        size     :: Double
    }

data Scene = Scene {
        scene_angle :: Double,
        balls       :: [Ball]
    }

options :: Options
options = Options
    (Just $ makeColor 0.1 0.1 0.2 1.0)
    (Just $ (1024, 512))
    (Just $ (150, 100))
    "Rotating"
    (Just $ Camera0 0.0 30.0 30.0)
    (Multisampled 16)

filePaths :: Int -> FilePath
filePaths i = "out/frame_" ++ showInt i 3 ++ ".bmp"

showInt :: Int -> Int -> String
showInt i n
    | i < 10     = replicate (n - 1) '0' ++ show i
    | i > 10 ^ n = error "Cannot show number " ++ show i ++ " with only " ++ show n ++ " chars."
    | otherwise  = showInt (div i 10) (n - 1) ++ show (mod i 10)

sampleRate :: Double
sampleRate = 0.016

numFrames :: Int
numFrames = 800

numBalls :: Int
numBalls = 30

main :: IO ()
main = visMovie
    options
    filePaths
    sampleRate
    frames
    Nothing

deltas :: [Double]
deltas = replicate (numFrames - 1) $ 1.0 / (fromIntegral numFrames)

deltaTimes :: [(Double, Double)]
deltaTimes = zip deltas $ scanl (+) 0.0 deltas

frames :: [VisObject Double]
frames = map drawScene $ scanl updateScene startScene deltaTimes

startScene :: Scene
startScene = Scene 0.0 [
        Ball
            x
            (V3 (fromIntegral $ x - div numBalls 2) 1.0 0.0)
            0.0
            azure --(makeColor 0.8 0.6 0.1 1.0)
            startSize
        | x <- [1..numBalls]
    ]

startSize :: Double
startSize = 0.15

updateScene :: Scene -> (Double, Double) -> Scene
updateScene (Scene angle balls) (dt, t) = Scene (angle + 4 * pi * dt) $ map (`updateBall` (dt, t)) balls

updateBall :: Ball -> (Double, Double) -> Ball
updateBall (Ball n pos angle color size) (dt, t) = Ball n pos newAngle color size --newSize
    where
        newAngle :: Double
        newAngle = angle + fromIntegral (n * 2) * pi * dt

        -- newSize :: Double
        -- newSize = startSize * (1 + 5 * sin (pi * t) * (fromIntegral (numBalls - n) / fromIntegral numBalls))

drawScene :: Scene -> VisObject Double
drawScene (Scene angle balls) = Trans (V3 0.0 0.0 (-3.0))
    . rotateZ angle
    . VisObjects
    . map drawBall $ balls

drawBall :: Ball -> VisObject Double
drawBall (Ball _ pos angle color size) = rotateX angle $ Trans pos $ Sphere size Solid color

rotateX :: Double -> VisObject Double -> VisObject Double
rotateX = rotate (V3 1.0 0.0 0.0)

rotateZ :: Double -> VisObject Double -> VisObject Double
rotateZ = rotate (V3 0.0 0.0 1.0)

rotate :: V3 Double -> Double -> VisObject Double -> VisObject Double
rotate vector angle = RotQuat $ axisAngle vector angle
