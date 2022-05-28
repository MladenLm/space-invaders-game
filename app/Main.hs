module Main where

import Animation
    ( Animation
    , Env(..)
    , St(..)
    , defaultEnv
    , defaultSt
    , next
    , render
    , runAnimation
    )
  
import Control.Concurrent (threadDelay)
import System.Random (randomRIO)
import Data.List (nubBy)
import Control.Monad.Trans.State.Strict (put, get)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import Animation.Type (GameStatus(..))

{-
putInitialState :: Animation Env St ()
putInitialState = do
    (Env (width, height) _ baselength bricklength _ _ lifes) <- ask
    posX <- lift $ lift $ randomRIO (div width  3, (*) 2 $ div width  3)
    posY <- lift $ lift $ randomRIO (div height 3, (*) 2 $ div height 3)
    dirX <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
    dirY <- lift $ lift $ fmap directionFromInt $ randomRIO (1, 2)
  
 -- | Creation of a random number of blocks limited by a desired maximum number.
  
    randNumBlocks  <- let maxDesiredBlocks = div (width * (height - 4)) (bricklength * 4) in lift $ lift $ randomRIO (0, maxDesiredBlocks)
  
 -- | Creation of a random number of blocks limited by a desired maximum number.

    randNumBlocks  <- let maxDesiredBlocks = div (width * (height - 4)) (bricklength * 4) in lift $ lift $ randomRIO (0, maxDesiredBlocks)

 -- | Creation of a list of DIFFERENT positions. The range of available positions has to be divided by the bricklength so
 -- | we can introduce the bricklength space afterwards in order to get bricks not to overlap

    randRedDistBlocks <- (fmap (nubBy (==)) $ sequence $ replicate randNumBlocks $ randomRIO (1, div (width * (height - 4)) bricklength :: Int))
    
    lift $ put $ St [(posX, posY)] (dirX, dirY) (div (width - baselength) 2) (bricksInPlace width randRedDistBlocks lifes bricklength) 0 Paused

-}
 -- | Management of the animation. Restarted state interrupts it.
 
animate :: Animation Env St ()
animate = do
    render
    event <- lift get
    --case (status event) of
    --    Restarted -> putInitialState
    --    _         -> next
    next
    lift $ lift $ threadDelay 1000000
    animate

mainAnimation :: Animation Env St ()
mainAnimation = do
    --putInitialState
    animate

main :: IO ()
main = do
    runAnimation defaultEnv defaultSt mainAnimation
