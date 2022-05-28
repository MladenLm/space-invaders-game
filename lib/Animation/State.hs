module Animation.State where

import Control.Monad.Trans.State.Strict (get, put)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)
import System.IO (hReady, Handle(..), stdin, hSetEcho, hSetBuffering, BufferMode(NoBuffering))

import Animation.Env (Env(..))
import Animation.Type ( Animation
                      , Brick(..)
                      , GameStatus(..)
                      , UserInput(..)
                      )


-- directionFromInt :: Int -> Direction
-- directionFromInt 0 = Neutral
-- directionFromInt 1 = Positive
-- directionFromInt 2 = Negative
-- directionFromInt _ = error "Boooooo....."

-- directionToMultiplier :: Direction -> Int
-- directionToMultiplier Positive =  1
-- directionToMultiplier Negative = -1
-- directionToMultiplier Neutral  =  0

data St =
    St
        { position     :: [(Int, Int)]
        , bXPosition   :: Int
        , bricks       :: [Brick]
        , points       :: Int
        , status       :: GameStatus
        }

-- | Allocation of the list of reduced positions in the game
-- | A reduced position is a 'x' value divided by the brick length
-- | Positions in this function are a list of 'x positions'. This means that
-- | given width = 50 then positions 49, 50, 51, 52,... correspond to points (49,0), (50,0), (1,1), (2,1),...
{-
bricksInPlace :: Int -> [Int] -> Int -> Int -> [Brick]
bricksInPlace width positions life bricklength = map (\x -> (findPosition (bricklength*x) width 0) life) positions
           where findPosition x width level = if x < width then (x,level) else findPosition (x - width) width (level + 1)
-}

defaultSt :: St
defaultSt = St [(0, 0), (1,2), (3,4)] 0 [(5,5), (5,6), (7,1)] 0 Playing          

-- | Management of the input of the user in a handy way. Pendant to solve reaction delay when holding key.

getKey :: IO [Char]
getKey = reverse <$> getKey' ""
  where getKey' chars = do
          char <- getChar
          more <- hReady stdin
          (if more then getKey' else return) (char:chars)

getUserInput :: IO (Maybe UserInput)
getUserInput = go Nothing
        where go a = do
                hSetBuffering stdin NoBuffering 
                hSetEcho stdin False
                ready <- hReady stdin
                if not ready then return Nothing
                else do
                      hSetBuffering stdin NoBuffering
                      string <- getKey
                      let condition = (==) (head string) in
                             if condition 'a' || condition 'A'                  then return (Just MoveLeft)
                        else if condition 'd' || condition 'D'                  then return (Just MoveRight)
                        else if condition 'p' || condition 'P'                  then return (Just Pause)
                        else if condition 'q' || condition 'Q'                  then return (Just Stop)
                        else if condition 's' || condition 'S'                  then return (Just Start)
                        else if condition 'r' || condition 'R'                  then return (Just Restart)
                        else if condition ' '                                   then return (Just Shoot)
                        else return Nothing

next :: Animation Env St ()
next = do
     env    <- ask
     input  <- lift $ lift getUserInput
     prevSt <- lift get
     lift (put (nextInternal env input prevSt))

nextInternal :: Env -> Maybe UserInput -> St -> St
nextInternal (Env (width, height) velocity baselength bricklength _ _ _ ) 
             userInput 
             prevSt@(St prevPositions prevBXPos prevBricks prevPoints prevStatus)
             =
    
 -- | Management of next state according to GameStatus and UserInput
   
    case prevStatus of
        Paused        -> case userInput of
                           Just Start -> prevSt {status = Playing}
                           Just Stop  -> prevSt {status = Stopped}
                           _          -> prevSt
        Stopped       -> case userInput of
                           Just Restart -> prevSt {status = Restarted}
                           _            -> prevSt
        LevelComplete -> case userInput of
                           Just Restart -> prevSt {status = Restarted}
                           _            -> prevSt
        Playing       -> if prevBricks /= [] then
                            case userInput of
                               Just Stop     -> prevSt {status = Stopped}
                               Just Pause    -> prevSt {status = Paused }
                               Just MoveLeft -> 
                                 St 
                                   { position   = newPositions
                                   , bXPosition = newBXPosition (-1)
                                   , bricks     = newBricks
                                   , points     = newPoints
                                   , status     = newStatus
                                   }
                               Just MoveRight -> 
                                 St 
                                   { position   = newPositions
                                   , bXPosition = newBXPosition (1)
                                   , bricks     = newBricks
                                   , points     = newPoints
                                   , status     = newStatus
                                   }
                               Just Shoot -> 
                                  St 
                                   { position   = newPositions ++ [(prevBXPos, height - 1)]
                                   , bXPosition = prevBXPos
                                   , bricks     = newBricks
                                   , points     = newPoints
                                   , status     = newStatus
                                   }
                               _              -> 
                                 St 
                                   { position   = newPositions
                                   , bXPosition = prevBXPos
                                   , bricks     = newBricks
                                   , points     = newPoints
                                   , status     = newStatus
                                   }
                         else prevSt {status = LevelComplete }

    where

       collisions = [bullet | bullet <- prevPositions, brick <- prevBricks, brick == bullet]
       newPositions = filter (\(x,y) -> y > 0) $ map (\(x, y) -> (x, y - 1)) $ filter (\x -> not (x `elem` collisions)) prevPositions
       newBricks = filter (\x -> not (x `elem` collisions)) prevBricks
       newPoints = prevPoints + length collisions
       newStatus = prevStatus
       newBXPosition i = let newBxPos = prevBXPos + i
                       in if newBxPos + baselength > width
                          then prevBXPos
                          else if newBxPos <= 0
                               then 0
                               else newBxPos

