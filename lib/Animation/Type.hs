module Animation.Type where

import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Strict (StateT(..), evalStateT)

type Animation env st a = ReaderT env (StateT st IO) a

data Object = Ball Int
            | Base Int Int

type Brick = (Int, Int)

data GameStatus = Paused
                | Playing
                | Stopped
                | LevelComplete
                | Restarted
                deriving (Show)

data UserInput = MoveLeft
               | MoveRight
               | Pause
               | Stop
               | Start
               | Restart
               | Shoot

runAnimation :: env -> st -> Animation env st a -> IO a
runAnimation env st action = evalStateT (runReaderT action env) st