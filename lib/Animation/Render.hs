module Animation.Render where

import Control.Monad.Trans.State.Strict (get)
import Control.Monad.Trans.Reader (ask)
import Control.Monad.Trans.Class (lift)

import Animation.Env (Env(..))
import Animation.State (St(..))
import Animation.Type (Animation,Object(..),Brick(..), GameStatus(..))

render :: Animation Env St ()
render = do
    val <- renderVal
    lift (lift (putStrLn val))

renderVal :: Animation Env St String
renderVal = do
    env <- ask
    st <- lift get
    return (renderInternal env st)

renderInternal :: Env -> St -> String
renderInternal env st = makeBox (size        env) 
                                (baselength  env) 
                                (bXPosition  st ) 
                                (position    st ) 
                                (bricklength env) 
                                (bricks      st ) 
                                (status      st )
                                (points      st )

-- | Definition of how each line is going to be rendered according to what is there. Oprions are:
-- | 1) Nothing / 2) Just bricks / 3) Just the ball / 4) Just the base / 5) The ball and bricks /
-- | 6) The ball and the base / (The base and bricks is not an option)

makeLine :: Char 
         -> Char 
         -> Int 
         -> [Int] 
         -> Maybe Object 
         -> [Int] 
         -> String
makeLine endChar innerChar i mb mba bricks =
    let positions = [0 .. i]
        renderPixel x = 
            if x `elem` mb 
                then '.'
                    else if x `elem` bricks then '='
                        else case mba of 
                            Nothing           -> ' ' 
                            Just (Base bl ba) -> if x `elem` [ba..(ba+bl)] then ':' else innerChar
                                 
                -- Just (Ball b) -> case mba of 
                --                Nothing           -> if x == b then 'O' else printBlock x
                --                Just (Base bl ba) -> if x == b then 'O' 
                --                                     else if x `elem` [ba..(ba+bl)] then ':' 
                --                                     else innerChar
                                       
     in [endChar] ++ map renderPixel positions ++ [endChar]
     
     -- | Finding if a brick should be the owner of a pixel considering its position and length
     -- | If True, it paints it according to the life of the brick
     {-
     where brickXPositions = map (fst . brickPosition) bricks
           printBlock x    = if x `elem` foldl (\u v  -> u ++ [v..(v+bricklength-1)]) [] brickXPositions
                             then if (life $ pixelOwner x) > 0 then '=' else '-'
                             else innerChar
           pixelOwner x    = head $ filter (\u -> x - fst (brickPosition u) < bricklength
                                               && x - fst (brickPosition u) >= 0 ) bricks
-}

makeBox :: (Int, Int) 
        -> Int 
        -> Int 
        -> [(Int, Int)]
        -> Int 
        -> [Brick] 
        -> GameStatus
        -> Int 
        -> String
makeBox (numCols, numRows) baseL baseX bullets bricklength bricks status points =
    unlines 
        (["            BRICK BREAKER VIDEOGAME"] ++ [" "] 
        ++ case status of 
              LevelComplete -> [celebratrionCartoon]
              _             -> [   makeLine '-' '-' numCols [] Nothing [] ]
                              ++   mappedPositions          
                              ++ [ makeLine '-' '-' numCols [] Nothing [] ]
                            --   ++ ["Status: " ++ show status
                            --      ++ if ballY /= numRows then   " | Score: " ++ show points 
                            --         else  " | ***** GAME OVER ***** | Your Score is " ++ show points 
                            --      ]
                            --   ++ -- Render menu according to status
                            --      [ case status of
                            --          Stopped       -> "Press (R) to Restart"
                            --          Paused        -> "Press (S) to Play | Controls: (A) Left / (D) Right"
                            --          Playing       -> "(P) Pause / (Q) Stop / (A) Left / (D) Right"
                            --          _             -> ""
                            --      ]
                           -- | Uncomment these lines for debugging purposes 
--                            ++ [  "BaseX: " ++ show (baseX + div baseL 2) 
--                               ++ " | Ball: (" ++ show ballX ++ "," ++ show ballY ++ ")"
--                               ++ " | BallOverBase: "   ++ show (ballX >= baseX && (ballX <= (baseX + baseL)))
--                               ]
        )
    where   positions = [0 .. numRows]
            mappedPositions = map lineMaker positions

         -- | Painting lines depending on the position of the ball and position of the base given a Y position
            
            lineMaker y =
              let brickXPositions = map fst $ filter ((==) y . snd) bricks
                  ballXpositions = map fst $ filter ((==) y . snd) bullets   
               in 
                    if y == numRows - 1
                         then makeLine '|' ' ' numCols (ballXpositions) (Just (Base baseL baseX)) brickXPositions
                         else makeLine '|' ' ' numCols (ballXpositions) Nothing                   brickXPositions 
                    -- else if y == numRows - 1
                    --      then makeLine '|' ' ' numCols Nothing             (Just (Base baseL baseX)) brickYPositions bricklength
                    --      else makeLine '|' ' ' numCols Nothing             Nothing                   brickYPositions bricklength
            
            celebratrionCartoon = 
                                    "                        .-."
                                ++"\n                _.--¨¨¨¨.o/         .-.-._"
                                ++"\n             __'   .¨¨¨; {        _J ,__  `.       Level Complete"
                                ++"\n            ; o`.-.`._.'J;       ; /  `- /  ;"
                                ++"\n            `--i`¨. `¨ .';       `._ __.'   |     ¡CONGRATULATIONS!"
                                ++"\n                `  `¨¨¨   `         `;      :"
                                ++"\n                 `.¨-.     ;     ____/     /     Your Score: " ++ show points ++ " points"
                                ++"\n                   `-.`     `-.-'    `¨-..'"
                                ++"\n     ___              `;__.-'¨           `."
                                ++"\n  .-{_  `--._         /.-¨                 `-."
                                ++"\n /    ¨¨T    ¨¨---...'  _.-¨¨   ¨¨¨-.         `."
                                ++"\n;       /                 __.-¨¨.    `.         `,             _.."
                                ++"\n `     /            __.-¨¨       '.    `          `.,__      .'L' }"
                                ++"\n  `---¨`-.__    __.¨    .-.       j     `.         :   `.  .' ,' /"
                                ++"\n            ¨¨¨¨       /   `     :        `.       |     F' `   ;"
                                ++"\n                      ;     `-._,L_,-¨¨-.   `-,    ;     `   ; /"
                                ++"\n                       `.       |        `-._  `.__/_        `/"
                                ++"\n                         `     _;            `  _.'  `-.     /"
                                ++"\n                          `---¨ `.___,,      ;¨¨        `  .'"
                                ++"\n                                    _/       ;           `¨"
                                ++"\n      Bring me                   .-¨     _,-' "
                                ++"\n     more bricks!               {       ¨¨;            Next Level - Press SPACE"
                                ++"\n                                 ;-.____.'`."
                                ++"\n      I am not done yet!          `.  ` '.  :"
                                ++"\n                                    `  : : /"
                                ++"\n                                     `':/ `"
                                

                                {-     "\n                /`_.----._"
                                    ++"\n              .¨ _,=<'¨=. ¨,/|   Hey you did great."
                                    ++"\n             /,-'    ¨=.`.   (   You're almost as good as Sulley!"
                                    ++"\n            //         ` |    `"
                                    ++"\n           /,    _.,.   |      `    (|"
                                    ++"\n         ,¨ |   ,`'v/', |       `  _)("
                                    ++"\n        /   |   !>(¨)<|/         ` c_ `"
                                    ++"\n     _-/     `  '=,Z``7           . C. `"
                                    ++"\n _,-¨ V  /    '-._,>*¨     `      |   ` `"
                                    ++"\n `  <¨|  )` __ __ ____ _    Y     |    ` `"
                                    ++"\n  ` ` |   >._____________.< |     ¨-.   ` `"
                                    ++"\n   ` `|   ` `/`/`/`/`/`/ /' /    =_  '-._) `"
                                    ++"\n    ` (    `            /         |¨*=,_   /"
                                    ++"\n     ` `    `_/`/`/`/`_/         /      ¨¨¨"
                                    ++"\n     _).^     ¨******¨          /"
                                    ++"\n    (()!|`                     /"
                                    ++"\n     *==¨ ¨,                 ,¨"
                                    ++"\n            ¨,_            ,¨"
                                    ++"\n               `¨*<==> ,=*¨"
                                    ++"\n                ` ` / /"
                                    ++"\n            _____>_V /"
                                    ++"\n           f  .-----¨"
                                    ++"\n           |  `    ` `"
                                    ++"\n           |   `    ` '-."
                                    ++"\n           J    `    `   `"
                                    ++"\n          (  ` ` ` _.-J   )"
                                    ++"\n           `V)V)=*.','  ,'"
                                    ++"\n    jjs        (V(V)(V)/"-}