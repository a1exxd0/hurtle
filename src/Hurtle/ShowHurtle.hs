module Hurtle.ShowHurtle where

import Graphics.Gloss as Gloss
import Hatch
import Hurtle.Types
import qualified Data.Map.Strict as Map
import Control.Monad.State
--DRAW FUNCTIONS-------------------------------------------------------

-- | Gloss source code copy similar to rectanglePath
customRect
        :: Float        -- ^ width of rectangle
        -> Float        -- ^ height of rectangle
        -> Picture
customRect sizeX sizeY
 = let  sx      = sizeX / 2
   in   Polygon [(-sx, 0.0), (-sx, sizeY), (sx, sizeY), (sx, 0.0)]

-- | Draw line with angle, length, width, and color params
hline 
    :: Float -- ^ Angle of rotation clockwise (0 => vertical up)
    -> Float -- ^ Length
    -> Float -- ^ Width
    -> Color -- ^ Gloss.Color color
    -> Image
hline angle len wid cl = 
    Leaf $ Gloss.rotate angle $ Gloss.color cl $ customRect wid len

align :: (Float, Float) -> Image -> Image
align (x, y) = fmap (Gloss.translate x y)

-- | Align a 'hline' to a position
alignLine 
    :: (Float, Float) -- ^ Hurtle position
    -> Float -> Float -> Float -> Color -- ^ hline params
    -> Image
alignLine coords angle len wid cl = 
    align coords $ hline angle len wid cl




--STATE CONTAINERS----------------------------------------------------
data HogoRun = HogoRun {
    program :: HogoProgram,
    penState :: Bool,
    penColor :: Gloss.Color,
    penWidth :: Float,
    turtlePos :: (Float, Float),
    turtleDir :: Float,
    currentImage :: [Image]
}

initialState :: HogoProgram -> HogoRun
initialState program = HogoRun {
    program = program,
    penState = True,
    penColor = Gloss.black,
    penWidth = 5,
    turtlePos = (0.0, 0.0),
    turtleDir = 0,
    currentImage = [Hatch.blank]
}


getValue :: HogoRun -> Variable -> Float
getValue run var = let currmap = varTable (program run) in
    case var of
        (Variable x) -> case x of
            (Key k) -> let found = Map.lookup k currmap in
                case found of
                    Just val -> getValue run val
                    Nothing -> error "not possible"
            (Value v) -> v

        (Sum x y) -> getValue run x + getValue run y
        (Difference x y) -> getValue run x - getValue run y
        (Multiply x y) -> getValue run x * getValue run y

updatePosByAngle 
    :: (Float, Float)   -- ^ Original position
    -> Float            -- ^ Angle of rotation from up
    -> Float            -- ^ Length of distance travelled
    -> (Float, Float)
updatePosByAngle (x, y) theta len 
    = (x + (len * sin (theta * (pi / 180))), y + (len * cos (theta * (pi / 180))))

drawByMultiplier 
    :: HogoRun      -- ^ Current run state
    -> Variable     -- ^ Variable to update by
    -> [HogoCode]   -- ^ Tail after function is called on program code
    -> Float        -- ^ 1 or -1 depending on direction
    -> HogoRun
drawByMultiplier run x xs mul = 
    let val = getValue run x
        img = alignLine (turtlePos run) (turtleDir run) (val * mul) (penWidth run) (penColor run)
        finImg =  img:currentImage run
        newProgram = (program run) { code = xs } -- Update the program's code
        newPos = updatePosByAngle (turtlePos run) (turtleDir run) (val * mul)

        in run { program = newProgram, currentImage = finImg, turtlePos = newPos}

moveByMultiplier 
    :: HogoRun      -- ^ Current run state
    -> Variable     -- ^ Variable to update by
    -> [HogoCode]   -- ^ Tail after function is called on program code
    -> Float        -- ^ 1 or -1 depending on forward or backward
    -> HogoRun
moveByMultiplier run x xs mul = 
    let val = getValue run x
        newProgram = (program run) { code = xs } -- Update the program's code
        newPos = updatePosByAngle (turtlePos run) (turtleDir run) (val * mul)

        in run { program = newProgram, turtlePos = newPos}

rotateByAngle 
    :: HogoRun      -- ^ Current run state
    -> Variable     -- ^ Variable to update by
    -> [HogoCode]   -- ^ Tail after function is called on program code
    -> Float        -- ^ 1 or -1 depending on left or right
    -> HogoRun
rotateByAngle run x xs mul = 
    let val = getValue run x
        newProgram = (program run) { code = xs } -- Update the program's code
        newDir = turtleDir run + (val * mul)

        in run { program = newProgram, turtleDir = newDir}

chooseColor :: Float -> Color
chooseColor value = colors !! index
    where
        index = floor value `mod` 13
        colors = 
            [
                red, green, blue, yellow, cyan,
                magenta, rose, violet, azure,
                aquamarine, chartreuse, orange, black
            ]   

setHogoColor 
    :: HogoRun -- ^ Current run state
    -> Variable -- ^ Variable to change to
    -> [HogoCode] -- ^ Tail of code
    -> HogoRun    
setHogoColor run x xs = 
    let val = getValue run x
        newProgram = (program run) { code = xs } -- Update the program's code
        newColor = chooseColor val

        in run { program = newProgram, penColor = newColor}

setHogoWidth 
    :: HogoRun -- ^ Current run state
    -> Variable -- ^ Variable to change to
    -> [HogoCode] -- ^ Tail of code
    -> HogoRun    
setHogoWidth run x xs = 
    let val = getValue run x
        newProgram = (program run) { code = xs } -- Update the program's code
        newWidth = val

        in run { program = newProgram, penWidth = newWidth}

checkPenMovement 
    :: HogoRun -- ^ Current run state
    -> Variable -- ^ Variable to change to
    -> [HogoCode] -- ^ Tail of code
    -> Float -- ^ 1 for forward, -1 for back
    -> HogoRun
checkPenMovement run x xs mul
    | penState run          = drawByMultiplier run x xs mul
    | otherwise             = moveByMultiplier run x xs mul

updateVariable 
    :: HogoRun -> String -> Variable -> [HogoCode] -> HogoRun
updateVariable run name var xs =
    let val = getValue run var
        in run { 
            program = (program run) 
            {
                varTable = Map.insert name (Variable $ Value val) (varTable (program run)),
                code = xs
            }
        }

interpretRepeat :: HogoRun -> Int -> [HogoCode] -> [HogoCode] -> HogoRun
interpretRepeat run count rep xs = 
    let tempRun = run { program = (program run) { code = concat $ replicate count rep } }  -- Set the program code to the repeated section
        res = iterate interpret tempRun !! (count * length rep) -- Apply the interpreter function 'count' times
    in res { program = (program run) { code = xs } } 



interpret :: HogoRun -> HogoRun
interpret run = case code (program run) of
    [] -> run -- No commands left, return the current state
    -- | Movement Commands
    ((Forward x):xs) -> checkPenMovement run x xs 1
    ((Back x):xs) -> checkPenMovement run x xs (-1)
    ((GoRight x):xs) -> rotateByAngle run x xs 1
    ((GoLeft x):xs) -> rotateByAngle run x xs (-1)
    (Home:xs) -> run {turtlePos = (0,0), turtleDir = 0, program = (program run) { code = xs } }
    -- | Pen Commands
    ((SetColor x):xs) -> setHogoColor run x xs
    ((SetWidth x):xs) -> setHogoWidth run x xs
    (PenUp:xs) -> run {penState = False, program = (program run) { code = xs } }
    (PenDown:xs) -> run {penState = True, program = (program run) { code = xs } }
    (ClearScreen:xs) -> run {program = (program run) { code = xs }, currentImage = [Hatch.blank]}
    -- | Variable Usage
    ((MakeVariable name val):xs) -> updateVariable run name val xs
    -- | Control Flow
    ((Repeat count x):xs) -> interpretRepeat run count x xs
    

    _ -> undefined 

showRun :: HogoRun -> Image
showRun run = superimposeAll $
     currentImage run ++ [align (turtlePos run) (Leaf $ Gloss.rotate (turtleDir run) 
        (Gloss.scale 0.05 0.05 $ png "assets/haskell.png"))]

animation :: HogoProgram -> Int -> Image
animation prog s = showRun $ interpret $ iterate interpret (initialState prog) !! s
    
--superimposeAll [hline 0 100 10 Gloss.black,alignLine (0, 100) 90 100 10 Gloss.green,alignLine (100,100) 180 100 10 Gloss.black,align (100,0) (Leaf $ Gloss.rotate (90) (Gloss.scale 0.05 0.05 $ png "assets/haskell.png"))]
    
-- superimposeAll [superimposeAll [hline 0 100 10 Gloss.black, alignLine (0, 100) 90 100 10 Gloss.green], align (100,100) (Leaf $ Gloss.rotate (90) (Gloss.scale 0.05 0.05 $ png "assets/haskell.png"))]
    
    
-- showRun $ interpret $ iterate interpret (initialState prog) !! s