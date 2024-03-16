module Hurtle.ShowHurtle where

-- | Graphics
import Graphics.Gloss as Gloss
    ( Picture(Polygon),
      aquamarine,
      azure,
      black,
      blue,
      chartreuse,
      cyan,
      green,
      magenta,
      orange,
      red,
      rose,
      violet,
      yellow,
      color,
      rotate,
      scale,
      translate,
      blank,
      Color )
import Images ( png )
import Layout ( superimposeAll, Arrange(Leaf), Image )

-- HogoProgram Interpretation
import Hurtle.Types
    ( HogoCode(..),
      Variable(..),
      HogoProgram(code, procTable, varTable),
      KeyValue(Key, Value) )
import qualified Data.Map.Strict as Map

--DRAW BASIC SHAPES-------------------------------------------------------

-- | Gloss source code copy similar to rectanglePath. This is adjusted
--   for use specifically because the path created (Gloss.Path) is a
--   rectangle such that the rectangle formed is not centered around
--   input parameters, but instead only centered on @X@, and starts on
--   @Y=0@ and points directly upwards.
--
--   The implication of this is that the float inputs represent the width
--   of the pen, and the distance the turtle will travel. The central point
--   of the end of the rectangle is also the resultant coordinate after
--   the turtle moves.
customRect
        :: Float        -- ^ width of rectangle
        -> Float        -- ^ height of rectangle
        -> Picture
customRect sizeX sizeY
 = let  sx      = sizeX / 2
   in   Polygon [(-sx, 0.0), (-sx, sizeY), (sx, sizeY), (sx, 0.0)]

-- | Draw line with angle, length, width, and color parameters.
--   Drawn using a @customRect@, not a @Gloss.line@.
hline 
    :: Float -- ^ Angle of rotation clockwise (0 => vertical up)
    -> Float -- ^ Length
    -> Float -- ^ Width
    -> Color -- ^ Gloss.Color color
    -> Image
hline angle len wid cl = 
    Leaf $ Gloss.rotate angle $ Gloss.color cl $ customRect wid len

-- | Align an image with a float coordinate
align 
    :: (Float, Float)         -- ^ Coordinates to displace image by
    -> Image                  -- ^ Image to dispace  
    -> Image
align (x, y) = fmap (Gloss.translate x y)

-- | Construct a @hline@ and align @hline@ to a coordinate
alignLine 
    :: (Float, Float) -- ^ Hurtle position
    -> Float          -- ^ Angle of rotation clockwise (0 => vertical up)
    -> Float          -- ^ Length
    -> Float          -- ^ Width
    -> Color          -- ^ Gloss.Color colour
    -> Image
alignLine coords angle len wid cl = 
    align coords $ hline angle len wid cl


-- | Given a float input, convert to a colour
chooseColor 
    :: Float          -- ^ Float to convert
    -> Color
chooseColor value = colors !! index
    where
        index = floor value `mod` 13
        colors = 
            [
                red, green, blue, yellow, cyan,
                magenta, rose, violet, azure,
                aquamarine, chartreuse, orange, black
            ]   



--STATE CONTAINERS & HELPERS----------------------------------------------------

-- | Container for current run state
data HogoRun = HogoRun {
    program :: HogoProgram,         -- ^ Current program state to be interpreted
    penState :: Bool,               -- ^ Pen state, true => draw, false => move only
    penColor :: Gloss.Color,        -- ^ Pen colour
    penWidth :: Float,              -- ^ Pen width
    turtlePos :: (Float, Float),    -- ^ Current position of Hurtle
    turtleDir :: Float,             -- ^ Current direction in clockwise degrees of turtle (0 is up)
    currentImage :: [Image]         -- ^ List of images formed by prgram code
}

-- | Initial state (doesn't really matter what this is).
--   You can change the background colour here by adjusting 
--   currentImage.
initialState 
    :: HogoProgram      -- ^ A HogoProgram object to interpret
    -> HogoRun
initialState program = HogoRun {
    program = program,
    penState = True,
    penColor = Gloss.black,
    penWidth = 5,
    turtlePos = (0.0, 0.0),
    turtleDir = 0,
    currentImage = [Leaf Gloss.blank]
}

-- | Get the float value out of a variable. Variables can be
--   a combination of operators of variables, or a string
--   referencing another (existing) variable, enforced at syntax
--   analysis, or simply a float value.
--
--   All variables can be converted to a float value due to
--   syntax analysis stage, so recursively locate the float value
--   
--   You can consider a variable storing a reference to another
--   variable as just that, if the variable it is referencing
--   changes, then the value it gets for itself also changes.
getValue :: HogoRun -> Variable -> Float
getValue run var = 

    -- Get the variable map of the current state
    let currmap = varTable (program run) in
    case var of

        -- Either references another variable or contains float value
        (Variable x) -> case x of
            (Key k) -> let found = Map.lookup k currmap in
                case found of
                    -- Recursively trace the float value
                    Just val -> getValue run val
                    -- \/ this case is never reached due to syntax analysis
                    Nothing -> error "not possible" 
            (Value v) -> v -- Or return a float directly

        -- Get the float values of parameters and perform maths
        (Sum x y)        -> getValue run x + getValue run y
        (Difference x y) -> getValue run x - getValue run y
        (Multiply x y)   -> getValue run x * getValue run y
        (Divide x y)     -> getValue run x / getValue run y

getProcedure :: HogoRun -> String -> ([String], HogoProgram)
getProcedure run name = 
    case Map.lookup name (procTable (program run)) of
        Just details -> details
        Nothing -> error "Procedure not found"

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

removeVariable 
    :: HogoRun -> String -> [HogoCode] -> HogoRun
removeVariable run name xs =
    run { 
        program = (program run) 
        {
            varTable = Map.delete name (varTable (program run)),
            code = xs
        }
    }

interpretRepeat 
    :: HogoRun -- ^ Current run state
    -> Variable -- ^ Number of repeats
    -> [HogoCode] -- ^ Repeated code
    -> [HogoCode] -- ^ tail of code
    -> HogoRun
interpretRepeat run count rep xs =
    let val = getValue run count in
        run { program = (program run) { code = concat (replicate (truncate val) rep) ++ xs } }

interpretFor 
    :: HogoRun -- ^ Current run state
    -> String -- ^ Variable name
    -> Variable -- ^ Initial state
    -> Variable -- ^ End condition (>=)
    -> Variable -- ^ Step
    -> [HogoCode] -- ^ Repeated code
    -> [HogoCode] -- ^ Tail code
    -> HogoRun
interpretFor run var strt end step rep xs = 
    let pinit = getValue run strt
        pend = getValue run end
        pstep = getValue run step
        res = [MakeVariable var strt]
        newRep = rep ++ [MakeVariable var (Sum (Variable $ Key var) step)]
        count = length [pinit, pstep+pinit..pend]
    in
        run { program = (program run) { code = res ++ concat (replicate count newRep) ++ xs } }

callFunction 
    :: HogoRun -- ^ Current run state
    -> String -- ^ Function name
    -> [Variable] -- ^ Arguments
    -> [HogoCode] -- ^ Tail code
    -> HogoRun
callFunction run name args xs =
    let (argNames, prog) = getProcedure run name
        newVars = zip argNames args
        newTable = quickUpdate run newVars
        newProgram = (program run) { code = code prog ++ xs, varTable = newTable }
        in run { program = newProgram }
    where
        quickUpdate :: HogoRun -> [(String, Variable)] -> Map.Map String Variable
        quickUpdate prog [] = varTable (program prog)
        quickUpdate prog ((y, var):ys) = Map.insert y var (quickUpdate prog ys)
                


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
    (ClearScreen:xs) -> run {program = (program run) { code = xs }, currentImage = [Leaf Gloss.blank]}
    -- | Variable Usage
    ((MakeVariable name val):xs) -> updateVariable run name val xs
    -- | Control Flow
    ((Repeat count x):xs) -> interpretRepeat run count x xs
    ((For var start end step x):xs) -> interpretFor run var start end step x xs
    ((Function name args):xs) -> callFunction run name args xs

showRun :: HogoRun -> Image
showRun run = superimposeAll $
     currentImage run ++ [align (turtlePos run) (Leaf $ Gloss.rotate (turtleDir run) 
        (Gloss.scale 0.05 0.05 $ png "assets/haskell.png"))]

animation :: HogoProgram -> Int -> Image
animation prog s = showRun $ interpret $ iterate interpret (initialState prog) !! s