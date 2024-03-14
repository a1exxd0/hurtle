module Hurtle.ShowHurtle where

import Graphics.Gloss as Gloss
import Hatch
import Hurtle.Types
import qualified Data.Map.Strict as Map


hline :: Float -> Float -> Float -> Color -> Image
hline angle length width color = 
    Leaf $ Gloss.rotate angle $ Gloss.color color $ Gloss.rectangleSolid width length

alignLine :: (Float, Float) 
    -> Float -> Float -> Float -> Color 
    -> Image
alignLine (x, y) angle length width color = 
    fmap (Gloss.translate (x / 2) (y / 2)) (hline angle length width color)

data HogoRun = HogoRun {
    program :: HogoProgram,
    penState :: Bool,
    penColor :: Gloss.Color,
    penWidth :: Float,
    turtlePos :: (Float, Float),
    turtleDir :: Float,
    currentImage :: Image
}

initialState :: HogoProgram -> HogoRun
initialState program = HogoRun {
    program = program,
    penState = True,
    penColor = Gloss.black,
    penWidth = 5,
    turtlePos = (0.0, 0.0),
    turtleDir = 0,
    currentImage = Hatch.blank
}

getValue :: HogoRun -> Variable -> Float
getValue run var = let map = varTable (program run) in
    case var of
        (Variable x) -> case x of
            (Key k) -> let found = Map.lookup k map in
                case found of
                    Just x -> getValue run x
                    Nothing -> error "not possible"
            (Value v) -> v

interpret :: HogoRun -> HogoRun
interpret run = let cmd = head $ code $ program run in
    case cmd of
        (Forward x) -> let val = getValue run x in
            let img = alignLine (turtlePos run) (turtleDir run) val (penWidth run) (penColor run) in
                let finImg = superimposeAll [img, currentImage run] in
                    HogoRun {
                        program = HogoProgram {
                            varTable = varTable $ program run,
                            procTable = procTable $ program run,
                            code = tail $ code $ program run
                        },
                        penState = penState run,
                        penColor = penColor run,
                        penWidth = penWidth run,
                        turtlePos = (turtlePos) run,
                        turtleDir = turtleDir run,
                        currentImage = finImg
                    }

showRun :: HogoRun -> Image
showRun = currentImage

animation :: HogoProgram -> Int -> Image
animation prog s = showRun $ interpret $ iterate interpret (initialState prog) !! s