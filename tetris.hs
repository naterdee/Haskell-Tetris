{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import qualified Data.Text as T
import Data.List
import System.Random
import Data.String.Conversions (cs)
import Data.Aeson           
import Data.Maybe

data Tetris = Tetris {cshape :: Shape,
                      nshapes :: [String],
                      holdshape :: ([Shape], Bool),
                      full :: [(Double,Double,Color)],
                      time :: Double,
                      score :: Int}
                      deriving (Show, Eq)

--Shape Stuff---------------------------------------
data Shape = Shape {name :: String,
                    pos :: (Double,Double),
                    shape :: [(Double,Double)]}
                    deriving (Show, Eq, Ord)
shapeex = (Shape "LR" (5,8) [(0,0),(1,1),(0,-1),(0,1)])

shapelist :: String -> Shape
shapelist "LR" = (Shape "LR" (5,18) [(0,0),(-1,1),(-1,0),(1,0)])
shapelist "LL" = (Shape "LL" (5,18) [(0,0),(-1,0),(1,0),(1,1)])
shapelist "SQ" = (Shape "SQ" (5,18) [(0,0),(1,1),(1,0),(0,1)])
shapelist "LO" = (Shape "LO" (5,18) [(0,0),(-1,0),(2,0),(1,0)])
shapelist "SR" = (Shape "SR" (5,18) [(0,0),(-1,0),(0,1),(1,1)])
shapelist "SL" = (Shape "SL" (5,18) [(0,0),(0,1),(1,0),(-1,1)])
shapelist "T" = (Shape "T" (5,18) [(0,0),(-1,0),(1,0),(0,1)])

picknewshape 0 =  "LR"
picknewshape 1 =  "LL"
picknewshape 2 =  "SQ"
picknewshape 3 =  "LO"
picknewshape 4 =  "SR"
picknewshape 5 =  "SL"
picknewshape 6 =  "T"
picknewshape x = picknewshape 0

resetshapepos (Shape n p s) = (Shape n (5,18) s)

--Movement------------------------------------------
rotate (Shape n p [(x1,y1),(x2,y2),(x3,y3),(x4,y4)]) = 
        (Shape n p [(y1,-x1),(y2,-x2),(y3,-x3),(y4,-x4)])

move "D" (Shape n (x,y) sps) = (Shape n (x+1,y) sps)
move "A" (Shape n (x,y) sps) = (Shape n (x-1,y) sps)
move "W" sh = rotate sh
move "S" (Shape n (x,y) sps) = (Shape n (x,y-1) sps)
move "Dwn" (Shape n (x,y) sps) = (Shape n (x,y-1) sps)
move "Right" (Shape n (x,y) sps) = (Shape n (x+1,y) sps)
move "Left" (Shape n (x,y) sps) = (Shape n (x-1,y) sps)
move "Up" sh = rotate sh
move "Down" (Shape n (x,y) sps) = (Shape n (x,y-1) sps)
move k sh = sh

harddrop sh frees
                | shapepos (move "Dwn" sh) frees && shapepos (move "Dwn" (move "Dwn" sh)) frees = harddrop (move "Dwn" sh) frees
                | shapepos (move "Dwn" sh) frees = (move "Dwn" sh)
                | otherwise = sh

--Logic Check Stuff---------------------------------
freespace :: [(Double,Double,Color)]
freespace = [ (x,y,white) | x <- [0..9], y <- [0..20]]


checkhelp :: [(Double,Double,Color)] -> (Double,Double,Color) -> [(Double,Double,Color)]
checkhelp [] _ = []
checkhelp ((x,y,c):xs) (x0,y0,c0)
                           | x == x0 && y == y0 = checkhelp xs (x0,y0,c0)
                           | otherwise = (x,y,c) : checkhelp xs (x0,y0,c0)

checkfree :: [(Double,Double,Color)] -> [(Double,Double,Color)]
checkfree fulls = foldl checkhelp freespace fulls

shapepos :: Shape -> [(Double,Double,Color)] -> Bool
shapepos (Shape n (x,y) sps) frees = 4 == length [(p,t) | (p,t) <- map trans sps, (f,s,c) <- frees, p == f && t == s]
         where trans (a,b) = (a + x, b + y)

addtofs :: Shape -> [(Double,Double,Color)] -> [(Double,Double,Color)]
addtofs (Shape "LR" (x,y) sps) fulls = (map addcol (map trans sps)) ++ fulls
        where trans (a,b) = (a + x, b + y)
              addcol (a,b) = (a,b, blue)
addtofs (Shape "LL" (x,y) sps) fulls = (map addcol (map trans sps)) ++ fulls
        where trans (a,b) = (a + x, b + y)
              addcol (a,b) = (a,b, orange)
addtofs (Shape "SQ" (x,y) sps) fulls = (map addcol (map trans sps)) ++ fulls
        where trans (a,b) = (a + x, b + y)
              addcol (a,b) = (a,b, yellow)
addtofs (Shape "LO" (x,y) sps) fulls = (map addcol (map trans sps)) ++ fulls
        where trans (a,b) = (a + x, b + y)
              addcol (a,b) = (a,b, mixed [blue, white])
addtofs (Shape "SR" (x,y) sps) fulls = (map addcol (map trans sps)) ++ fulls
        where trans (a,b) = (a + x, b + y)
              addcol (a,b) = (a,b, green)
addtofs (Shape "SL" (x,y) sps) fulls = (map addcol (map trans sps)) ++ fulls
        where trans (a,b) = (a + x, b + y)
              addcol (a,b) = (a,b, red)              
addtofs (Shape "T" (x,y) sps) fulls = (map addcol (map trans sps)) ++ fulls
        where trans (a,b) = (a + x, b + y)
              addcol (a,b) = (a,b, purple)
              -- howtodoit thecolor = \(a,b) ->(a,b,thecolor)



line10 :: [(Double,Double,Color)] -> Double -> Double
line10 [] _ = 11
line10 fs y0
           | 10 == length [(x,y0) | (x,y,c) <- fs, y0 == y] = y0
           | y0 == 11 = 11
           | otherwise = line10 fs (y0 + 1)

lineremoval fs 11 = fs
lineremoval fs y0 = [(x,y,c) | (x,y,c) <- fs, y /= y0]

lineadjust fs 11 = fs
lineadjust fs y0 = [(x,y,c) | (x,y,c) <- fs, y < y0] ++
                    [(x,y-1,c) | (x,y,c) <- [(x,y,c) | (x,y,c) <- fs, y > y0]]

lineremoved fs = lineadjust (lineremoval fs (line10 fs 0)) (line10 fs 0)

fullline :: [(Double,Double,Color)] -> [(Double,Double,Color)]
fullline fs 
          | lineremoved fs == lineremoved (lineremoved fs) = lineremoved fs
          | otherwise = fullline (lineremoved fs)

howmanylines :: [(Double,Double,Color)] -> Int
howmanylines fs 
                | lineremoved fs == lineremoved (lineremoved fs) = 0
                | otherwise = 1 + howmanylines (lineremoved fs)
          
goodtime :: Double -> Bool
goodtime t = t < 0

fairscoring :: Int -> Int -> Int
fairscoring s 0 = s + 1
fairscoring s 1 = s + 40
fairscoring s 2 = s + 100
fairscoring s 3 = s + 300
fairscoring s l = s + (l * 300)

--Draw Handler--------------------------------------
drawnext :: [Shape] -> Double -> Picture
drawnext _ 5 = pictures []
drawnext ((Shape na p sps):sh) n = pictures ((map rec [(6.5 + x, 5 + y) | (x,y) <- (map coordtrans sps)]) ++ [drawnext sh (n+1)])
          where rec (x,y) = translated x (y + (-2.5 * n)) (colorshape (solidRectangle 1 1) na)
drawhold ([],b) = pictures []
drawhold ([Shape n p sps], b) = pictures $ map rec $ [(-8.5 + x, 5 + y) | (x,y) <- (map coordtrans sps)]
          where rec (x,y) = translated x y (colorshape (solidRectangle 1 1) n)
          
colorshape :: Picture -> String -> Picture
colorshape r "LR" = colored blue r
colorshape r "LL" = colored orange r
colorshape r "SQ" = colored yellow r
colorshape r "LO" = colored (mixed [white,blue]) r
colorshape r "SR" = colored green r
colorshape r "SL" = colored red r
colorshape r "T" = colored purple r
colorshape r "h" = colored (translucent yellow) r

drawrect (x,y,c) = translated (x - 4.5) (y - 9.5) $ colored c $ solidRectangle 1 1

drawhighlight :: Shape -> [(Double,Double,Color)] -> Picture
drawhighlight (Shape n p sps) fulls = drawshape $ harddrop (Shape "h" p sps) (checkfree fulls)

gameover fs = or $ map cond fs
              where cond (x,y,c) = y >= 18

dhand :: Tetris -> Picture
dhand (Tetris csh nsh h fs t s)
                              | gameover fs = (scaled 2 2 (lettering "GAME OVER"))
                              | otherwise = pictures ((drawnext (map shapelist nsh) 0) :
--                                          (translated (-7.66) 0 (lettering (T.pack (take 8 (show t))))) :
                                            (translated (-7.66) (8) (lettering "Hold")) :
                                            (translated (7.5) (8) (lettering "Next")) :
                                            (translated (-7.66) (-2) (lettering "SCORE")) :
                                            (translated (-7.66) (-4) (lettering (T.pack (take 8 (show s))))) :
                                            drawhighlight csh fs :
                                            (drawhold h) :
                                            [drawshape csh] ++
                                            (map drawrect fs) ++
                                            [polyline [(x,-10),(x,10)] | x <- [-5..5]] ++
                                            [polyline [(-5,y),(5,y)] | y <- [-10..10]] ++
                                            [(translated 7.5 0.0 (colored grey (solidRectangle 5 20))),
                                            (translated (-7.5) 0.0 (colored grey (solidRectangle 5 20)))])


coordtrans (x,y) = (x + 0.5, y + 0.5)


drawshape :: Shape -> Picture
drawshape (Shape n (x0,y0) sps) = pictures $ map (translated (-5) (-10)) $ map rec $ [(x0 + x, y0 + y) | (x,y) <- (map coordtrans sps)]
          where rec (x,y) = translated x y (colorshape (solidRectangle 1 1) n)

--Event Handler-------------------------------------
hand :: Event -> Tetris -> Tetris
hand (KeyPress " ") (Tetris csh nsh (hsh,b) fs t s) = (Tetris (shapelist (head nsh))
                                                      (tail nsh)
                                                      (hsh,False)
                                                      (fullline (addtofs (harddrop csh (checkfree fs)) fs))
                                                      t
                                                      (fairscoring s (howmanylines (addtofs (harddrop csh (checkfree fs)) fs))))

hand (KeyPress "S") (Tetris csh nsh (hsh,b) fs t s)
                                          | shapepos (move "S" csh) (checkfree fs) && 
                                            shapepos (move "S" (move "S" csh)) (checkfree fs) = (Tetris (move "S" csh) nsh (hsh,False) fs t s)
                                          | shapepos (move "S" csh) (checkfree fs) = (Tetris (shapelist (head nsh))
                                                                                             (tail nsh)
                                                                                             (hsh,False)
                                                                                             (fullline (addtofs (move "S" csh) fs))
                                                                                             t
                                                                                             (fairscoring s (howmanylines (addtofs (move "S" csh) fs))))
                                          | otherwise = (Tetris csh nsh (hsh,b) fs t s)

hand (KeyPress "Down") game = hand (KeyPress "S") game

hand (KeyPress "C") (Tetris csh nsh ([],_) fs t s) = (Tetris (shapelist (head nsh)) (tail nsh) ([csh],False) fs t s)
hand (KeyPress "C") (Tetris csh nsh ([hsh],False) fs t s) = (Tetris hsh nsh ([resetshapepos csh],True) fs t s)
hand (KeyPress char) (Tetris csh nsh h fs t s)
                                         | shapepos (move char csh) (checkfree fs) = (Tetris (move char csh) nsh h fs t s)
                                         | otherwise = (Tetris csh nsh h  fs t s)

hand (TimePassing n) (Tetris csh nsh (hsh,b) fs t s)
                                           | goodtime t &&
                                             shapepos (move "S" csh)
                                             (checkfree fs) && 
                                             shapepos (move "S" (move "S" csh)) 
                                             (checkfree fs) = (Tetris (move "S" csh)
                                                                      nsh
                                                                      (hsh,b)
                                                                      fs
                                                                      (1)
                                                                      s)
                                           | goodtime t &&
                                             shapepos (move "S" csh) 
                                             (checkfree fs) = (Tetris (shapelist (head nsh)) 
                                                                      (tail nsh) 
                                                                      (hsh,False)
                                                                      (fullline (addtofs (move "S" csh) fs)) 
                                                                      (1) 
                                                                      (fairscoring s (howmanylines (addtofs (move "S" csh) fs))))
                                           | goodtime t = (Tetris csh nsh (hsh,b) fs 1 s)
                                           | otherwise = (Tetris csh nsh (hsh,b) fs (t-n) s)
--hand (TimePassing n) (Tetris csh nsh fs t s) = (Tetris csh nsh fs (t-n) s)
hand _ t = t


--Testing Stuff-------------------------------------
testctrans (x,y) = (x + 0.5, y + 0.5)

testdraw :: Shape -> Picture
testdraw (Shape n p sps) = pictures $ map rec $ map testctrans sps
          where rec (x,y) = translated x y (rectangle 1 1)
          
--Main Code-----------------------------------------
{--
--Testing code
main = drawingOf $ pictures [testdraw shapeex,
                             coordinatePlane]
--}
endlessgen :: (Double, StdGen) -> (Double, StdGen)
endlessgen (n, gen) = randomR (0,7) gen

endlesslist :: (Double, StdGen) -> [Double]
endlesslist (x, gen) = (fst (endlessgen (x, gen))) : (endlesslist (endlessgen (x, gen)))

tetrisex = Tetris (shapelist "SL") (map picknewshape (map round (endlesslist (69, mkStdGen 137)))) ([],False) [] 0 0

norepeats :: [String] -> String -> [String]
norepeats [] _ = []
norepeats (sh:shs) s
                   | sh == s = norepeats shs sh
                   | otherwise = sh : norepeats shs sh

main = do
       gen <- newStdGen
       gen' <- newStdGen
       let randomnum gen = randomR (0,7) gen :: (Double, StdGen)
       let tetrisstart = Tetris (shapelist (picknewshape (round (fst (randomnum gen')))))
                                (norepeats (map picknewshape (map round (endlesslist (69, gen)))) (picknewshape (round (fst (randomnum gen')))))
                                ([], False)
                                []
                                2
                                0
       activityOf tetrisstart hand dhand















