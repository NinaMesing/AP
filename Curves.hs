module Curves
( point
, pointX
, pointY
, curve
, connect
, rotate
, translate
, reflect
, bbox
, width
, height
, toList
, normalize
, toSVG
, toFile
, Line(..)
, Point(..)
, Curve(..)
) where


data Point = Point (Double,Double) 
instance Eq Point where (Point (x1,y1)) == (Point (x2,y2)) = (abs(x1-x2) < 0.01) && (abs(y1-y2) < 0.01)
instance Show Point where show p = "(" ++ show (pointX p) ++ "," ++ show (pointY p) ++ ")"  

point :: (Double, Double) -> Point
point (a, b) = Point(a, b)

pointX :: Point -> Double
pointX (Point (x,_)) = x

pointY :: Point -> Double
pointY (Point (_,y)) = y


data Curve = Curve [Point]
instance Eq Curve where (Curve xs) == (Curve ys) = xs == ys

curve :: Point -> [Point] -> Curve
curve p ps = Curve (p:ps)

connect :: Curve -> Curve -> Curve
connect (Curve c1) (Curve c2) = Curve( c1 ++ c2)

rotate :: Curve -> Double -> Curve
rotate (Curve points) d  = Curve (map (rotatePoint d) points)


rotatePoint :: Double -> Point -> Point
rotatePoint d p = let x = pointX p 
                      y = pointY p 
                      rad = d*pi/180 
                  in Point (x*cos(rad)-y*sin(rad), x*sin(rad)+y*cos(rad))
                  
translate2 :: Curve -> Point -> Point 
translate2 (Curve cs) p = let addVector = getTranslationVector (head cs) p
                          in  addVector

translate :: Curve -> Point -> Curve
translate (Curve cs) p = let addVector = getTranslationVector (head cs) p 
                     in Curve $ map (\cp -> Point (pointX cp + pointX addVector, pointY cp + pointY addVector)) (cs)

--This is a helper function, which calculates the vector to add to each point in the curve.
getTranslationVector :: Point -> Point -> Point
getTranslationVector p1 p2 = Point (pointX p2 - pointX p1, pointY p2 - pointY p1)

data Line = Vertical Double | Horizontal Double

reflect :: Curve -> Line -> Curve
reflect (Curve c) (Horizontal d) = Curve $ map (\p -> Point(pointX p, pointY p - 2*(pointY p - d))) c 
reflect (Curve c) (Vertical d) = Curve $ map (\p -> Point(pointX p - 2*(pointX p - d), pointY p)) c

bbox :: Curve -> (Point, Point)
bbox (Curve cs) = let highestVal = foldl (\acc p -> if pointY p > acc then pointY p else acc) (pointY (head cs)) cs
                      lowestVal = foldl (\acc p -> if pointY p < acc then pointY p else acc) (pointY (head cs)) cs
                      rightmost = foldl (\acc p -> if pointX p > acc then pointX p else acc) (pointX (head cs)) cs
                      leftmost = foldl (\acc p -> if pointX p < acc then pointX p else acc) (pointX (head cs)) cs
                  in (Point (leftmost, lowestVal), Point (rightmost, highestVal))  

width :: Curve -> Double
width c = let upperRight = snd $ bbox c
              lowerLeft = fst $ bbox c
          in pointX upperRight - pointX lowerLeft

height :: Curve -> Double
height c = let upperRight = snd $ bbox c
               lowerLeft = fst $ bbox c
           in pointY upperRight - pointY lowerLeft

toList :: Curve -> [Point]
toList (Curve c) = c

normalize :: Curve -> Curve
normalize (Curve cs) = let box = bbox $ Curve $ cs
                           leftLower = fst $ box
                           move = if (pointX leftLower) < 0.0 || (pointY leftLower) < 0.0 then True else False 
                           newStartingPoint = Point (pointX (head cs) - pointX leftLower, pointY (head cs) - pointY leftLower)
                       in if move then (translate (Curve(cs)) newStartingPoint) else Curve cs

--SVG part

--Helper function for toSVG that recursively setup the lines for drawing the curve
exstractLines :: [Point] -> String
exstractLines (_:[]) = ""
exstractLines (l1:l2:cs) = "<line style=\"stroke-width: 2px; stroke: black; fill:white\" x1=\"" ++ show(pointX l1) ++ "\" x2=\"" ++ show(pointX l2) ++ "\" y1=\"" ++ show(pointY l1) ++ "\" y2=\"" ++ show(pointY l2) ++ "\" />\n" ++ exstractLines (l2:cs)

--Constructs the svg string by combining 
toSVG :: Curve -> String
toSVG (Curve cur) = let svgStart = "<svg xmlns=\"http://www.w3.org/2000/svg\""
                        svgWidth = "      width=\"" ++ show (width (Curve(cur))) ++ "px\" height=\""++ show (height (Curve(cur))) ++"px\" version=\"1.1\">"
                   in   svgStart ++ "\n" ++ svgWidth ++ "\n<g>" ++ "\n" ++ exstractLines cur ++ "\n</g>\n</svg>"



toFile :: Curve -> FilePath -> IO ()
toFile cs filePath = writeFile filePath (toSVG cs)


hilbert :: Curve -> Curve
hilbert c = c0 `connect` c1 `connect` c2 `connect` c3
   where  w = width c
          h = height c
          p = 6

          ch = reflect c $ Vertical 0

          c0 = ch `rotate` (-90) `translate` Point (w+p+w, h+p+h)
          c1 = c `translate` Point (w+p+w, h)
          c2 = c
          c3 = ch `rotate` 90 `translate` Point (0, h+p)
          
