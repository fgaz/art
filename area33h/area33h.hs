import Diagrams
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Diagrams.Backend.SVG.CmdLine

d0 = circle 0.01 # fc black
d1 = vrule 1
d2 = square 1
d3 = polygon $ def {_polyType = PolyRegular 6 1}

side = vsep 0.3 [d3, d2, d1, d0]

cross = f (0/6) <> f (2/6) <> f (4/6)
  where f :: Double -> Diagram B
        f x = strokeT $ fromOffsets [unitX # rotateBy x]

area33h = cross <> foldMap (\(a,x) -> rotate (a @@ deg) x) (zip [0,60..300] $ repeat side)

main = mainWith (rotate (30 @@ deg) area33h :: Diagram B)
