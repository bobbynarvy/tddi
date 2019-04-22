data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius                      
           
Eq Shape where              
  (==) (Triangle x z) (Triangle y w) = x == y && z == w
  (==) (Rectangle x z) (Rectangle y w) = x == y && z == w
  (==) (Circle x) (Circle y) = x == y
  (==) _ _ = False

Ord Shape where
  compare x y = compare (area x) (area y)
