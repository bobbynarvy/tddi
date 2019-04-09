data Direction = North
               | East
               | South
               | West

turnClockwise : Direction -> Direction
turnClockwise North = East
turnClockwise East = South
turnClockwise South = West
turnClockwise West = North

||| Represents shapes
data Shape = ||| A triangle, with ist base length and height
             Triangle Double Double 
           | ||| A rectangle, with its length and height
             Rectangle Double Double 
           | ||| A circle, with its radius
             Circle Double
           
area : Shape -> Double         
area (Triangle base height) = 0.5 * base * height
area (Rectangle length height) = length * height
area (Circle radius) = pi * radius * radius

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)

testPicture : Picture
testPicture = Combine (Translate 5 5 rectangle) 
              (Combine (Translate 35 5 circle) 
              (Translate 15 25 triangle))                          

%name Shape shape, shape1, shape2
%name Picture pic, pic1, pic2

pictureArea : Picture -> Double
pictureArea (Primitive shape) = area shape
pictureArea (Combine pic pic1) = pictureArea pic + pictureArea pic1
pictureArea (Rotate x pic) = pictureArea pic
pictureArea (Translate x y pic) = pictureArea pic

-- data Maybe valtype = Nothing | Just valtype

safeDivide : Double -> Double -> Maybe Double
safeDivide x y = if y == 0 then Nothing
                           else Just (x / y)

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)
               
%name Tree tree, tree1

insert : Ord elem => elem -> Tree elem -> Tree elem                              
insert x Empty = Node Empty x Empty
insert x (Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => Node left val right
                                      GT => Node left val (insert x right)
