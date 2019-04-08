import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans
                             
addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)                  
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

getDotProduct : Num a => Vect m a -> Vect m a -> a
getDotProduct [] [] = 0
getDotProduct (x :: xs) (y :: ys) = (x * y) + (getDotProduct xs ys)

getDotProductVector : Num a => Vect m a -> Vect n (Vect m a) -> Vect n a
getDotProductVector a [] = []
getDotProductVector a (b :: bs) = getDotProduct a b :: getDotProductVector a bs

multMatrix : Num numType =>
             Vect n (Vect m numType) -> Vect m (Vect p numType) -> Vect n (Vect p numType)
multMatrix [] b = []
multMatrix (x :: xs) b = let b' = transposeMat b in 
                             getDotProductVector x b' :: multMatrix xs b 

