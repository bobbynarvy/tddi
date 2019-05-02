every_other : Stream a -> Stream a
every_other (value :: (x :: xs)) = x :: every_other xs

data InfList : Type -> Type where
     (::) : (value : elem) -> Inf (InfList elem) -> InfList elem
     
Functor InfList where
  map func (value :: xs) = func value :: map func xs
  
data Face = Heads | Tails

getFace : Int -> Face
getFace x = if (mod (abs x) 2 == 0) then Heads else Tails

coinFlips : (count : Nat) -> Stream Int -> List Face
coinFlips Z xs = []
coinFlips (S k) (value :: xs) = getFace value :: (coinFlips k xs)

square_root_approx : (number : Double) -> (approx : Double) -> Stream Double
square_root_approx number approx = approx :: square_root_approx number ((approx + (number / approx)) / 2)

square_root_bound : (max : Nat) -> (number : Double) -> (bound : Double) -> (approxs : Stream Double) -> Double
square_root_bound Z number bound (value :: xs) = value
square_root_bound (S k) number bound (value :: xs) = if ((value * value) - number) < bound
                                                        then value
                                                        else square_root_bound k number bound xs
                                                        
square_root : (number : Double) -> Double
square_root number = square_root_bound 100 number 0.00000000001 (square_root_approx number number)
