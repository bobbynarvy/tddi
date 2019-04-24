data Vect : (len : Nat) -> a -> Type where
     Nil : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) ->  Vect (S k) a
     
Eq a => Eq (Vect len a) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = case x == y of
                                  True => (xs == ys)
                                  False => False

Foldable (Vect len) where
  foldr func acc [] = acc
  foldr func acc (x :: xs) = func x  (foldr func acc xs)
  foldl func acc [] = acc
  foldl func acc (x :: xs) = foldl func (func acc x) xs
