import Data.Vect

Vector : Nat -> Type
Vector n = Vect n Double

Matrix : Nat -> Nat -> Type
Matrix m n = Vect m (Vector n)

TupleVect : Nat -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, (TupleVect k x))
