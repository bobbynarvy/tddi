import Data.Vect

data PowerSource = Petrol | Pedal | Electric

data Vehicle : PowerSource -> Type where
     Bicycle : Vehicle Pedal
     Unicycle : Vehicle Pedal
     Motocycle : (fuel : nat) -> Vehicle Petrol
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Tram : Vehicle Electric
     
wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels Unicycle = 1
wheels (Motocycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Tram = 4

refuel : Vehicle Petrol -> Vehicle Petrol         
refuel (Motocycle fuel) = Motocycle 50
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200

vectTake : (n : Nat) -> Vect (n + m) elem -> Vect n elem
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                Just idx => Just ((index idx xs) + (index idx ys))
