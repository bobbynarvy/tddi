import Data.Vect
import Data.Fin

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

something : (m : Nat) -> (xs : Vect (m + (S k)) a) -> Vect (S k) a

vectTake : (n : Nat) -> Vect (m + n) a -> Vect n a
vectTake Z xs = []
vectTake {m} (S k) xs = something m xs 
