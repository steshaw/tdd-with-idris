data PowerSource = Petrol | Pedal

data Vehicle : PowerSource -> Type where
  Unicycle : Vehicle Pedal
  Bicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Motocycle : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Car _) = 4
wheels (Motocycle _) = 2
wheels (Bus _) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Motocycle fuel) = Motocycle 50
refuel (Bus fuel) = Bus 200
refuel Bicycle impossible
