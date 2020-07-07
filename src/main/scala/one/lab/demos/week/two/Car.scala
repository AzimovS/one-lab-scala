package one.lab.demos.week.two

object Car {
  def apply(vehicleType: String, year: Int, color: String): Car = new Car(vehicleType, year, color)
  def apply(vehicleType: String, year: Int): Car                = new Car(vehicleType, year, "white")

  def repairCar(car: Car): Car =
    car

}

class Car(val vehicleType: String, val year: Int, val color: String) {}
