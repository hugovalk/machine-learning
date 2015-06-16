

import scala.util.Random

object TSPSimulation extends App {

  println("Welcome to the TSP simulation.")

  import City._
  import com.devdiscoveries.ml.genalg.GenAlgSolver._

  val tspPopulation = for {
    i <- 1 to 100
  } yield {
    new CandidateSolution(City.baseCity, Random.shuffle(City.cities))
  }
  val tspFitnessFunction: FitnessFunction[CandidateSolution] = c => c.fitness
  val tspRecombineFunction = ???
  val tspMutateFunction: MutateFunction[CandidateSolution] = (candidateSolution) => {
    def findOther(first: Int, size: Int): Int = {
      val second = Random.nextInt(size)
      if (first == second) findOther(first, size)
      else second
    }
    val r1 = Random.nextInt(candidateSolution.visitingCities.size)
    val r2 = findOther(r1, candidateSolution.visitingCities.size)
    candidateSolution.visitingCities.
    // need to swap the cities.
  }

  run(population = tspPopulation.toList,
    numberOfGenerations = 1,
    parentPoolSize = 10,
    parentSelectionSize = 6,
    mutationProbability = 0.1,
    fitnessThreshold = 10.0,
    fitnessFunction = tspFitnessFunction,
    recombineFunction = tspRecombineFunction,
    mutateFunction = tspMutateFunction)

}

case class CandidateSolution(baseCity: City, visitingCities: List[City]) {
  lazy val route = baseCity :: visitingCities.toList ++ List(baseCity)
  lazy val fitness = calculateFitness

  def calculateFitness(): Double = {
    def loop(rest: List[City], result: List[(Double, City)]): List[(Double, City)] = {
      if (rest.size <= 1) result
      val elem = (City.calculateDistance(rest.head, rest.tail.head), rest.head)
      loop(rest.tail, elem :: result)
    }
    loop(route, List()).map(_._1).sum
  }
}

case class City(name: String, lat: Double, lon: Double)
object City {
  val baseCity = new City("Amsterdam", 52.370216, 4.89516);

  val cities = List(
    new City("Athens", 37.975334, 23.736151),
    new City("Bern", 46.947922, 7.444608),
    new City("Berlin", 52.519173, 13.406091),
    new City("Bratislava", 48.146240, 17.107262),
    new City("Brussels", 50.850342, 4.351710),
    new City("Bucharest", 44.437710, 26.097366),
    new City("Budapest", 47.498405, 19.040758),
    new City("Copenhagen", 55.676098, 12.568337),
    new City("Dublin", 53.344105, -6.267494),
    new City("Helsinki", 60.166588, 24.943556),
    new City("Lisbon", 38.706932, -9.135632),
    new City("London", 51.508129, -0.128005),
    new City("Luxemburg", 49.611622, 6.131935),
    new City("Madrid", 40.416691, -3.700345),
    new City("Oslo", 59.913868, 10.752245),
    new City("Prague", 50.087811, 14.420460),
    new City("Rome", 41.890518, 12.494249),
    new City("Sofia", 42.696491, 23.326012),
    new City("Stockholm", 59.328930, 18.064911),
    new City("Vienna", 48.208176, 16.373819),
    new City("Warsaw", 52.229675, 21.012230))

  def calculateDistance(city1: City, city2: City): Double = {
    import scala.language.postfixOps
    val earthRadius = 3958.75
    val dLat = Math.toRadians(city2.lat - city1.lat)
    val dLng = Math.toRadians(city2.lon - city1.lon)
    val a = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.cos(Math.toRadians(city1.lat)) *
      Math.cos(Math.toRadians(city2.lat)) *
      Math.sin(dLng / 2) * Math.sin(dLng / 2)
    val c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))
    Math.round(earthRadius * c * 1.609344)
  }
}