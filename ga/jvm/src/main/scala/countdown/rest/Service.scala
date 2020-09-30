package countdown.rest

import cats.Applicative
import cats.implicits._
import com.typesafe.config.Config
import countdown.{AsCountdownConfig, CountdownConfig, Equation}
import ga.GeneticAlgo.Generation
import ga.{AlgoSettings, Seed}

import scala.collection.mutable.ListBuffer

trait Service[F[_]] {

  import Service._

  def countdown(request: CountdownRequest): F[CountdownResponse]

}

object Service {

  final case class CountdownResponse(
                                      solution: Option[String],
                                      workingsOut: List[String]
                                    )

  object CountdownResponse {
    implicit val codec =
      io.circe.generic.semiauto.deriveCodec[CountdownResponse]
  }

  /*
   * Actually what would be easier would be to just take a block of json, overlay it on top of our typesafe Config
   * and be done with it rather than duplicate it here (and hit issues w/ e.g. mutationProbability)
   */
  final case class CountdownRequest(targetNumber: Int,
                                    inputNumbers: Set[Int],
                                    seed: Option[Long] = None,
                                    maxPopulationSize: Option[Int] = None,
                                    minEquationSize: Option[Int] = None,
                                    maxEquationSize: Option[Int] = None,
                                    maxGenerations: Option[Int] = None,
                                    //                                    mutationProbability: Option[Double] = None,
                                    includeWorkingsOut: Boolean = false)

  object CountdownRequest {
    implicit val codec = io.circe.generic.semiauto.deriveCodec[CountdownRequest]

    def example = CountdownRequest(
      targetNumber = 378,
      inputNumbers = Set(25, 3, 8, 12, 9, 15),
      seed = Option(12345),
      maxPopulationSize = Option(200),
      minEquationSize = Option(3),
      maxEquationSize = Option(10),
      maxGenerations = Option(200)
    )
  }

  def forConfig[F[_] : Applicative](config: Config): Service[F] = {
    apply(AsCountdownConfig(config))
  }

  def apply[F[_] : Applicative](templateConfig: CountdownConfig): Service[F] =
    new Service[F] {
      override def countdown(
                              request: CountdownRequest): F[CountdownResponse] = {


        val workingsOut = ListBuffer[String]()
        val newConf = updateConfig(templateConfig, request) { input: Generation[Equation] =>
          if (request.includeWorkingsOut) {
            val (generation, geneology) = input
            workingsOut ++= (s"Generation $generation") +: geneology.map(_.toString)
          }
        }

        newConf.solve() match {
          case None => CountdownResponse(None, workingsOut.toList).pure[F]
          case Some(result) =>
            implicit val show =
              Equation.showForTarget(newConf.targetValue)

            val str = show.show(result.value)
            CountdownResponse(Option(str), workingsOut.toList
            ).pure[F]
        }
      }
    }

  def updateConfig(oldConf: CountdownConfig, request: CountdownRequest)(debug: Generation[Equation] => Unit) = {
    val newRand =
      request.seed.map(Seed.apply).getOrElse(oldConf.rand)

    val newSettings: AlgoSettings[Equation] = oldConf.settings.withSizes(
      request.maxPopulationSize,
      request.maxGenerations
    )

    oldConf.copy(
      settings = newSettings,
      rand = newRand,
      inputValues = request.inputNumbers,
      targetValue = request.targetNumber,
      minEquationSize = request.minEquationSize.getOrElse(oldConf.minEquationSize),
      maxEquationSize = request.maxEquationSize.getOrElse(oldConf.maxEquationSize),
      debug = debug
    )
  }
}
