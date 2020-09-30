package countdown.rest

import cats.Applicative
import cats.implicits._
import com.typesafe.config.{Config, ConfigFactory}
import countdown.{AsCountdownConfig, CountdownConfig, Equation}
import ga.GeneticAlgo.Generation
import io.circe.Json

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
  type CountdownRequest = Json

  object CountdownRequest {
    def example =
      """{
        |  target : 100
        |  from : [1,2,3,4]
        |  maxPopulationSize : 200
        |  minEquationSize : 3
        |  maxEquationSize : 0
        |  maxGenerations : 200
        |  mutationProbability : 0.01
        |  seed : 1234
        |  debug : false
        |}""".stripMargin
  }

  def forConfig[F[_] : Applicative](config: Config): Service[F] = {
    new Service[F] {
      override def countdown(request: CountdownRequest): F[CountdownResponse] = {
        val workingsOut = ListBuffer[String]()
        val newConf = prepareConfig(config, request, workingsOut)
        newConf.solve() match {
          case None => CountdownResponse(None, workingsOut.toList).pure[F]
          case Some(result) =>
            implicit val show = Equation.showForTarget(newConf.targetValue)
            val str = show.show(result.value)
            CountdownResponse(Option(str), workingsOut.toList).pure[F]
        }
      }
    }
  }

  def prepareConfig(config: Config, request: CountdownRequest, workingsOut: ListBuffer[String]): CountdownConfig = {
    val userConfig = ConfigFactory.parseString(request.noSpaces).withFallback(config)
    val countdownConfig: CountdownConfig = AsCountdownConfig(userConfig)
    if (userConfig.getBoolean("debug")) {
      countdownConfig
    } else {
      countdownConfig.copy(debug =
        (input: Generation[Equation]) => {
          val (generation, geneology) = input
          workingsOut ++= (s"Generation $generation") +: geneology.map(_.toString)
        }
      )
    }
  }
}