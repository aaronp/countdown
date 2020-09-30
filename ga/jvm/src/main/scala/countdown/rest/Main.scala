package countdown.rest

import args4c.implicits._
import cats.effect.{ExitCode, IO, IOApp}
import com.typesafe.config.{Config, ConfigFactory}

object Main extends IOApp {

  def configSummary(config: Config) = {
    config
      .without(
        ConfigFactory
          .systemEnvironment()
          .withFallback(ConfigFactory.systemProperties())
      )
      .summary()
  }

  def run(args: List[String]): IO[ExitCode] = {
    val config = args.toArray.asConfig().resolve()
    config.showIfSpecified() match {
      case None =>
        for {
          _ <- IO(println(s"Running with\n${configSummary(config)}\n"))
          exitCode <- CountdownServer
            .stream[IO](config)
            .compile
            .drain
            .as(ExitCode.Success)
        } yield exitCode
      case Some(showMe) => {
        IO {
          println(showMe)
          ExitCode.Success
        }
      }
    }
  }
}
