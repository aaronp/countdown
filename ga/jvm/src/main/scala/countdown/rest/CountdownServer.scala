package countdown.rest

import cats.effect.{ConcurrentEffect, ContextShift, Timer}
import com.typesafe.config.Config
import fs2.Stream
import org.http4s.implicits.http4sKleisliResponseSyntaxOptionT
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.server.middleware.Logger

import scala.concurrent.ExecutionContext.global

object CountdownServer {

  def stream[F[_]: ConcurrentEffect](config: Config)(
      implicit T: Timer[F],
      C: ContextShift[F]): Stream[F, Nothing] = {
    val host = config.getString("rest.host")
    val port = config.getInt("rest.port")
    val service = Service.forConfig[F](config)

    stream(host, port, service)
  }

  def stream[F[_]: ConcurrentEffect](host: String,
                                     port: Int,
                                     service: Service[F])(
      implicit T: Timer[F],
      C: ContextShift[F]): Stream[F, Nothing] = {
    val httpApp = CountdownRoutes[F](service).orNotFound
    val finalHttpApp = Logger.httpApp(true, true)(httpApp)

    for {
      exitCode <- BlazeServerBuilder[F](global)
        .bindHttp(port, host)
        .withHttpApp(finalHttpApp)
        .serve
    } yield exitCode
  }.drain
}
