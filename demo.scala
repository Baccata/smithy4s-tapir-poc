import sttp.tapir.server.jdkhttp.*

import smithy4s.deriving.{*, given}
import smithy4s.deriving.aliases.*
import scala.annotation.experimental
import sttp.monad.MonadError
import java.net.InetSocketAddress
import com.sun.net.httpserver._

@experimental
@simpleRestJson
class HelloWorld derives API {

  @httpGet("/hello/{name}")
  def greet(@httpLabel name: String): String = s"Hello $name!"

}

@main
@experimental
def main(): Unit = {

  // copied from Tapir
  // TODO put the equivalent `MonadThrowLike` somewhere
  implicit val idMonad: MonadError[Id] = new MonadError[Id] {
    override def unit[T](t: T): Id[T] = t
    override def map[T, T2](fa: Id[T])(f: T => T2): Id[T2] = f(fa)
    override def flatMap[T, T2](fa: Id[T])(f: T => Id[T2]): Id[T2] = f(fa)
    override def error[T](t: Throwable): Id[T] = throw t
    override protected def handleWrappedError[T](rt: Id[T])(
        h: PartialFunction[Throwable, Id[T]]
    ): Id[T] = rt
    override def eval[T](t: => T): Id[T] = t
    override def ensure[T](f: Id[T], e: => Id[Unit]): Id[T] =
      try f
      finally e
  }

  val handler =
    JdkHttpServerInterpreter(JdkHttpServerOptions.Default).toHandler(
      tapirBridge(new HelloWorld().liftService[Id], simpleRestJsonCodecs[Id])
    )

  val socketAddress = new InetSocketAddress("localhost", 8080)
  val server = HttpServer.create(socketAddress, 64)
  server.createContext(JdkHttpServerOptions.Default.basePath, handler)
  server.start()
  println(s"Running on : ${server.getAddress()}")

}
