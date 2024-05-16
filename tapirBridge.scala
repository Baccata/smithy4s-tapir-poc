import smithy4s.Blob
import smithy4s.Endpoint.Middleware
import smithy4s.Service
import smithy4s.server.UnaryServerCodecs
import smithy4s.capability.MonadThrowLike
import smithy4s.http.CaseInsensitive
import smithy4s.http.*

import sttp.model.*
import sttp.monad.MonadError
import sttp.tapir.CodecFormat
import sttp.tapir.RawBodyType
import sttp.tapir.model.ServerRequest
import sttp.tapir.server.interceptor.RequestResult
import sttp.tapir.server.interpreter.RawValue
import sttp.tapir.server.interpreter.RequestBody
import sttp.tapir.server.interpreter.ThirdPartyInterpreter
import sttp.tapir.server.interpreter.ToResponseBody
import sttp.tapir.server.model.ServerResponse

import scala.util.control.NonFatal
import sttp.model.Uri.QuerySegment

implicit def monadErrorBridge[F[_]](implicit
    F: MonadError[F]
): MonadThrowLike[F] =
  new MonadThrowLike[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)
    def handleErrorWith[A](fa: F[A])(f: Throwable => F[A]): F[A] =
      F.handleError(fa) { case NonFatal(t) =>
        f(t)
      }
    def pure[A](a: A): F[A] = F.unit(a)
    def raiseError[A](e: Throwable): F[A] = F.error(e)
    def zipMapAll[A](seq: IndexedSeq[F[Any]])(f: IndexedSeq[Any] => A): F[A] =
      flatMap(seq.foldLeft(F.unit(Vector.empty[Any])) { case (acc, current) =>
        flatMap(acc)(vec => flatMap(current)(any => pure(vec :+ any)))
      })(vector => pure(f(vector)))
  }

// scalafmt: {maxColumn = 120}
def tapirBridge[Alg[_[_, _, _, _, _]], F[_]](
    impl: smithy4s.kinds.FunctorAlgebra[Alg, F],
    makeServerCodecs: UnaryServerCodecs.Make[F, HttpRequest[Blob], HttpResponse[Blob]],
    maxBytes: Option[Long] = None
)(implicit service: Service[Alg], F: MonadError[F]): ThirdPartyInterpreter[F] =
  new ThirdPartyInterpreter[F] {

    val smithy4sRouter = smithy4s.http
      .HttpUnaryServerRouter(service)(
        impl,
        makeServerCodecs,
        Middleware.noop,
        _.method,
        _.uri,
        (request, pathParams) => request.copy(uri = request.uri.copy(pathParams = Some(pathParams)))
      )

    def run[B, S](
        request: ServerRequest,
        fromRequestBody: RequestBody[F, S],
        toResponseBody: ToResponseBody[B, S]
    ): F[RequestResult[B]] = {
      val smithy4sMethod = request.method match {
        case Method.GET    => HttpMethod.GET
        case Method.POST   => HttpMethod.POST
        case Method.PUT    => HttpMethod.PUT
        case Method.PATCH  => HttpMethod.PATCH
        case Method.DELETE => HttpMethod.DELETE
        case Method(other) => HttpMethod.OTHER(other)
      }
      val uri = request.uri
      val smithy4sQueryParams = uri.querySegments
        .flatMap { segment =>
          segment match
            case QuerySegment.KeyValue(k, v, keyEncoding, valueEncoding) => List(k -> v)
            case QuerySegment.Value(v, encoding)                         => List(v -> "")
            case QuerySegment.Plain(v, encoding)                         => List.empty
        }
        .groupMap(_._1)(_._2)

      val smithy4sUri = HttpUri(
        scheme = uri.scheme match {
          case Some("http") | None => HttpUriScheme.Http
          case Some(_)             => HttpUriScheme.Https
        },
        host = uri.authority.map(_.host).getOrElse("localhost"),
        port = uri.authority.flatMap(_.port),
        path = uri.pathSegments.segments.map(_.v).toIndexedSeq,
        queryParams = smithy4sQueryParams,
        pathParams = None
      )
      val smithy4sHeaders = request.headers
        .map { header =>
          CaseInsensitive(header.name) -> header.value
        }
        .groupMap(_._1)(_._2)

      import sttp.monad.syntax.*
      def tapirResponse(smithy4sResponse: HttpResponse[Blob]): ServerResponse[B] = {
        val tapirHeaders = smithy4sResponse.headers.flatMap { case (key, values) =>
          values.map(Header(key.value, _))
        }.toSeq
        val maybeResponsBody =
          smithy4sResponse.headers.get(CaseInsensitive("content-type")).flatMap(_.headOption).map { contentType =>
            toResponseBody.fromRawValue(
              smithy4sResponse.body.toArray,
              sttp.model.Headers(Seq.empty),
              new CodecFormat { def mediaType: MediaType = MediaType.unsafeParse(contentType) },
              RawBodyType.ByteArrayBody
            )
          }
        ServerResponse(
          code = StatusCode(smithy4sResponse.statusCode),
          headers = tapirHeaders,
          body = maybeResponsBody,
          source = None
        )
      }

      for {
        requestBody <- fromRequestBody.toRaw(request, RawBodyType.ByteArrayBody, maxBytes)
        smithy4sRequest = HttpRequest(
          smithy4sMethod,
          smithy4sUri,
          smithy4sHeaders,
          Blob(requestBody.value)
        )
        tapirResult <- smithy4sRouter(smithy4sRequest) match {
          case None                    => F.unit(RequestResult.Failure(List.empty))
          case Some(smithy4sResponseF) => smithy4sResponseF.map(tapirResponse).map(RequestResult.Response(_))
        }
      } yield tapirResult
    }

  }
