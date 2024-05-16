import smithy4s.json.*
import smithy4s.http.*
import smithy4s.Blob
import sttp.monad.MonadError
import smithy4s.codecs.BlobEncoder
import smithy4s.capability.MonadThrowLike

//****************************************************
// THIS SHOULD FACTORED OUT IN SMITHY4S ITSELF
//****************************************************
private val hintMask =
  alloy.SimpleRestJson.protocol.hintMask

val jsonCodecs = Json.payloadCodecs
  .withJsoniterCodecCompiler(
    Json.jsoniter
      .withHintMask(hintMask)
      .withMaxArity(2048)
  )

private val payloadEncoders: BlobEncoder.Compiler =
  jsonCodecs.encoders

private val payloadDecoders =
  jsonCodecs.decoders

// Adding X-Amzn-Errortype as well to facilitate interop with Amazon-issued code-generators.
private val errorHeaders = List(
  smithy4s.http.errorTypeHeader,
  smithy4s.http.amazonErrorTypeHeader
)

def simpleRestJsonCodecs[F[_]: MonadThrowLike] = {
  val baseResponse = HttpResponse(200, Map.empty, Blob.empty)
  HttpUnaryServerCodecs
    .builder[F]
    .withBodyDecoders(payloadDecoders)
    .withSuccessBodyEncoders(payloadEncoders)
    .withErrorBodyEncoders(payloadEncoders)
    .withErrorTypeHeaders(errorHeaders*)
    .withMetadataDecoders(Metadata.Decoder)
    .withMetadataEncoders(Metadata.Encoder)
    .withBaseResponse(_ => MonadThrowLike[F].pure(baseResponse))
    .withResponseMediaType("application/json")
    .withWriteEmptyStructs(!_.isUnit)
    .build()
}
