/**
  * Example3 shows that semi-auto derivation is possible using a context object that is defined where the encoder is used
  */
object Example3 {
  // Example context object
  final case class SerdesContext(redact: Boolean)

  // Example sensitive model ADT
  final case class Sensitive[T, RedactedT](value: T, redacted: RedactedT)
  object Sensitive {
    import io.circe.Encoder
    import shapeless.Lazy

    // Here we have "contextual encoding". The encoder makes decisions based on the `SerdesContext`, but we don't have
    // to have the context in scope until the encoder is actually used
    implicit def sensitiveEncoder[A, B](
        implicit aEncoder: Encoder[A],
        bEncoder: Encoder[B],
        serdesContext: Lazy[SerdesContext]): Encoder[Sensitive[A, B]] = {
      if (serdesContext.value.redact) {
        Encoder.instance[Sensitive[A, B]](s => bEncoder(s.redacted))
      } else {
        Encoder.instance[Sensitive[A, B]](s => aEncoder(s.value))
      }
    }
  }

  // Example response model ADT that will need redacting
  final case class Response(
      someCleartextVal: String,
      someSensitive: Sensitive[String, String]
  )
  object Response {
    import io.circe.Encoder
    import io.circe.generic.semiauto._
    import shapeless.Lazy

    // Our Response encoder. It's a bit more verbose here, but I don't think it's that bad. Will only need to do this
    // when one of children uses contextual encoding
    implicit def responseEncoder(
        implicit a: Lazy[Encoder[Sensitive[String, String]]])
      : Encoder[Response] = {
      implicit val sensitiveEncoder: Encoder[Sensitive[String, String]] =
        a.value
      deriveEncoder[Response]
    }
  }

  // Example usage of redaction using semi-auto derivation and Lazy
  def main(args: Array[String]): Unit = {
    import io.circe.parser._
    import io.circe.syntax._

    val response = Response("clear", Sensitive("secret", "******"))

    List(true, false).foreach { redact =>
      // Here we finally provide the context for encoding. Lazy allows everything to wire up correctly. If we don't
      // define the context, compilation will fail
      implicit val serdesContext: SerdesContext = SerdesContext(redact)
      if (redact) {
        val expected = parse("""
                               |{
                               |  "someCleartextVal" : "clear",
                               |  "someSensitive" : "******"
                               |}
                             """.stripMargin)
        println(response.asJson)
        assert(response.asJson == expected.right.get)
      } else {
        val expected = parse("""
                               |{
                               |  "someCleartextVal" : "clear",
                               |  "someSensitive" : "secret"
                               |}
                             """.stripMargin)
        println(response.asJson)
        assert(response.asJson == expected.right.get)
      }
    }
  }
}
