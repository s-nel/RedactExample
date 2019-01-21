import io.circe.Encoder
import shapeless.Lazy

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

    implicit def sensitiveEncoder[A, B](implicit aEncoder: Encoder[A], bEncoder: Encoder[B], serdesContext: Lazy[SerdesContext]): Encoder[Sensitive[A, B]] = {
      if(serdesContext.value.redact) {
        Encoder.instance[Sensitive[A, B]](s => bEncoder(s.redacted))
      } else {
        Encoder.instance[Sensitive[A, B]](s => aEncoder(s.value))
      }
    }
  }

  // Example response model ADT that will need redacting
  final case class Nested(
    someOtherCleartextVal: String,
    someOtherSensitive: Sensitive[String, String]
  )
  object Nested {
    import io.circe.generic.semiauto._
    // For any fields in `Nested` that have contextual encoding, we must provide the encoder lazily
    implicit def nestedEncoder(implicit a: Lazy[Encoder[Sensitive[String, String]]]): Encoder[Nested] = {
      implicit val sensitiveEncoder: Encoder[Sensitive[String, String]] = a.value
      deriveEncoder[Nested]
    }
  }
  final case class Response(
    someCleartextVal: String,
    someSensitive: Sensitive[String, String],
    nested: Nested
  )
  object Response {
    import io.circe.generic.semiauto._
    implicit def responseEncoder(implicit a: Lazy[Encoder[Sensitive[String, String]]]): Encoder[Response] = {
      implicit val sensitiveEncoder: Encoder[Sensitive[String, String]] = a.value
      deriveEncoder[Response]
    }
  }

  // Example usage of redaction using semi-auto derivation and Lazy
  def main(args: Array[String]): Unit = {

    import io.circe.parser._
    import io.circe.syntax._

    val response = Response("clear",
      Sensitive("secret", "******"),
      Nested("clear", Sensitive("secret", "redacted")))

    List(true, false).foreach { redact =>
      // Lazy allows us to provide the context here
      implicit val serdesContext: SerdesContext = SerdesContext(redact)
      if (redact) {
        val expected = parse("""
                               |{
                               |  "someCleartextVal" : "clear",
                               |  "someSensitive" : "******",
                               |  "nested" : {
                               |    "someOtherCleartextVal" : "clear",
                               |    "someOtherSensitive" : "redacted"
                               |  }
                               |}
                             """.stripMargin)
        println(expected)
        assert(response.asJson == expected.right.get)
      } else {
        val expected = parse("""
                               |{
                               |  "someCleartextVal" : "clear",
                               |  "someSensitive" : "secret",
                               |  "nested" : {
                               |    "someOtherCleartextVal" : "clear",
                               |    "someOtherSensitive" : "secret"
                               |  }
                               |}
                             """.stripMargin)
        println(expected)
        assert(response.asJson == expected.right.get)
      }
    }
  }
}
