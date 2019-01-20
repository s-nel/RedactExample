object Example2 {
  // Example sensitive model ADT
  final case class Sensitive[T, RedactedT](value: T, redacted: RedactedT)

  object RedactedInstances {
    import io.circe.Encoder

    implicit def redactedSensitiveEncoder[A, B](
        implicit e: Encoder[B]): Encoder[Sensitive[A, B]] =
      Encoder.instance[Sensitive[A, B]](s => e(s.redacted))
  }

  object ExposedInstances {
    import io.circe.Encoder

    implicit def exposedSenstiveEncoder[A, B](
        implicit e: Encoder[A]): Encoder[Sensitive[A, B]] =
      Encoder.instance[Sensitive[A, B]](s => e(s.value))
  }

  // Example usage of redaction
  def main(args: Array[String]): Unit = {
    import io.circe.generic.auto._
    import io.circe.parser._
    import io.circe.syntax._

    // Example response model ADT that will need redacting
    final case class Nested(
        someOtherCleartextVal: String,
        someOtherSensitive: Sensitive[String, String]
    )
    final case class Response(
        someCleartextVal: String,
        someSensitive: Sensitive[String, String],
        nested: Nested
    )

    val response = Response("clear",
                            Sensitive("secret", "******"),
                            Nested("clear", Sensitive("secret", "redacted")))

    List(true, false).foreach { redact =>
      if (redact) {
        import RedactedInstances._
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
        assert(response.asJson == expected.right.get)
      } else {
        import ExposedInstances._
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
        assert(response.asJson == expected.right.get)
      }
    }
  }
}
