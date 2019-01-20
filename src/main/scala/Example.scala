object Example {
  // Example sensitive model ADT
  sealed trait Sensitive
  final case class Secret[A](a: A) extends Sensitive
  case object Redacted extends Sensitive

  // A typeclass that describes data that can be redacted
  trait Redactable[A] {
    def redact(a: A): A
  }

  object Redactable {
    def apply[A](implicit r: Redactable[A]): Redactable[A] = r

    def instance[A](func: A => A): Redactable[A] = new Redactable[A] {
      override def redact(a: A): A = func(a)
    }
    def clearText[A]: Redactable[A] = instance(identity)
  }

  // Instances of Redactable
  object Instances {
    import shapeless._

    // Define instances for cleartext types
    implicit val stringRedactable: Redactable[String] =
      Redactable.clearText[String]
    implicit val intRedactable: Redactable[Int] = Redactable.clearText[Int]

    // Define custom redactables for sensitive container models
    implicit val sensitiveRedactable: Redactable[Sensitive] =
      Redactable.instance(_ => Redacted)
    new Redactable[Sensitive] {
      override def redact(s: Sensitive): Sensitive = Redacted
    }

    // Instance for HNil case
    implicit val hnilRedactable: Redactable[HNil] = Redactable.clearText[HNil]

    // Instance for HList case
    implicit def hlistRedactable[H, T <: HList](
        implicit hRedactable: Redactable[H],
        tRedactable: Redactable[T]
    ): Redactable[H :: T] = Redactable.instance[H :: T] {
      case h :: t => hRedactable.redact(h) :: tRedactable.redact(t)
    }

    implicit val cnilRedactable: Redactable[CNil] =
      Redactable.instance[CNil](_ => ???)

    implicit def coproductRedactable[H, T <: Coproduct](
        implicit hRedactable: Redactable[H],
        tRedactable: Redactable[T]): Redactable[H :+: T] =
      Redactable.instance[H :+: T] {
        case Inl(h) => Inl(hRedactable.redact(h))
        case Inr(t) => Inr(tRedactable.redact(t))
      }

    // Generic derived instance for ADTs
    implicit def genericRedactable[A, R](
        implicit gen: Generic.Aux[A, R],
        r: Lazy[Redactable[R]]): Redactable[A] =
      Redactable.instance[A](a => gen.from(r.value.redact(gen.to(a))))
  }

  // A syntax for making redacting easy
  object RedactableSyntax {
    implicit class AnyRedactableSyntax[A](a: A) {
      def redact(implicit redactable: Redactable[A]) = redactable.redact(a)
    }
  }

  // Example usage of redaction
  def main(args: Array[String]): Unit = {
    import RedactableSyntax._
    import Instances._

    // Example response model ADT that will need redacting
    sealed trait NestedCoproduct
    final case class Nested(someOtherCleartextVal: String,
                            someOtherSensitive: Sensitive)
        extends NestedCoproduct
    final case class Nested2(b: Int) extends NestedCoproduct
    final case class Response(someCleartextVal: String,
                              someSensitive: Sensitive,
                              nested: NestedCoproduct,
                              nested2: NestedCoproduct)

    val redacted = Response("clear",
                            Secret("secret"),
                            Nested("clear", Secret("secret")),
                            Nested2(6)).redact
    assert(
      redacted == Response("clear",
                           Redacted,
                           Nested("clear", Redacted),
                           Nested2(6)))
  }
}
