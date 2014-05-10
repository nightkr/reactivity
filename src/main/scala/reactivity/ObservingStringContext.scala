package reactivity

import scala.language.implicitConversions
import reactivity.ObservingStringContext.{ObservableValue, StaticValue, MaybeObservable}
import reactivity.ObservingStringContexts.LowPrio

class ObservingStringContext(val ctx: StringContext) extends AnyVal {
  def os(interpolated: MaybeObservable[Any]*): Observable[String] = {
    import ctx._

    checkLengths(interpolated)

    //val observableParts = parts.map(Observable.Immutable(_))
    val observableInterpolated = interpolated.map(_.observable).zip(parts)

    observableInterpolated.foldRight[Observable[String]](Observable.Immutable(parts.last)) {
      case ((value, str), seed) =>
        for {
          seedValue <- seed
          current <- value
        } yield str + current + seedValue
    }
  }
}

object ObservingStringContext {

  trait MaybeObservable[+A] {
    def observable: Observable[A]
  }

  case class ObservableValue[+A](observable: Observable[A]) extends MaybeObservable[A]

  case class StaticValue[+A](value: A) extends MaybeObservable[A] {
    override lazy val observable: Observable[A] = new Observable.Mutable[A](value)
  }

}

trait ObservingStringContexts extends LowPrio {
  implicit def stringContext2observing(ctx: StringContext): ObservingStringContext = new ObservingStringContext(ctx)

  implicit def observable2maybeObservable[T](x: Observable[T]): MaybeObservable[T] = ObservableValue(x)
}

object ObservingStringContexts {

  trait LowPrio {
    implicit def any2maybeObservable[T](x: T): MaybeObservable[T] = StaticValue(x)
  }

}
