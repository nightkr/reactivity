package reactivity

import org.scalatest.prop.TableFor2

class ObservableSpec extends PropUnitSpec {
  def bases: TableFor2[Observable.Mutable[Int], Observable[Int]] = Table(
    ("root", "modified"),
    (for {
      root <- Seq[Observable.Mutable[Int]](
        new Observable.Mutable(0),
        new Observable.Mutable(1)
      )
      modified <- Seq[Observable[Int] => Observable[Int]](
        identity,
        _.map(_ + 1)
      )
    } yield {
      val newRoot = root.mutableCopy
      (newRoot, modified(newRoot))
    }): _*
  )

  property("when changed it should update its observers") {
    forAll(bases) {
      (root, observable) =>
        val observer1 = mock[Observer]
        val observer2 = mock[Observer]

        observable.addObserver(observer1)
        observable.addObserver(observer2)

        (observer1.recalculate _).expects()
        (observer2.recalculate _).expects()

        root() += 1
    }
  }

  property("when changed it should update its value before notifying observers") {
    forAll(bases) {
      (root, observable) =>
        val origin = observable.get

        val observer = new Observer {
          override def recalculate() {
            assert(observable.get === origin + 1)
          }
        }

        observable.addObserver(observer)
        root() += 1
    }
  }

  property("when changed it should keep its new value") {
    forAll(bases) {
      (root, observable) =>
        val origin = observable.get
        assert(observable.get === origin)
        root() += 1
        assert(observable.get === origin + 1)
    }
  }

  property("when changed its dependency chain should be updated") {
    forAll(bases) {
      (root, observable) =>
        val myInt = new Observable.Mutable(42)
        val myStr = new Observable.Mutable("hello")

        val originRoot = root.get
        val originObservable = observable.get

        val newObservable = for {
          number <- root
          otherNumber <- observable
          thirdNumber <- myInt
          string <- myStr
        } yield (number, otherNumber, thirdNumber, string)

        assert(newObservable.get ===(originRoot, originObservable, 42, "hello"))
        root() += 1
        assert(newObservable.get ===(originRoot + 1, originObservable + 1, 42, "hello"))
        myInt() += 1
        assert(newObservable.get ===(originRoot + 1, originObservable + 1, 43, "hello"))
        myStr() += " there"
        assert(newObservable.get ===(originRoot + 1, originObservable + 1, 43, "hello there"))
    }
  }
}
