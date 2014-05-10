package reactivity

class ObservableStringContextSpec extends UnitSpec {
  it("should produce a string observable filled with regular interpolated data") {
    val b = 55
    val d = 60
    assert(os"a $b c $d e".get === "a 55 c 60 e")
  }

  it("should produce a string observable filled with observable data") {
    val b = Observable.Immutable(55)
    val d = Observable.Immutable(60)
    assert(os"a $b c $d e".get === "a 55 c 60 e")
  }

  it("should be updated whenever a dependency changes") {
    val b = new Observable.Mutable(55)
    val d = new Observable.Mutable(60)
    val str = os"a $b c $d e"
    assert(str.get === "a 55 c 60 e")
    b() += 1
    assert(str.get === "a 56 c 60 e")
    d() += 1
    assert(str.get === "a 56 c 61 e")
  }
}
