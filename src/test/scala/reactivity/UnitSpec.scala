package reactivity

import org.scalatest.{PropSpec, Suite, FunSpec}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.TableDrivenPropertyChecks

trait BaseUnitSpec extends MockFactory {
  this: Suite =>
}

abstract class UnitSpec extends FunSpec with BaseUnitSpec

abstract class PropUnitSpec extends PropSpec with TableDrivenPropertyChecks with BaseUnitSpec
