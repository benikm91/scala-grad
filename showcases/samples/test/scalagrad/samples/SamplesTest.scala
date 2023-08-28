// NeuralNetworkMNISTest.scala
import org.scalatest.flatspec.*
import org.scalatest.matchers.*
import scalagrad.*
import org.scalatest.wordspec.AnyWordSpec

class SamplesTest extends AnyWordSpec with should.Matchers {
  val functionsToTest = List(
    ("complexExampleClass", scalagrad.complexExampleClass),
    ("complexExampleTuple2Tuple", scalagrad.complexExampleTuple2Tuple),
    ("complexExampleTuple2TupleDual", scalagrad.complexExampleTuple2TupleDual),
    ("forwardModeExampleSquare", scalagrad.forwardModeExampleSquare),
    ("forwardModeExampleSwap", scalagrad.forwardModeExampleSwap),
    ("higherOrderExampleSquare", scalagrad.higherOrderExampleSquare),
    ("mixtureSample", scalagrad.mixtureSample),
    ("reverseModeExampleSquare", scalagrad.reverseModeExampleSquare),
    ("reverseModeExampleSwap", scalagrad.reverseModeExampleSwap),
  )

  for ((name, function) <- functionsToTest) {
    name should {
      "run without exceptions" in {
        noException should be thrownBy {
          function
        }
      }
    }
  }

}
