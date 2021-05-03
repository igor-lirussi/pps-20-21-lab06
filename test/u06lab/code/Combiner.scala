package u06lab.code

import org.junit.jupiter.api.{Assertions, Test}
import Assertions._

class CombinerTests {


  object ImplicitCombiners {

    implicit object sumCombiner extends Combiner[Double] {
      override def unit: Double = 0.0
      override def combine(a: Double, b: Double): Double = a + b
    }

    implicit object concatCombiner extends Combiner[String] {
      override def unit: String = ""
      override def combine(a: String, b: String): String = a + b
    }

    implicit object maxCombiner extends Combiner[Int] {
      override def unit: Int = Int.MinValue
      override def combine(a: Int, b: Int): Int = if (a > b) a else b
    }
  }

  @Test
  def testFunctions() {
    val f: Functions = FunctionsImpl
    assertEquals(60.1, f.sum(List(10.0, 20.0, 30.1)), 0.001) // 60.1
    assertEquals(0.0, f.sum(List())) // 0.0
    assertEquals("abc", f.concat(Seq("a", "b", "c")))
    assertEquals("", f.concat(Seq()))
    assertEquals(3, f.max(List(-10, 3, -5, 0)))
    assertEquals( Integer.MIN_VALUE, f.max(List()))

    //passing the combiner
    import ImplicitCombiners._
    assertEquals(60.1, f.combine(List(10.0,20.0,30.1))(sumCombiner)) // 60.1
    assertEquals(0.0, f.combine(List())(sumCombiner))                // 0.0
    assertEquals("abc", f.combine(Seq("a","b","c"))(concatCombiner))   // abc
    assertEquals("", f.combine(Seq())(concatCombiner))              // ""
    assertEquals(3, f.combine(List(-10,3,-5,0))(maxCombiner))      // 3
    assertEquals( Integer.MIN_VALUE, f.combine(List())(maxCombiner))                // -2147483648

    //implicit combiner
    import ImplicitCombiners._
    assertEquals(60.1, f.combine(List(10.0,20.0,30.1))) // 60.1
    assertEquals(0.0, f.combine(List[Double]()))                // 0.0
    assertEquals("abc", f.combine(Seq("a","b","c")))   // abc
    assertEquals("", f.combine(Seq[String]()))              // ""
    assertEquals(3, f.combine(List(-10,3,-5,0)))      // 3
    assertEquals( Integer.MIN_VALUE, f.combine(List[Int]()))                // -2147483648
  }
}