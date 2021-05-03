package u06lab.code

import scala.collection.immutable.{AbstractSeq, LinearSeq}

/**
  * 1) Implement trait Functions with an object FunctionsImpl such that the code
  * in TryFunctions works correctly.
 */

trait Functions {
  def sum(a: List[Double]): Double
  def concat(a: Seq[String]): String
  def max(a: List[Int]): Int // gives Int.MinValue if a is empty
  def combine[A](a: Iterable[A])(implicit combiner: Combiner[A]): A
}

object FunctionsImpl extends Functions {

  override def sum(a: List[Double]): Double = {
    var acc = 0.0
    a.foreach(el => acc = acc + el)
    acc
  }

  override def concat(a: Seq[String]): String = {
    var acc = ""
    a.foreach(el => acc = acc + el)
    acc
  }

  override def max(a: List[Int]): Int = {
    var max = Integer.MIN_VALUE
    a.foreach(el => max=if (el>max) el else max)
    max
  }

  //metodo combine riesce a fare quello che fanno i metodi sopra, passando un combiner
  override def combine[A](a: Iterable[A])(implicit combiner: Combiner[A]): A = { //per rendere implicito il combiner
    var acc = combiner.unit
    a.foreach(el => acc = combiner.combine(acc,el))
    acc
  }
}

trait Combiner[A] {
  def unit: A
  def combine(a: A, b: A): A
}



object TryFunctions extends App {
  val f: Functions = FunctionsImpl
  println(f.sum(List(10.0,20.0,30.1))) // 60.1
  println(f.sum(List()))                // 0.0
  println(f.concat(Seq("a","b","c")))   // abc
  println(f.concat(Seq()))              // ""
  println(f.max(List(-10,3,-5,0)))      // 3
  println(f.max(List()))                // -2147483648

  implicit val sumCombiner = new Combiner[Double] {
    override def combine(a: Double, b: Double): Double = a+b
    override def unit: Double = 0.0
  }

  implicit val concatCombiner = new Combiner[String] {
    override def combine(a: String, b: String): String = a+b
    override def unit: String = ""
  }

  implicit val maxCombiner = new Combiner[Int] {
    override def combine(a: Int, b: Int): Int = if (a>b) a else b
    override def unit: Int = Integer.MIN_VALUE
  }


  println(f.combine(List(10.0,20.0,30.1))(sumCombiner)) // 60.1
  println(f.combine(List())(sumCombiner))                // 0.0
  println(f.combine(Seq("a","b","c"))(concatCombiner))   // abc
  println(f.combine(Seq())(concatCombiner))              // ""
  println(f.combine(List(-10,3,-5,0))(maxCombiner))      // 3
  println(f.combine(List())(maxCombiner))                // -2147483648


  println(f.combine(List(10.0,20.0,30.1))) // 60.1
  println(f.combine(List[Double]()))       // 0.0 <-Va specificato il tipo altrimenti non sa quale combiner prendere

  println(f.combine(Seq("a","b","c")))   // abc
  println(f.combine(Seq[String]()))      // "" <-Va specificato il tipo

  println(f.combine(List(-10,3,-5,0)))      // 3
  println(f.combine(List[Int]()))           // -2147483648   <-Va specificato il tipo

}