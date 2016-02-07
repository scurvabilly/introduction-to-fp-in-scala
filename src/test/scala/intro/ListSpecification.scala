package intro

import org.scalacheck._, Arbitrary._, Gen._, Prop._

object ListSpecification extends Properties("List") {
  /*
   * Example: Verify that that our Lists.length matches the
   * builtin List#size
   */
  property("Lists#length matches standard library") =
    forAll((xs: List[Int]) => Lists.length(xs) == xs.size)

  /* Exercise 1 */
  property("Lists#append - the length of the result is equal to the sum of the two input lengths") =
    forAll((xs: List[Int], ys: List[Int]) => Lists.append(xs, ys).size == (xs.size + ys.size))

  /* Exercise 2 */
  property("Lists#append - every element in the first and second list appears in the result") =
    forAll((xs: List[Int], ys: List[Int]) => {
      val appended = Lists.append(xs, ys)
      xs.forall(x => appended.contains(x)) && ys.forall(y => appended.contains(y))
    })

  /* Exercise 3 */
  property("Lists#filter - filter(_ => false) always gives empty list") =
    forAll((xs: List[Int]) => Lists.filter(xs)(_ => false).size == 0)

  /* Exercise 4 */
  property("Lists#filter - filter(_ => true) always gives input list") =
    forAll((xs: List[Int]) => Lists.filter(xs)(_ => true).equals(xs))

  /* Exercise 5 */
  property("Lists#filter - length of output is always less than length of input") =
    forAll((xs: List[Int], p: (Int => Boolean)) => Lists.filter(xs)(p).size <= xs.size)

  /* *Challenge* exercise 6
     Identify a set of properties that together with the type signature
     guarantees the validity of your reverse function (assuming pure-total FP) */
  property("Lists#reverse - the length of the output equals the length of the input") =
    forAll((xs: List[Int]) => Lists.reverse(xs).size == xs.size)

  property("Lists#reverse - elements at input indices (0,1..n) equal those at respective output indices (n,n-1,..0)") =
    forAll((xs: List[Int]) => Lists.reverse(xs).zipWithIndex.forall(e => e._1 == xs(xs.size - e._2 - 1)))

  /* *Challenge* exercise 7
     Identify a set of properties for testing sequence */
  property("Lists#sequence - the length of the output is equal to the input or is None") =
    forAll((xs: List[Option[Int]]) => {
      Lists.sequence(xs) match {
        case Some(vs) => vs.length == xs.length
        case None => true
      }
    })

  property("Lists#sequence - if all inputs have a value, all input values are within the output") =
    forAll((xs: List[Int]) =>
      Lists.sequence(xs.map(Some(_))) match {
        case Some(vs) => vs == xs
        case None => false
      }
    )

  property("Lists#sequence - if the input contains a None, the output will be None") =
    forAll((xs: List[Option[Int]]) =>
      if (xs.contains(None)) {
        Lists.sequence(xs) == None
      } else {
        true // Input is not applicable. Alternatively, could insert None at a random position?
      }
    )

  /* *Challenge* exercise 8
     Identify a set of properties for testing ranges */
  property("Lists#ranges...") =
    true

}
