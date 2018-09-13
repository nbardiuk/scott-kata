import Scott._
import org.scalatest._

class ScottSpec extends FlatSpec with Matchers {

  "The Option type" can "be cast to scala Option" in {
    toOption(none) should be(None)
    toOption(some(4)) should be(Some(4))
  }
  it can "be cast from scala Option" in {
    fromOption[Int](None)[Int](0, _ + 1) should be(0)
    fromOption(Some(4))[Int](0, _ + 1) should be(5)
  }
  it can "be predicated on its content" in {
    isNone(none) should be(true)
    isNone(some(4)) should be(false)
    isSome(none) should be(false)
    isSome(some(4)) should be(true)
  }
  it can "be flattened in lists" in {
    toList(catOptions(fromList(List(some(1), none, some(2))))) should be(List(1, 2))
  }

  "The List type" can "be cast to scala List" in {
    toList(nil[Int]) should be(List())
    toList(new SList[Int] {
      override def apply[B] = (_, f) => f(1, new SList[Int] {
        override def apply[B] = (_, g) => g(2, nil[Int])
      })
    }) should be(List(1, 2))
  }
  it can "be cast from scala List" in {
    fromList[Int](List())[Int](0, reduce) should be(0)
    fromList[Int](List(1, 2, 3))[Int](0, reduce) should be(321)
  }
  it can "have new elements appended" in {
    toList(cons(1, nil[Int])) should be(List(1))
    toList(hello) should be("hello".toList)
  }
  it can "be concatenated with other lists" in {
    toList(concat(digits, nil)) should be(toList(digits))
    toList(concat(nil, digits)) should be(toList(digits))
    toList(concat(digits, fromList((10 to 20).toList))) should be(0 to 20)
  }
  it can "be predicated on its content" in {
    Scott.empty(nil) should be(true)
    Scott.empty(cons(1, nil)) should be(false)
  }
  it can "compute its length" in {
    Scott.length(nil) should be(0)
    Scott.length(hello) should be(5)
    Scott.length(digits) should be(10)
  }
  it can "be mapped over" in {
    Scott.empty(map[Int, Int](_ * 10, nil)) should be(true)
    toList(map[Char, Char](c => (c + 1).toChar, hello)) should be("ifmmp".toList)
    toList(map[Int, Int](_ * 5, digits)) should be(0 to(45, 5))
  }
  it can "be zipped with other lists" in {
    Scott.empty(zip(digits, nil)) should be(true)
    Scott.empty(zip(nil, digits)) should be(true)
    toListPair(zip(digits, hello)) should be(List(0 -> 'h', 1 -> 'e', 2 -> 'l', 3 -> 'l', 4 -> 'o'))
  }
  it can "be left-folded" in {
    foldLeft[Int, Int]((a, _) => a, 0, nil) should be(0)
    foldLeft[Char, Int]((c, _) => c + 1, 0, hello) should be(Scott.length(hello))
    foldLeft[Int, Int]((c, _) => c + 1, 0, digits) should be(Scott.length(digits))
    foldLeft[Int, Int]((c, a) => c + a, 0, digits) should be(45)
    foldLeft[Char, String]((c, a) => a + c, "", hello) should be("olleh")
  }
  it can "be right-folded" in {
    foldRight[Int, Int]((_, a) => a, 0, nil) should be(0)
    foldRight[Char, Int]((_, c) => c + 1, 0, hello) should be(Scott.length(hello))
    foldRight[Int, Int]((_, c) => c + 1, 0, digits) should be(Scott.length(digits))
    foldRight[Int, Int]((a, c) => a + c, 0, digits) should be(45)
    foldRight[Char, String]((a, c) => a + c, "", hello) should be("hello")
  }
  it can "be reduced to an initial segment" in {
    toList(take(0, digits)) should be(List())
    toList(take(5, digits)) should be(0 to 4)
    toList(take(10, hello)) should be("hello".toList)
  }

  "The Either type" can "be cast to scala Either" in {
    toEither(new SEither[Int, String] {
      override def apply[C] = (left, _) => left(3)
    }) should be(Left(3))

    toEither(new SEither[Int, String] {
      override def apply[C] = (_, right) => right("hello")
    }) should be(Right("hello"))
  }
  it can "be cast from scala Either" in {
    fromEither[Int, String](Left(3))[String](_.toString, identity) should be("3")
    fromEither[Int, String](Right("hello"))[String](_.toString, identity) should be("hello")
  }
  it can "be predicated on its content" in {
    isLeft(left(0)) should be(true)
    isLeft(right("hello")) should be(false)
    isRight(left(0)) should be(false)
    isRight(right("hello")) should be(true)
  }
  it can "be used to partition a list" in {
    toPairList(partition(nil)) should be(List() -> List())
    toPairList(partition(helloDigits)) should be((0 to 9).toList -> "hello".toList)
  }

  "The tuple type" can "be cast to (,)" in {
    toTuple(new STuple[Int, String] {
      override def apply[C] = f => f(2, "hi")
    }) should be((2, "hi"))
  }
  it can "be cast from (,)" in {
    fromTuple((2, "hi"))[List[String]](List.fill(_)(_)) should be(List("hi", "hi"))
    fromTuple((3, 6))[Int](_ * _) should be(18)
  }
  it can "be projected from its components" in {
    fst(fromTuple(5, "hello")) should be(5)
    snd(fromTuple(5, "hello")) should be("hello")
  }
  it can "swap its components" in {
    toTuple(swap(fromTuple(5, "hello"))) should be(("hello", 5))
  }
  it can "be used to curry functions" in {
    curry[Int, Int, Int](sp => sp[Int](_ * _))(4)(5) should be(20)
    curry[Int, String, List[String]](sp => sp[List[String]](List.fill(_)(_)))(2)("hello") should be(List("hello", "hello"))
  }
  it can "be used to uncurry function" in {
    uncurry[Int, Int, Int](a => b => a * b)(fromTuple(4, 5)) should be(20)
    uncurry[Int, String, List[String]](a => b => List.fill(a)(b))(fromTuple(2, "hello")) should be(List("hello", "hello"))
  }

  def none[A]: SOption[A] = new SOption[A] {
    def apply[B] = (z, _) => z
  }

  def some[A](a: A): SOption[A] = new SOption[A] {
    def apply[B] = (_, f) => f(a)
  }

  def reduce(i: Int, is: SList[Int]): Int =
    i + 10 * is[Int](0, reduce)

  def hello: SList[Char] = fromList("hello".toList)

  def digits: SList[Int] = fromList((0 to 9).toList)

  def toListPair[A, B](sl: SList[STuple[A, B]]): List[(A, B)] =
    sl[List[(A, B)]](List(), (a, as) => toTuple[A, B](a) :: toListPair(as))

  def toPairList[A, B](sp: STuple[SList[A], SList[B]]): (List[A], List[B]) =
    sp[(List[A], List[B])]((as, bs) => (toList(as), toList(bs)))

  def left[A, B](a: A): SEither[A, B] = new SEither[A, B] {
    def apply[C] = (left, _) => left(a)
  }

  def right[A, B](b: B): SEither[A, B] = new SEither[A, B] {
    def apply[C] = (_, right) => right(b)
  }

  def helloDigits: SList[SEither[Int, Char]] = {
    def merge[A](left: List[A], right: List[A]): List[A] = (left, right) match {
      case (xs, Nil) => xs
      case (Nil, ys) => ys
      case (x :: xs, y :: ys) => x :: y :: merge(xs, ys)
    }

    fromList(merge((0 to 9).toList.map(left), "hello".toList.map(right)))
  }
}
