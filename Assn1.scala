import scala.collection.immutable.ListMap

/* Part 2 */

def incr(x: Int): Int = x + 1
def double(x: Int): Int = x + x
def square(x: Int): Int = x * x

def factorial(n: Int): Int = 
  if (n == 0) {1} else {n * factorial(n-1)}


def power(x: Int, n: Int): Int =
  if (n == 0) {1} else {x * power(x,n-1)}

def factorial1(n: Int): Int = {
  val m = n-1 ; if (n == 0) {1} else {n * factorial1(m)}
}

def factorial2(n: Int): Int = {
  val m = n-1;
  if (n == 0) {1} else {n * factorial2(m)}
}


def factorial3(n: Int): Int = {
  val m = n-1;
  if (n == 0) {
    return 1;
  } else {
    return n * factorial3(m);
  }
}

/* Exercise 1 */
def p(x: Int, y:Int): Int = x^2+2*x*y+y^3-1

/* Exercise 2 */
def sum(n: Int): Int = {
  val m = n-1; if (n==0) {0} else {n + sum(m)}
}

/* Part 3 */

/* Exercise 3 */
def cycle(q:(Int,Int,Int)): (Int,Int,Int) = (q._2, q._3, q._1)

/* Part 4 */

def nameFromNum(presidentNum: Int): String = presidentNum match {
  case 41 => "George H. W. Bush"
  case 42 => "Bill Clinton"
  case 43 => "George W. Bush"
  case 44 => "Barack Obama"
  case 45 => "Donald J. Trump"
  case n => "I don't know who president number " + n + " is"
}

def numFromName(presidentName: String): Int = presidentName match {
  case "George H. W. Bush" => 41
  case "Bill Clinton" => 42
  case "George W. Bush" => 43
  case "Barack Obama" => 44
  case "Donald J. Trump" => 45
}

/* Exercise 4 */
def suffix(n: Int): String = {
  if (n%100 == 11 || n%100 == 12 || n%100 == 13){
    return "th"
  } else {
    n%10 match {
      case 1 => "st";
      case 2 => "nd";
      case 3 => "rd";
      case _ => "th";
    }
  }
}


abstract class Colour
case class Red() extends Colour
case class Green() extends Colour
case class Blue() extends Colour

/* Exercise 5 */
def favouriteColour(c: Colour): Boolean = c match { 
  case Red() => false;
  case Blue() => true;
  case Green() => false;
  case _ => false;
}

abstract class Shape
case class Circle(r: Int, x: Int, y: Int) extends Shape
case class Rectangle(llx: Int, lly: Int, w:Int, h:Int) extends Shape

def center(s: Shape): (Int,Int) = s match {
  case Rectangle(llx,lly,w,h) => (llx+w/2, lly+h/2)
  case Circle(r,x,y) => (x,y)
}

/* Exercise 6 */
def boundingBox(s: Shape): Rectangle = s match {
  case Rectangle(llx,lly,w,h) => Rectangle(llx,lly,w,h);
  case Circle(r, x, y) => Rectangle(x-r, y-r, 2*r, 2*r);
}

/* Exercise 7 */
def rectangleCornerCoordinates(rect: Rectangle): (Int, Int, Int, Int) = {
  val lx = rect.llx;
  val ly = rect.lly + rect.h;
  val rx = rect.llx + rect.w;
  val ry = rect.lly;

  return (lx, ly, rx, ry);
}

def mayOverlap(s1: Shape, s2: Shape): Boolean = {
  val r1: Rectangle = boundingBox(s1);
  val r2: Rectangle = boundingBox(s2);

  val (r1_lx, r1_ly, r1_rx, r1_ry) = rectangleCornerCoordinates(r1);
  val (r2_lx, r2_ly, r2_rx, r2_ry) = rectangleCornerCoordinates(r2);

  if (r1_lx > r2_rx || r2_lx > r1_rx || r1_ry > r2_ly || r2_ry > r1_ly){
    return false;
  }
  return true;
}


/* Part 5 */

val anonIncr = (x: Int) => x+1 // anonymous version of incr
val anonAdd = (x: Int) => (y: Int) => x + y

/* Exercise 8 */
def compose1[A, B, C](f: A => B, g: B => C)(x: A) = g(f(x));

/* Exercise 9 */
def compose[A, B, C](f: A => B, g: B => C): A => C = {
  x => g(f(x));
}

/* Exercise 10 */
def e1(x: Int): String = x.toString()
def e2(s: String): Boolean = s.length > 0

/*
Result for compose(e1,e2): val res1: Int => Boolean = Lambda$2279/0x0000000800ae6040@695df41a
Result for compose(e2,e1): Type mismatch
*/

def isEmpty[A](l: List[A]) = l match { 
  case Nil => true
  case x :: y => false
}

/* Exercise 11 */
def map[A, B](f: A => B, l: List[A]): List[B] = l match {
  case Nil => Nil;
  case head :: tail => f(head) :: map(f, tail);
}

/* Exercise 12 */
def filter[A](f: A => Boolean, l: List[A]): List[A] = l match {
  case Nil => Nil;
  case head :: tail => 
    if (f(head)) {
      head :: filter(f, tail);
    } else {
      filter(f, tail);
    }
}

/* Exercise 13 */
def reverse[A](l: List[A]): List[A] = l match {
  case Nil => Nil;
  case head :: tail => reverse(tail) :+ head;
}

/* Part 6 */

def empty[K,V]: List[(K,V)] = List()

/* Exercise 14 */
def lookup[K, V](m: List[(K, V)], k: K): V = m match {
  case Nil => sys.error(s"Key $k not found")
  case (key, value) :: tail => 
    if (key == k) {
      value
    } else {
      lookup(tail, k)
    }
}

/* Exercise 15 */
//RUN
def update[K, V](m: List[(K, V)], k: K, v: V): List[(K, V)] = m match {
  case Nil => sys.error(s"Key $k not found")
  case (key, value) :: tail =>
    if (key == k){
      (key,v) :: tail
    } else {
      (key, value) :: update(tail, k, v)
    }
}

/* Exercise 16 */
def keys[K,V](m: List[(K,V)]): List[K] = m match {
  case Nil => Nil;
  case (key, value) :: tail => key :: keys(tail);
}

/* Exercise 17 */
val presidentListMap: ListMap[Int, String] = ListMap(
  41 -> "George H. W. Bush",
  42 -> "Bill Clinton",
  43 -> "George W. Bush",
  44 -> "Barack Obama",
  45 -> "Donald J. Trump",
  46 -> "Joseph R. Biden"
) 

/* Exercise 18 */

def map12_withUpdate: List[(Int, String)] = {
  val emptyMap: List[(Int, String)] = List.empty
  update(update(update(emptyMap, 1, "a"), 2, "b"), 3, "c")
}

/* Exercise 19 */
def list2map[K,V](l: List[(K,V)]): ListMap[K,V] = l match {
  case Nil => ListMap.empty[K, V];
  case (key, value) :: tail => list2map(tail) + (key -> value);
}

/* Exercise 20 */
def election(votes: List[String]): ListMap[String,Int] = {
  def helper(votes: List[String], currMap: ListMap[String, Int]): ListMap[String, Int] = votes match {
    case Nil => currMap;
    case head :: tail =>
      val updatedMap = if (currMap.contains(head)){
        currMap + (head -> (currMap(head) + 1)) //Increment count
      } else {
        currMap + (head -> 1)
      }
      helper(tail, updatedMap)
  }
  helper(votes, ListMap.empty[String, Int]) //Start by calling helper on empty list
}
