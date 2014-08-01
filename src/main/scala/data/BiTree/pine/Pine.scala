package data.BiTree.pine

trait Pine[+A] {

  def inOrderTrav[B](f: A => (B => B))(ac: B): B = this match {
	case End(a) => f(a)(ac)
    case Fork(a,l,r) => 
    	r.inOrderTrav (f) (f(a) (l.inOrderTrav (f) (ac) )) 
  }

  def preOrderTrav[B](f: A => B => B)(ac: B): B = this match {
	case End(a) => f(a)(ac)
    case Fork(a,l,r) => 
    	r.preOrderTrav (f) (l.preOrderTrav (f) ( f(a) (ac) ))
  }

  def postOrderTrav[B](f: A => B => B)(ac: B): B = this match {
	case End(a) => f(a)(ac)
    case Fork(a,l,r) => 
    	f(a) (r.postOrderTrav (f) (l.postOrderTrav (f) (ac) ))
  }

}

case class End[A](value: A) extends Pine[A]
case class Fork[A](value: A, left: Pine[A] , right: Pine[A]) extends Pine[A]

object Pine {

	def apply[A](v: A, l: Pine[A], r: Pine[A]): Pine[A] = {
    Fork(v,l,r)
  }

}

object PineStat extends App {

  val pine = Pine(1, End(2), Fork(3, Fork(4, End(5), End(6)), End(7)))

  def r01[A](x: A)(xs:List[A]) = x :: xs

  def r02(x: Int)(c: Int) = x + c

  val result1 = pine.inOrderTrav ( r01  ) (Nil)
  val result2 = pine.preOrderTrav ( r01 ) (Nil)
  val result3 = pine.postOrderTrav ( r01 ) (Nil)
  val sum = pine.postOrderTrav ( r02 ) (0)

  println("pine tree traverse in order"+result1)
  println("pine tree traverse pre order"+result2)
  println("pine tree traverse post order"+result3)
  println("the sum is: " + sum)

}
