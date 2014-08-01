package data.BiTree.hashList

trait HashList[+A,+B] {

	def allKeys: List[A] = {

	  def go(m: HashList[A,B], ac: List[A]): List[A] = m match {
      case Empty => Nil
      case EndL(k) => k::ac
      case Root(l,r) => go(l, ac)
      case Fork(k,l,r) => go(l, k::ac)
      case _ => ac
	  }
	  go(this, Nil)
	}

  def valForKey(s: String): Option[B] = this match {
    case Empty => None
    case Root(EndL(k),EndR(v)) => if (k == s) Some(v) else None
    case Root(l,EndR(v)) => l match {
      case Fork(k,_,_) => if (k == s) Some(v) else l.valForKey(s)
    }
    case Fork(_,l,EndR(v)) => l match {
      case EndL(k) => if (k == s) Some(v) else l.valForKey(s)
      case Fork(k,_,_) => if (k == s) Some(v) else l.valForKey(s)
    }
    case _ => None
  }
}

case object Empty extends HashList[Nothing,Nothing]
case class Root[A,B](left: HashList[A,B], right: HashList[A,B]) extends HashList[A,B]
case class EndL[A,B](value: A) extends HashList[A,B]
case class EndR[A,B](value: B) extends HashList[A,B]
case class Fork[A,B](value: A, left: HashList[A,B], right: HashList[A,B]) extends HashList[A,B]

object HashList {

	def apply[B](pairs: List[(String,B)]): HashList[String,B] = {

		def go(p: List[(String,B)], ac: HashList[String,B]): HashList[String,B] = p match {
			case (a,b)::xs => ac match {
				case Empty => go(xs, Root(EndL(a), EndR(b)))
				case Root(l,r) => go(xs, Root(Fork(a,l,r), EndR(b)));
				case _ => ac
			}
      case _ => ac
		}
    pairs match {
      case Nil => Empty
      case _ => go(pairs, Empty)
    }
	}

  def foldRight[B](h: HashList[String,B], ac: B)(f: (B, B) => B): B = h match {
    case Empty => ac
    case EndR(v) => f(v, ac)
    case Root(l,EndR(v)) => f(foldRight(l, ac)(f), f(v, ac))
    case Fork(_,l,EndR(v)) => f(foldRight(l, ac)(f), f(v, ac))
    case _ => ac
  }

  def foldLeft[B](h: HashList[String,B], ac: B)(f: (B, B) => B): B = h match {
    case Empty => ac
    case EndR(v) => f(v, ac)
    case Root(l,EndR(v)) => foldLeft(l, f(v, ac))(f)
    case Fork(_,l,EndR(v)) => foldLeft(l, f(v, ac))(f)
    case _ => ac
  }

}

import HashList._

object HashListStat extends App {

  val l = List(("two",2),("three",3),("four",4))
	val hl = HashList(l)

	println(hl.allKeys)

	println( "sums with foldRight: " + foldRight(hl, 0)( (x,y) => x+y) )
  println( "sums with foldLeft: " + foldLeft(hl, 0)( (x,y) => x+y) )

  println("value for key(four): " + hl.valForKey("four").getOrElse("not found"))
  println("value for key(two): " + hl.valForKey("two").getOrElse("not found"))

}