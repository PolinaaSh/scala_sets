

object Main extends App {
  type Set = Int => Boolean
  override def main(args: Array[String]): Unit = {
    val x = singletonSet(5)
    val y = singletonSet(9)

    println(toString(union(x, y)))
    println(toString(intersect(x, y)))
    println(toString(diff(x, y)))
  }

  def contains(s : Set, elem : Int) = {
    s(elem)
  }

  def singletonSet(value: Int) = {
    (res:Int) => res == value
  }

  def union(x: Set, y: Set) = {
    (res:Int) => contains(x, res) || contains(y, res)
  }

  def intersect(x: Set, y: Set) = {
    (res:Int) => contains(x, res) && contains(y, res)
  }

  def diff(x: Set, y: Set) = {
    (res:Int) => contains(x, res) && !contains(y, res)
  }

  def filter(x : Set, p : Int => Boolean) = {
    (res:Int) => p(res)
  }

  def toString(s: Set): String = {
    val bound = 1000
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }

  def forall(s:Set, p :Int => Boolean):Boolean = {
    val bound = 1000
    def iter(a:Int):Boolean={
      if(a > bound) true
      else if (contains(s, a) && !p(a)) false
      else iter(a+1)
    }
    iter(-bound)
  }

  def exists(s:Set, p :Int => Boolean) = {
    forall(s,x => !p(x))
  }

  def map(s: Set, f: Int => Int): Set = {
    val bound = 1000
    def iterate(s:Set, accumulateSet: Set, a:Int): Set = {
      if (a > bound) accumulateSet
      else if (contains(s, a)) iterate(s, union(accumulateSet, x=>f(a) == x), a+1)
      else iterate(s, accumulateSet, a+1)
    }
    iterate(s, x=>false, -3)


  }
}
