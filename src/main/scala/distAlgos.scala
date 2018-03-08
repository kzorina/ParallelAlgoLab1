import my_utils._
object distAlgos {
  val n = 64
  // for input
  /*sealed abstract class Tree[Int]
  case class Leaf[Int](a: Int) extends Tree[Int]
  case class Node[Int](l: Tree[Int], r: Tree[Int]) extends Tree[Int]
*/
// for output 
  sealed abstract class TreeRes[Int] { val res: Int }
  case class Leaf[Int](from: Int, to: Int, override val res: Int) extends TreeRes[Int]
  case class Node[Int](left: TreeRes[Int], override val res: Int, right: TreeRes[Int]) extends TreeRes[Int]
  
  def up[A](inp: Array[Int], from: Int, to: Int, f: (Int,Int) => Int): TreeRes[Int] = { //bottom-up built of binary tree
    if (to - from < 2) Leaf(from, to, reduceSeg1(inp, from + 1, to, inp(from), f)) // Base case for small segment where we don't need to use parallelism
    else { // parallel case
      val mid = from + (to - from)/2
      val (tL,tR) = parallel(up(inp, from, mid, f), up(inp, mid, to, f))
      Node(tL, f(tL.res,tR.res), tR)
    }
  }
  def reduceSeg1[A](inp: Array[Int], left: Int, right: Int, a0: Int, f: (Int,Int) => Int): Int = {
    var a = a0
    var i = left
    while (i < right) {
      a = f(a, inp(i))
      i = i + 1
    }
    a
  }
  def scanLeftSeg[A](inp: Array[Int], left: Int, right: Int, a0: Int, f: (Int,Int) => Int, out: Array[Int]) = {
    if (left < right) {
      var i = left
      var a = a0
      while (i < right) {
        a = f(a, inp(i))
        i = i+1
        out(i) = a
      }
    }
  }

  def down(inp: Array[Int], a0: Int, f: (Int,Int) => Int, t: TreeRes[Int], out: Array[Int]): Unit = t match {
    case Leaf(from, to, res) => scanLeftSeg(inp, from, to, a0, f, out)
    case Node(l, _, r) => {
      val (_,_) = parallel(down(inp, a0, f, l, out), down(inp, f(a0,l.res), f, r, out))
    }

  } //compute output array

  def paralellScanLeft(inp: Array[Int], a0: Int, f:(Int,Int)=> Int, out: Array[Int]) = {
    val t = up(inp, 0, inp.length, f)
    down(inp, a0, f, t, out)
    out(0) = a0 //filing first element
    println(out.toList)
  }
  def main(args: Array[String]): Unit = {
    val list =  Array.fill(n)(1)
    val t0 = System.nanoTime()
    println(list.scanLeft(0)((prev,next) => prev + next).toList)
    /*
    2048 el - 193 345 840 ns (> then parallel)
    1024 el - 187 034 814 ns (> then parallel)
    64 el - 151 130 216 ns (> then parallel)
     */
    val t1 = System.nanoTime()
    println("Elapsed time (sequential scan): "+(t1-t0) + " ns")
    val array = Array.fill(n)(1)
    val array_out = Array.fill(n+1)(1)
    val t3 = System.nanoTime()
    paralellScanLeft(array, 0, ((prev,next) => prev + next), array_out)
    /*
    2048 el - 84 132 760 ns
    1024 el - 121 777 402 ns
    64 el - 66 753 704
    */
    val t4 = System.nanoTime()
    println("Elapsed time (parallel scan): "+(t4-t3) + " ns")

  }
}
