import my_utils._
object mergeSort {
  def mergeIter(list1: List[Int], list2: List[Int]): List[Int] = (list1, list2) match{
    case(Nil, list2) => list2
    case(list1, Nil) => list1
    case(x :: list1_1, y :: list2_1) =>
      if (x < y) x :: mergeIter(list1_1,list2)
      else y :: mergeIter(list1, list2_1)
  }
  def mergeSortProc(list: List[Int]): List[Int] = {
    val middle = list.length/2
    if (list.length == 1) list
    else {
      val (left, right) = list splitAt(middle)
      mergeIter(mergeSortProc(left),mergeSortProc(right))
    }
  }
  def mergeSortProc_abitparallel(list: List[Int]): List[Int] = {
    val middle = list.length/2
    if (list.length == 1) list
    else {
      val (left, right) = list splitAt(middle)
      val (one, two) = parallel(mergeSortProc(left),mergeSortProc(right))
      mergeIter(one,two)
    }
  }


  def main(args: Array[String]): Unit = {
    val array = Array(1,2,6,7,3,4,8,9)
    val list = Array(1,2,6,7,3,4,8,9).toList
    val t0 = System.nanoTime()
    println(mergeSortProc(list))

    val t1 = System.nanoTime()
    println("Elapsed time (sequential scan): "+(t1-t0) + " ns")

    val t3 = System.nanoTime()
    println(mergeSortProc_abitparallel(list))
    val t4 = System.nanoTime()
    println("Elapsed time (parallel scan): "+(t4-t3) + " ns")

  }
}
