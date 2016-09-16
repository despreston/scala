/**
  * Created by despreston on 9/7/16.
  */
object HelloWorld {
    def main (args: Array[String]): Unit = {
        println("Hello World")
        println(factorial(5, 1))
        println(msort((x: Int, y: Int) => x < y)(List(5,7,1,3)))
    }

    def factorial(n:Int, accumulator:Int): Int = if(n==0) accumulator else factorial(n-1, n*accumulator)

    def msort[A](less: (A, A) => Boolean)(xs: List[A]): List[A] = {
        def merge(xs1: List[A], xs2: List[A]): List[A] =
            if (xs1.isEmpty) xs2
            else if (xs2.isEmpty) xs1
            else if (less(xs1.head, xs2.head)) xs.head :: merge(xs1.tail, xs2)
            else xs2.head :: merge(xs1, xs2.tail)
        val n = xs.length/2
        if (n == 0) xs
        else merge(msort(less)(xs take n), msort(less)(xs drop n))
    }
}
