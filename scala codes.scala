object MyModule{
  def abc (n: Int): Int = 
    if (n<0) -n
    else n
  
    
  private def formatAbc(c:Int) = {
    val msg = "the absolute value of %d is %d"
    msg.format (c, abc(c))
  }
  def factorial (n: Int): Int = {
    def go(n: Int, acc: Int): Int = 
      if (n<=0)acc
      else go(n-1,n*acc)
    go(n,1)
  }
  private def formatFactorial(c:Int)={
    val msg_1 = "the factorial of %d is %d"
    msg_1.format(c, factorial(c))
  }
  def fib3(n: Int): Int = {
    def fib_tail(n: Int, a: Int, b: Int): Int = n match {
      case 0 => a
      case _ => fib_tail(n - 1, b, a + b)
  }
    return fib_tail(n, 0 , 1)
  }
  
  private def formatFibonacci(c:Int) ={
    val msg_2 = "The fibonacci number on index %d is %d"
    msg_2.format(c,fib3(c))
  }
  def main (args: Array[String]): Unit = 
    println(formatAbc(-42))
    println(factorial(5))
    println(formatFactorial(5))
    println(formatFibonacci(7))
    println(formatResult("fibonacci",7,fib3))
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg_3 = "the %s of %d is %d"
    msg_3.format(name, n, f(n) )
  }

}

//--------------------- ASSIGNMENT # 2-----------------------------------------------------------------

// TASK 2
char[] alpha = new char[26]
for(int i = 0; i < 26; i++){
    alpha[i] = (char)(97 + i)
}


// TASK 3
object Demo {
   def main(args: Array[String]): Unit = {
      val a = new Array[Int](5)
      val b = new Array[Int](5)
      val c = a.zip(b).map { case (x, y) => x + y }
      println(c)
  
   }
}


/ TASK 4
object ary{
  def main(args: Array[String]) : Unit = {  
        var IntArray = Array.fill(100)(util.Random.nextInt(100))
        var count:Int=0
        
        println("Even numbers are: ")
        while(count<IntArray.size)
        {
            if (IntArray(count)%2==0) 
            {
                printf("%d ",IntArray(count));
            }
            count=count+1
        }
        
        println()
    }
}



