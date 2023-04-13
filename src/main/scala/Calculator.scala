package com.knoldus
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class CalculatorException(message: String) extends Exception(message)
// A singleton object for the calculator that defines its methods
object Calculator {
  // This method takes an operator and a sequence of operands , returns a future
  def calculate(operator: String, operands: Seq[Double]): Future[Seq[Double]] = {
    operator match {
      case "+" => execute(Add, operands)
      case "-" => execute(Subtract, operands)
      case "*" => execute(Multiply, operands)
      case "/" => execute(Divide, operands)
      case "^" => execute(Power, operands)
      case "sqrt" => execute(SquareRoot, operands)
      case "!" => execute(Factorial, operands)
      case "sum" => execute(sum, operands)
      case "gcd" => execute(GCD, operands)
      case "even" => execute(IsEven, operands)
      case "odd" => execute(IsOdd, operands)
      case "fibonacci" => execute(Fibonacci, operands)
      case _ => Future.failed(CalculatorException("Invalid operator"))
    }
  }
  // This method takes two operands and returns whether their squares are equal or not
  def squareOfExpression(firstOperand: Double, secondOperand: Double): String = {
    val left = Math.pow((firstOperand + secondOperand), 2)
    val right = Math.pow(firstOperand, 2) + Math.pow(secondOperand, 2) + (2 * firstOperand * secondOperand)
    if (left == right) {
      "Equal"
    }
    else {
      "NotEqual"
    }
  }
  // This method takes a sequence of numbers and returns a Future containing a sequence of results
  def find(numbers: Seq[Double]): Future[Seq[Double]] = {
    // Recursive function to calculate the factorial of a number
    @tailrec
    def findFactorial(number: Double, accumulator: Double): Double = {
      if (number <= 1) accumulator
      else findFactorial(number - 1, accumulator * number)
    }
    val res = numbers.filter { num =>
      val res1 = findFactorial(num, 1)
      res1 > math.pow(6, num)
    }
    Future(res)
  }

  def findAverageAfterChainingOperations(numbers: Seq[Double]): Future[Double] = {
    Future {
      def fibonacci(times: Double, numberOne: Double, numberTwo: Double): Double = {
        if (times <= 1) numberTwo
        else fibonacci(times - 1, numberTwo, numberOne + numberTwo)
      }
      val filteredDataNumbers = numbers.filter { num =>
        val res = fibonacci(num.toInt, 0, 1)
        res % 2 != 0
      }
      filteredDataNumbers.foldLeft(0.0)((numOne: Double, numTwo: Double) => numOne + numTwo) / filteredDataNumbers.size
    }
  }

  private def execute(operator: Operator, operands: Seq[Double]): Future[Seq[Double]] = {
    if (operator.validate(operands)) {
      Future.successful(operator.validateAndExecute(operands))
    } else {
      Future.failed(CalculatorException("Invalid"))
    }
  }
}

object Add extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.length == 2
  override protected def execute(operands: Seq[Double]): Seq[Double] = Seq(operands.head + operands.last)
}

object Subtract extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.length == 2
  override protected def execute(operands: Seq[Double]): Seq[Double] = Seq(operands.last - operands.head)
}

object Multiply extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.length == 2
  override protected def execute(operands: Seq[Double]): Seq[Double] = Seq(operands.head * operands.last)
}

object Divide extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.length == 2 && operands.last != 0
  override protected def execute(operands: Seq[Double]): Seq[Double] = Seq(operands.head / operands.last)
}

object Power extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.length == 2

  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(Math.pow(operands(0), operands(1)))
  }
}

object SquareRoot extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.length == 1 && operands.head >= 0
  override protected def execute(operands: Seq[Double]): Seq[Double] = Seq(math.sqrt(operands.head))
}

object Factorial extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.length == 1 && operands.head >= 0 && operands.head == operands.head.toInt
  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    def factorial(n: Int): Int = {
      if (n == 0 || n == 1) {
        1
      }
      else {
        n * factorial(n - 1)
      }
    }
    Seq(factorial(operands.head.toInt))
  }
}

object sum extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.nonEmpty
  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    Seq(sumRecursive(operands))
  }
  private def sumRecursive(operands: Seq[Double]): Double = {
    if (operands.size == 1) {
      operands.head
    } else {
      operands.head + sumRecursive(operands.tail)
    }
  }
}
object GCD extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.length == 2
  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    def gcd(a: Double, b: Double): Double = {
      if (b == 0) a else gcd(b, a % b)
    }
    Seq(gcd(operands.head, operands.last))
  }
}

object IsEven extends ValidateOperator {
  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    operands.filter(x => x % 2 == 0)
  }
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.nonEmpty) true
    else false
  }
}

object IsOdd extends ValidateOperator {
  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    operands.filterNot(x => x % 2 == 0)
  }
  override def validate(operands: Seq[Double]): Boolean = {
    if (operands.nonEmpty) true
    else false
  }
}

object Fibonacci extends ValidateOperator {
  override def validate(operands: Seq[Double]): Boolean = operands.length == 1 && operands.head >= 0
  override protected def execute(operands: Seq[Double]): Seq[Double] = {
    val result = fibonacciRecursive(operands.head)
    result
  }
  private def fibonacciRecursive(number: Double): Seq[Double] = {
    if (number <= 0) Seq()
    else if (number == 1) Seq(0)
    else if(number == 2) Seq(0,1)
    else{
      val prevSeq = fibonacciRecursive(number - 1)
      prevSeq :+ (prevSeq.takeRight(2).sum)
    }
  }
}

