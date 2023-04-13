package com.knoldus
import scala.util.{Success,Failure}

import scala.concurrent.ExecutionContext.Implicits.global

object CalculatorDriver extends App{
  val resultofAddition = Calculator.calculate("+", Seq(2, 3))
  println(resultofAddition)
  val resultOfSubtraction = Calculator.calculate("-", Seq(2, 5))
  println(resultOfSubtraction)
  val resultofMultiplication = Calculator.calculate("*", Seq(2, 3))
  println(resultofMultiplication)
  val resultofdivision = Calculator.calculate("/", Seq(4, 2))
  println(resultofdivision)
  val resultofPower = Calculator.calculate("^", Seq(5, 2))
  println(resultofPower)
  val resultofSquareRoot = Calculator.calculate("sqrt", Seq(4))
  println(resultofSquareRoot)
  val resultofFactorial = Calculator.calculate("!", Seq(4))
  println(resultofFactorial)
  val resultofSum = Calculator.calculate("sum", Seq(4, 2, 3, 4))
  println(resultofSum)
  val resultOfGcd = Calculator.calculate("gcd", Seq(4, 20))
  println(resultOfGcd)
  val resultOfEven = Calculator.calculate("even", Seq(2, 4, 6, 7))
  val resultOfOdd = Calculator.calculate("odd", Seq(2, 3, 4, 5, 6, 7))
  println(resultOfEven)
  println(resultOfOdd)
  val resultOfFibonacci = Calculator.calculate("fibonacci", Seq(5))
  println(resultOfFibonacci)
  val resultOfSquareOfExpression = Calculator.squareOfExpression(2,8)
  println(resultOfSquareOfExpression)
  val resultOfFind = Calculator.find(Seq(100.0, 200.0))
  resultOfFind.onComplete{
    case Success(result) => println(result)
    case Failure(exception) => println(exception)
  }
  val resultOfFindAverageAfterChainingOperations = Calculator.findAverageAfterChainingOperations(Seq(3,5,7,9))
  resultOfFindAverageAfterChainingOperations.onComplete{
    case Success(result) => println(result)
    case Failure(exception) => println(exception)
  }
  Thread.sleep(100)
}
