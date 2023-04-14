package com.knoldus

import scala.util.{Success, Failure}
import scala.concurrent.ExecutionContext.Implicits.global

object CalculatorDriver extends App {
  private val addition = Calculator.calculate("+", Seq(2, 3))
  println(addition)
  private val subtraction = Calculator.calculate("-", Seq(2, 5))
  println(subtraction)
  private val multiplication = Calculator.calculate("*", Seq(2, 3))
  println(multiplication)
  private val division = Calculator.calculate("/", Seq(4, 2))
  println(division)
  private val power = Calculator.calculate("^", Seq(5, 2))
  println(power)
  private val squareRoot = Calculator.calculate("sqrt", Seq(4))
  println(squareRoot)
  private val factorial = Calculator.calculate("!", Seq(4))
  println(factorial)
  private val Sum = Calculator.calculate("sum", Seq(4, 2, 3, 4))
  println(Sum)
  private val gcd = Calculator.calculate("gcd", Seq(4, 20))
  println(gcd)
  private val isEven = Calculator.calculate("even", Seq(2, 4, 6, 7))
  println(isEven)
  private val isOdd = Calculator.calculate("odd", Seq(2, 3, 4, 5, 6, 7))
  println(isOdd)
  private val fibonacci = Calculator.calculate("fibonacci", Seq(7))
  println(fibonacci)
  private val squareOfExpression = Calculator.squareOfExpression(2, 8)
  println(squareOfExpression)
  private val find = Calculator.find(Seq(100.0, 200.0))
  find.onComplete {
    case Success(result) => println(result)
    case Failure(exception) => println(exception)
  }
  private val findAverageAfterChainingOperations = Calculator.findAverageAfterChainingOperations(Seq(3,5,7,9))
  findAverageAfterChainingOperations.onComplete {
    case Success(result) => println(result)
    case Failure(exception) => println(exception)
  }
  Thread.sleep(100)
  // for invalid operator
  val resultOfAddition = Calculator.calculate("@" , Seq(2,3))
  println(resultOfAddition)
}
