package com.knoldus
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.time.SpanSugar.convertIntToGrainOfTime

import scala.concurrent.{Await, Future}

class CalculatorSpecTest extends AnyFunSuite {
  test("Addition should return the correct result"){
    val result = Await.result(Calculator.calculate("+", Seq(2, 3)), 1.second)
    assert(result === Seq(5))
  }

  test("Subtraction should return the correct result"){
    val result = Await.result(Calculator.calculate("-", Seq(2,5)), 1.second)
    assert(result === Seq(3))
  }

  test("Multiplication should return the correct result") {
    val result = Await.result(Calculator.calculate("*", Seq(2, 3)), 1.second)
    assert(result === Seq(6))
  }

  test("Division should return the correct result") {
    val result = Await.result(Calculator.calculate("/", Seq(4, 2)), 1.second)
    assert(result === Seq(2))
  }

  test("Power should return the correct result") {
    val result = Await.result(Calculator.calculate("^", Seq(2, 5)), 1.second)
    assert(result === Seq(32))
  }

  test("squareRoot should return the correct result"){
    val result = Await.result(Calculator.calculate("sqrt", Seq(16)), 1.second)
    assert(result === Seq(4))
  }

  test("factorial should return the correct result") {
    val result = Await.result(Calculator.calculate("!", Seq(4)), 1.second)
    assert(result === Seq(24))
  }

  test("sum should return the correct result") {
    val result = Await.result(Calculator.calculate("sum", Seq(4,3,6,7)), 1.second)
    assert(result === Seq(20))
  }

  test("gcd should return the correct result") {
    val result = Await.result(Calculator.calculate("gcd", Seq(20,4)), 1.second)
    assert(result === Seq(4))
  }

  test("IsEven should return the correct result") {
    val result = Await.result(Calculator.calculate("even", Seq(2,3,4,5,6)), 1.second)
    assert(result === Seq(2,4,6))
  }

  test("IsOdd should return the correct result") {
    val result = Await.result(Calculator.calculate("odd", Seq(2,3,4,5,6)), 1.second)
    assert(result === Seq(3,5))
  }

  test("Fibonacci should return the correct result") {
    val result = Await.result(Calculator.calculate("fibonacci", Seq(7)), 1.second)
    assert(result === Seq(0,1,1,2,3,5,8))
  }

  test("square of expression should return the correct result") {
    val result = Calculator.squareOfExpression(2,5)
    assert(result === "Equal")
  }

  test("find should return the correct result"){
    val result = Await.result(Calculator.find(Seq(100,200)), 1.second)
    assert(result === Seq(100,200))
  }

  test("findAverageAfterChainingOperations should return the correct result"){
    val result = Await.result(Calculator.findAverageAfterChainingOperations(Seq(3,5,7,9)), 1.second)
    assert(result === 6.0)
  }
}
