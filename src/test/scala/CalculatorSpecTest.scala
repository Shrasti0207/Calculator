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
    val result = Await.result(Calculator.calculate("-", Seq(2, 3)), 1.second)
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
}
