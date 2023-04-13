package com.knoldus

trait Validator {
  def validate(operands: Seq[Double]): Boolean
}

trait Operator extends Validator {
  def validateAndExecute(operands: Seq[Double]): Seq[Double]

  protected def execute(operands: Seq[Double]): Seq[Double]
}

trait ValidateOperator extends Operator {
  override def validateAndExecute(operands: Seq[Double]): Seq[Double] = {
    if (validate(operands)) execute(operands)
    else throw new IllegalArgumentException("Invalid")
  }

  override def validate(operands: Seq[Double]): Boolean

  override protected def execute(operands: Seq[Double]): Seq[Double]
}
