package org.kr.scala.z80.utils

//TODO: determine if this is needed
sealed abstract class OptionInt {
  def isAny:Boolean
  def apply():Int
}

case object AnyInt extends OptionInt {
  override def isAny:Boolean=true
  def apply():Int=throw new InvalidIntValueException("AnyInt does not have a value")
}

case class IntValue(value:Int) extends OptionInt {
  override def isAny:Boolean=false
  def apply():Int=value
}

class InvalidIntValueException(message : String) extends Exception(message)