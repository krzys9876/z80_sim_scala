package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{RegisterChange, SystemChangeBase, Z80System}

object RotateDigit extends OperationSpec with OpCodeHandler {
  val operation: OpCodeMap[ArithmeticOperation] = new OpCodeMap(OpCodes.rotateDigitMap, None8b)
  val location: OpCodeMap[Location] = new OpCodeMap(OpCodes.operandMap, Location.empty)
  override val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)
  override lazy val isOper: OpCode => Boolean = opcode => operation.contains(opcode)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    //http://www.z80.info/z80sflag.htm
    val loc = location.find(code)
    val oper = operation.find(code)

    // input: value => accumulator, operand => location
    // output: valueOut => accumulator, valueAux => location
    val (result, newF) = oper.calcAll(
      ArithmeticOpInput(system.getRegValue("A"),
        system.getValueFromLocation(loc),
        system.getFlags))
    val change = List(
      new RegisterChange("A", result.valueOut),
      system.putValueToLocation(loc, result.valueAux),
      new RegisterChange("F", newF()))

    (change, instSize.find(code))
  }
}

class RotateDigitBase(override val name:String) extends ArithmeticOperation(name) with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset {

  def unchangedDigit1A(input:ArithmeticOpInput):Int = input.value & 0xF0
  def digit2A(input:ArithmeticOpInput):Int = input.value & 0x0F
  def digit1R(input:ArithmeticOpInput):Int = (input.operand & 0xF0) >> 4
  def digit2R(input:ArithmeticOpInput):Int = input.operand & 0x0F
}

object RotateDL extends RotateDigitBase("RLD") {
  override def calcAux(input: ArithmeticOpInput): Int = (digit2R(input) << 4) + digit2A(input)
  override def calcUnsigned(input: ArithmeticOpInput): Int = unchangedDigit1A(input) + digit1R(input)
}

object RotateDR extends RotateDigitBase("RLD") {
  override def calcAux(input: ArithmeticOpInput): Int = (digit2A(input) << 4) + digit1R(input)
  override def calcUnsigned(input: ArithmeticOpInput): Int = unchangedDigit1A(input) + digit2R(input)
}