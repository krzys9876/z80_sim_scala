package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, RegisterChange, Regs, SystemChange, Z80System}
import org.kr.scala.z80.utils.{IntValue, OptionInt}

object RotateDigit extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChange], Int, Int) = {
    //http://www.z80.info/z80sflag.htm
    val actualCode=castType[OpCode with OpCodeRotateDigit with OpCodeDestLocation with OpCodeSize with OpCodeTCycles](code)

    val loc = actualCode.destination
    val oper = actualCode.operation

    // input: value -> accumulator, operand -> location
    // output: valueOut -> accumulator, valueAux -> location
    val (result, newF) = oper.calcAll(
      ArithmeticOpInput(system.getRegValue(Regs.A),
        system.getValueFromLocation(loc),
        system.getFlags))
    val change = List(
      new RegisterChange(Regs.A, result.valueOut),
      system.putValueToLocation(loc, result.valueAux()),
      new RegisterChange(Regs.F, newF()))

    (change, actualCode.size, actualCode.t)
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
  override def calcAux(input: ArithmeticOpInput): OptionInt = IntValue((digit2R(input) << 4) + digit2A(input))
  override def calcUnsigned(input: ArithmeticOpInput): Int = unchangedDigit1A(input) + digit1R(input)
}

object RotateDR extends RotateDigitBase("RLD") {
  override def calcAux(input: ArithmeticOpInput): OptionInt = IntValue((digit2A(input) << 4) + digit1R(input))
  override def calcUnsigned(input: ArithmeticOpInput): Int = unchangedDigit1A(input) + digit2R(input)
}