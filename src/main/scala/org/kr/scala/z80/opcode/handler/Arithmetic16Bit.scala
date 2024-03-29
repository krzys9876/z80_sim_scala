package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, Flag, Regs, Z80System}
import org.kr.scala.z80.utils.{IntValue, OptionInt, Z80Utils}

object Arithmetic16Bit extends OpCodeHandler {
  // Z80 manual page 52
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (Z80System, Int, Int) = {
    val actualCode=castType[OpCode with OpCodeArithmetic16b with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize with OpCodeTCycles](code)
    val oper = actualCode.operation
    val sourceLoc = actualCode.source
    val destLoc = actualCode.destination

    val calcInput = ArithmeticOpInput(
      system.getValueFromLocation(destLoc),
      IntValue(system.getValueFromLocation(sourceLoc)),
      system.getFlags)

    val (result, flags) = oper.calcAll(calcInput)

    (system
      .putValueToLocation(destLoc, result.valueOut)
      .changeRegister(Regs.F,flags()),
      actualCode.size, actualCode.t)
  }
}


object Add16b extends ArithmeticOperation with ArithmeticCalculatorWord
  with FlagHCarryWord with FlagNReset with FlagCCarry {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value + input.operand())
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawWordTo2Compl(input.value) + Z80Utils.rawWordTo2Compl(input.operand()))
  override def calcAux(input: ArithmeticOpInput): OptionInt = IntValue((input.value & 0x0FFF) + (input.operand() & 0x0FFF))
}

object AddC16b extends ArithmeticOperation with ArithmeticCalculatorWord
  with FlagSSignWord with FlagZZero with FlagHCarryWord with FlagPOverflowWord with FlagNReset with FlagCCarry {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value + input.operand() + input.flags.flagValue(Flag.C))
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawWordTo2Compl(input.value) + Z80Utils.rawWordTo2Compl(input.operand()) + input.flags.flagValue(Flag.C))
  override def calcAux(input: ArithmeticOpInput): OptionInt =
    IntValue((input.value & 0x0FFF) + (input.operand() & 0x0FFF) + input.flags.flagValue(Flag.C))
}

object SubC16b extends ArithmeticOperation with ArithmeticCalculatorWord
  with FlagSSignWord with FlagZZero with FlagHBorrow with FlagPOverflowWord with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value - input.operand() - input.flags.flagValue(Flag.C))
  override def calcSigned(input: ArithmeticOpInput): OptionInt =
    IntValue(Z80Utils.rawWordTo2Compl(input.value) - Z80Utils.rawWordTo2Compl(input.operand()) - input.flags.flagValue(Flag.C))
  override def calcAux(input: ArithmeticOpInput): OptionInt =
    IntValue((input.value & 0x0FFF) - (input.operand() & 0x0FFF) - input.flags.flagValue(Flag.C))
}

object Inc16b extends ArithmeticOperation with ArithmeticCalculatorWord {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value + 1)
  override def calcSigned(input: ArithmeticOpInput): OptionInt = IntValue(Z80Utils.rawWordTo2Compl(input.value) + 1)
}

object Dec16b extends ArithmeticOperation with ArithmeticCalculatorWord {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue(input.value - 1)
  override def calcSigned(input: ArithmeticOpInput): OptionInt = IntValue(Z80Utils.rawWordTo2Compl(input.value) - 1)
}
