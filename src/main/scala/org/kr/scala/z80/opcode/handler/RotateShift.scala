package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, Flag, Regs, Z80System}
import org.kr.scala.z80.utils.{AnyInt, IntValue, OptionInt, Z80Utils}

object RotateShift extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (Z80System, Int, Int) = {
    val actualCode=castType[OpCode with OpCodeRotateShift with OpCodeSourceLocation with OpCodeSize with OpCodeTCycles](code)

    val oper = actualCode.operation
    val loc = actualCode.source
    val prevValue = system.getValueFromLocation(loc)
    val prevFlags = system.getFlags

    val (result, flags) = oper.calcAll(ArithmeticOpInput(prevValue, AnyInt, prevFlags))

    (system
      .putValueToLocation2(loc, result.valueOut)
      .changeRegister(Regs.F, flags()),
      actualCode.size,actualCode.t)
  }
}

class RotateLeftBase extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCBit7

class RotateLeftBaseAccum extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCBit7

class RotateRightBase extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCBit0

class RotateRightBaseAccum extends ArithmeticOperation with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCBit0

object RotShRlc extends RotateLeftBase {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt =
    IntValue(((input.value << 1) & 0xFF) + (if(Z80Utils.getBit(input.value,7)) 1 else 0))
}

object RotShRlca extends RotateLeftBaseAccum {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = RotShRlc.calcUnsigned(input)
}

object RotShRrc extends RotateRightBase {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt =
    IntValue(((input.value >> 1) & 0xFF) + (if(Z80Utils.getBit(input.value,0)) 0x80 else 0))
}

object RotShRrca extends RotateRightBaseAccum {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = RotShRrc.calcUnsigned(input)
}

object RotShRl extends RotateLeftBase {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt =
    IntValue(((input.value << 1) & 0xFF) + input.flags.flagValue(Flag.C))
}

object RotShRla extends RotateLeftBaseAccum {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = RotShRl.calcUnsigned(input)
}

object RotShRr extends RotateRightBase {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt =
    IntValue(((input.value >> 1) & 0xFF) + (input.flags.flagValue(Flag.C) << 7))
}

object RotShRra extends RotateRightBaseAccum {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = RotShRr.calcUnsigned(input)
}

object RotShSla extends RotateLeftBase {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue((input.value << 1) & 0xFF)
}

object RotShSra extends RotateRightBase {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt =
    IntValue(((input.value >> 1) & 0xFF) + (if(Z80Utils.getBit(input.value,7)) 0x80 else 0))
}

object RotShSrl extends RotateRightBase {
  override def calcUnsigned(input: ArithmeticOpInput): OptionInt = IntValue((input.value >> 1) & 0xFF)
}
