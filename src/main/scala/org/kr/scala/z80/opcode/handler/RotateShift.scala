package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, Flag, RegisterChange, Regs, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object RotateShift extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChangeBase], Int) = {
    val actualCode=castType[OpCode with OpCodeRotateShift with OpCodeSourceLocation with OpCodeSize](code)

    val oper = actualCode.operation
    val instrSize = actualCode.size
    val loc = actualCode.source
    val prevValue = system.getValueFromLocation(loc)
    val prevFlags = system.getFlags

    val (result, flags) = oper.calcAll(ArithmeticOpInput(prevValue, OpCode.ANY, prevFlags))

    (List(
      system.putValueToLocation(loc, result.valueOut),
      new RegisterChange(Regs.F, flags())
    ),
      instrSize)
  }
}

class RotateLeftBase(override val name:String) extends ArithmeticOperation(name) with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCBit7

class RotateLeftBaseAccum(override val name:String) extends ArithmeticOperation(name) with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCBit7

class RotateRightBase(override val name:String) extends ArithmeticOperation(name) with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCBit0

class RotateRightBaseAccum(override val name:String) extends ArithmeticOperation(name) with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCBit0

object RotShRlc extends RotateLeftBase("RLC") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value << 1) & 0xFF) + (if(Z80Utils.getBit(input.value,7)) 1 else 0)
}

object RotShRlca extends RotateLeftBaseAccum("RLCA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = RotShRlc.calcUnsigned(input)
}

object RotShRrc extends RotateRightBase("RRC") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value >> 1) & 0xFF) + (if(Z80Utils.getBit(input.value,0)) 0x80 else 0)
}

object RotShRrca extends RotateRightBaseAccum("RRCA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = RotShRrc.calcUnsigned(input)
}

object RotShRl extends RotateLeftBase("RL") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value << 1) & 0xFF) + input.flags.flagValue(Flag.C)
}

object RotShRla extends RotateLeftBaseAccum("RL") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = RotShRl.calcUnsigned(input)
}

object RotShRr extends RotateRightBase("RR") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value >> 1) & 0xFF) + (input.flags.flagValue(Flag.C) << 7)
}

object RotShRra extends RotateRightBaseAccum("RRA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = RotShRr.calcUnsigned(input)
}

object RotShSla extends RotateLeftBase("SLA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = (input.value << 1) & 0xFF
}

object RotShSra extends RotateRightBase("SRA") {
  override def calcUnsigned(input: ArithmeticOpInput): Int =
    ((input.value >> 1) & 0xFF) + (if(Z80Utils.getBit(input.value,7)) 0x80 else 0)
}

object RotShSrl extends RotateRightBase("SRL") {
  override def calcUnsigned(input: ArithmeticOpInput): Int = (input.value >> 1) & 0xFF
}
