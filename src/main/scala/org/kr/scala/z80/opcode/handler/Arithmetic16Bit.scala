package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object Arithmetic16Bit extends OperationSpec with OpCodeHandler {
  // Z80 manual page 52
  val operation: OpCodeMap[ArithmeticOperation] = new OpCodeMap(OpCodes.operation16bMap, None16b)
  val source: OpCodeMap[Location] = new OpCodeMap(OpCodes.sourceMap, Location.empty)
  val destination: OpCodeMap[Location] = new OpCodeMap(OpCodes.destinationMap, Location.empty)
  val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)
  override lazy val isOper: OpCode => Boolean = opcode => operation.contains(opcode)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val oper = operation.find(code)
    val sourceLoc = source.find(code)
    val destLoc = destination.find(code)

    val calcInput = ArithmeticOpInput(
      system.getValueFromLocation(destLoc),
      system.getValueFromLocation(sourceLoc),
      system.getFlags)

    val (result, flags) = oper.calcAll(calcInput)
    val chgList = List(system.putValueToLocation(destLoc, result.valueOut), new RegisterChange("F", flags()))

    (chgList, instSize.find(code))
  }
}


object Add16b extends ArithmeticOperation("ADD_16B") with ArithmeticCalculatorWord
  with FlagHCarryWord with FlagNReset with FlagCCarry {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value + input.operand
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawWordTo2Compl(input.value) + Z80Utils.rawWordTo2Compl(input.operand)
  override def calcAux(input: ArithmeticOpInput): Int = (input.value & 0x0FFF) + (input.operand & 0x0FFF)
}

object AddC16b extends ArithmeticOperation("ADD_CARRY_16B") with ArithmeticCalculatorWord
  with FlagSSignWord with FlagZZero with FlagHCarryWord with FlagPOverflowWord with FlagNReset with FlagCCarry {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value + input.operand + input.flags.flagValue(Flag.C)
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawWordTo2Compl(input.value) + Z80Utils.rawWordTo2Compl(input.operand) + input.flags.flagValue(Flag.C)
  override def calcAux(input: ArithmeticOpInput): Int =
    (input.value & 0x0FFF) + (input.operand & 0x0FFF) + input.flags.flagValue(Flag.C)
}

object SubC16b extends ArithmeticOperation("SUB_CARRY_16B") with ArithmeticCalculatorWord
  with FlagSSignWord with FlagZZero with FlagHBorrow with FlagPOverflowWord with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - input.operand - input.flags.flagValue(Flag.C)
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawWordTo2Compl(input.value) - Z80Utils.rawWordTo2Compl(input.operand) - input.flags.flagValue(Flag.C)
  override def calcAux(input: ArithmeticOpInput): Int =
    (input.value & 0x0FFF) - (input.operand & 0x0FFF) - input.flags.flagValue(Flag.C)
}

object Inc16b extends ArithmeticOperation("INC_16B") with ArithmeticCalculatorWord {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value + 1
  override def calcSigned(input: ArithmeticOpInput): Int = Z80Utils.rawWordTo2Compl(input.value) + 1
}

object Dec16b extends ArithmeticOperation("DEC_16B") with ArithmeticCalculatorWord {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - 1
  override def calcSigned(input: ArithmeticOpInput): Int = Z80Utils.rawWordTo2Compl(input.value) - 1
}
