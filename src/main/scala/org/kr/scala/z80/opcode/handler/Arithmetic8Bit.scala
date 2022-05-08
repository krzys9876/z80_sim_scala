package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{ArithmeticCalculatorByte, ArithmeticOpInput, ArithmeticOperation, FlagCBorrow, FlagCCarry,
  FlagCInvert, FlagCReset, FlagCSet, FlagHBorrow, FlagHCarryByte, FlagHCopyC, FlagHReset, FlagHSet, FlagNReset, FlagNSet,
  FlagPOverflowByte, FlagPParity, FlagSSignByte, FlagZZero, Location, OpCode, OpCodeArithmetic8b, OpCodeDestLocation,
  OpCodeSize, OpCodeSourceLocation}
import org.kr.scala.z80.system.{Debugger, Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object Arithmetic8Bit extends OpCodeHandler {
  // Z80 manual page 50 (NOTE: ADD A,(HL) is 0x86, not 0x88!
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChangeBase], Int) = {
    val actualCode=castType[OpCode with OpCodeArithmetic8b with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize](code)

    val oper = actualCode.operation
    val instrSize = actualCode.size
    val destLoc = actualCode.destination
    val dest = system.getValueFromLocation(destLoc)
    val prevFlags = system.getFlags
    val sourceLocation = actualCode.source
    val prevValue = system.getValueFromLocation(sourceLocation)

    val (result, flags) = oper.calcAll(ArithmeticOpInput(prevValue, dest, prevFlags))
    val chgList = List(system.putValueToLocation(oper.getDestination(sourceLocation), result.valueOut))
    (chgList ++ List(new RegisterChange("F", flags())), instrSize)
  }
}

object Add8b extends ArithmeticOperation("ADD_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHCarryByte with FlagPOverflowByte with FlagNReset with FlagCCarry {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value + input.operand
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) + Z80Utils.rawByteTo2Compl(input.operand)
  override def calcAux(input: ArithmeticOpInput): Int = (input.value & 0x0F)+(input.operand & 0x0F)
}

object AddC8b extends ArithmeticOperation("ADD_8B_CARRY") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHCarryByte with FlagPOverflowByte with FlagNReset with FlagCCarry {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value + input.operand + input.flags.flagValue(Flag.C)
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) + Z80Utils.rawByteTo2Compl(input.operand) + input.flags.flagValue(Flag.C)
  override def calcAux(input: ArithmeticOpInput): Int =
    (input.value & 0x0F)+(input.operand & 0x0F)+input.flags.flagValue(Flag.C)
}

object Sub8b extends ArithmeticOperation("SUB_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - input.operand
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand)
  override def calcAux(input: ArithmeticOpInput): Int =
    (input.value & 0x0F)-(input.operand & 0x0F)
}

object SubC8b extends ArithmeticOperation("SUB_8B_CARRY") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - input.operand - input.flags.flagValue(Flag.C)
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand) - input.flags.flagValue(Flag.C)
  override def calcAux(input: ArithmeticOpInput): Int =
    (input.value & 0x0F) - (input.operand & 0x0F) - input.flags.flagValue(Flag.C)
}

object And8b extends ArithmeticOperation("AND_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHSet with FlagPParity with FlagNReset with FlagCReset {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value & input.operand
}

object Xor8b extends ArithmeticOperation("XOR_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCReset {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value ^ input.operand
}

object Or8b extends ArithmeticOperation("OR_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCReset {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value | input.operand
}

object Comp8b extends ArithmeticOperation("COMP_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - input.operand
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand)
  override def calcAux(input: ArithmeticOpInput): Int =
    (input.value & 0x0F)-(input.operand & 0x0F)

  override def getDestination(source:Location):Location=Location.empty
}

object Inc8b extends ArithmeticOperation("INC_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHCarryByte with FlagPOverflowByte with FlagNReset {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value + 1
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) + 1
  override def calcAux(input: ArithmeticOpInput): Int = (input.value & 0x0F)+1
}

object Dec8b extends ArithmeticOperation("INC_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet {
  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - 1
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) - 1
  override def calcAux(input: ArithmeticOpInput): Int = (input.value & 0x0F)-1
}

object Cpl8b extends ArithmeticOperation("CPL_8B") with ArithmeticCalculatorByte
  with FlagHSet with FlagNSet {
  override def calcUnsigned(input: ArithmeticOpInput): Int = ~input.value
}

object Neg8b extends ArithmeticOperation("NEG_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {
  override def calcUnsigned(input: ArithmeticOpInput): Int = 0 - input.value
  override def calcSigned(input: ArithmeticOpInput): Int = 0 - Z80Utils.rawByteTo2Compl(input.value)
  override def calcAux(input: ArithmeticOpInput): Int = 0 - (input.value & 0x0F)
}

object Ccf8b extends ArithmeticOperation("CCF_8B") with ArithmeticCalculatorByte
  with FlagHCopyC with FlagNReset with FlagCInvert {

  override def getDestination(source:Location):Location=Location.empty
}

object Scf8b extends ArithmeticOperation("SCF_8B") with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCSet {

  override def getDestination(source:Location):Location=Location.empty
}
