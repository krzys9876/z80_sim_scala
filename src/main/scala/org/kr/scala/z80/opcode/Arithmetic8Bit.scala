package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object Arithmetic8Bit extends OperationSpec with OpCodeHandler {
  // Z80 manual page 50 (NOTE: ADD A,(HL) is 0x86, not 0x88!
  val operationListMap: Map[List[OpCode], ArithmeticOperation] = Map(
    List(ADD_A_A, ADD_A_B, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_H, ADD_A_L,
      ADD_A_HL, ADD_A_IX_d, ADD_A_IY_d, ADD_A_n) -> Add8b,
    List(ADC_A_A, ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_H, ADC_A_L,
      ADC_A_HL, ADC_A_IX_d, ADC_A_IY_d, ADC_A_n) -> AddC8b,
    List(SUB_A, SUB_B, SUB_C, SUB_D, SUB_E, SUB_H, SUB_L,
      SUB_HL, SUB_IX_d, SUB_IY_d, SUB_n) -> Sub8b,
    List(SBC_A_A, SBC_A_B, SBC_A_C, SBC_A_D, SBC_A_E, SBC_A_H, SBC_A_L,
      SBC_A_HL, SBC_A_IX_d, SBC_A_IY_d, SBC_A_n) -> SubC8b,
    List(AND_A, AND_B, AND_C, AND_D, AND_E, AND_H, AND_L,
      AND_HL, AND_IX_d, AND_IY_d, AND_n) -> And8b,
    List(XOR_A, XOR_B, XOR_C, XOR_D, XOR_E, XOR_H, XOR_L,
      XOR_HL, XOR_IX_d, XOR_IY_d, XOR_n) -> Xor8b,
    List(OR_A, OR_B, OR_C, OR_D, OR_E, OR_H, OR_L,
      OR_HL, OR_IX_d, OR_IY_d, OR_n) -> Or8b,
    List(CP_A, CP_B, CP_C, CP_D, CP_E, CP_H, CP_L,
      CP_HL, CP_IX_d, CP_IY_d, CP_n) -> Comp8b,
    List(INC_A, INC_B, INC_C, INC_D, INC_E, INC_H, INC_L,
      INC_HL, INC_IX_d, INC_IY_d) -> Inc8b,
    List(DEC_A, DEC_B, DEC_C, DEC_D, DEC_E, DEC_H, DEC_L,
      DEC_HL, DEC_IX_d, DEC_IY_d) -> Dec8b,
    List(CPL) -> Cpl8b,
    List(NEG) -> Neg8b,
    List(CCF) -> Ccf8b,
    List(SCF) -> Scf8b
  )

  val operation: OpCodeMap[ArithmeticOperation] = new OpCodeMap(operationListMap, None8b)

  val source: OpCodeMap[LoadLocation] = new OpCodeMap(OpCodes.sourceMap, LoadLocation.empty)
  val destination: OpCodeMap[LoadLocation] = new OpCodeMap(OpCodes.destinationMap, LoadLocation.empty)
  val operand: OpCodeMap[LoadLocation] = new OpCodeMap(OpCodes.operandMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(ADD_A_HL, ADC_A_HL, SUB_HL, SBC_A_HL, AND_HL, XOR_HL, OR_HL, CP_HL, INC_HL, DEC_HL, CPL, CCF, SCF) -> 1,
    List(ADD_A_n, ADC_A_n, SUB_n, SBC_A_n, AND_n, XOR_n, OR_n, CP_n, NEG) -> 2,
    List(ADD_A_IX_d, ADC_A_IX_d, SUB_IX_d, SBC_A_IX_d, AND_IX_d, XOR_IX_d, OR_IX_d, CP_IX_d, INC_IX_d, DEC_IX_d,
      ADD_A_IY_d, ADC_A_IY_d, SUB_IY_d, SBC_A_IY_d, AND_IY_d, XOR_IY_d, OR_IY_d, CP_IY_d, INC_IY_d, DEC_IY_d) -> 3,
    List(ADD_A_A, ADD_A_B, ADD_A_C, ADD_A_D, ADD_A_E, ADD_A_H, ADD_A_L) -> 1,
    List(ADC_A_A, ADC_A_B, ADC_A_C, ADC_A_D, ADC_A_E, ADC_A_H, ADC_A_L) -> 1,
    List(SUB_A, SUB_B, SUB_C, SUB_D, SUB_E, SUB_H, SUB_L) -> 1,
    List(SBC_A_A, SBC_A_B, SBC_A_C, SBC_A_D, SBC_A_E, SBC_A_H, SBC_A_L) -> 1,
    List(AND_A, AND_B, AND_C, AND_D, AND_E, AND_H, AND_L) -> 1,
    List(XOR_A, XOR_B, XOR_C, XOR_D, XOR_E, XOR_H, XOR_L) -> 1,
    List(OR_A, OR_B, OR_C, OR_D, OR_E, OR_H, OR_L) -> 1,
    List(CP_A, CP_B, CP_C, CP_D, CP_E, CP_H, CP_L) -> 1,
    List(INC_A, INC_B, INC_C, INC_D, INC_E, INC_H, INC_L) -> 1,
    List(DEC_A, DEC_B, DEC_C, DEC_D, DEC_E, DEC_H, DEC_L) -> 1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val oper = Arithmetic8Bit.operation.find(code)
    val instrSize = Arithmetic8Bit.instSize.find(code)
    val operandLoc = Arithmetic8Bit.operand.find(code)
    val operand = system.getValueFromLocation(operandLoc)
    val prevFlags = system.getFlags
    val sourceLocation = Arithmetic8Bit.source.find(code)
    val prevValue = system.getValueFromLocation(sourceLocation)
    val destLocation = Arithmetic8Bit.destination.find(code)

    val (result, flags) = oper.calcAll(ArithmeticOpInput(prevValue, operand, prevFlags))
    val chgList = List(system.putValueToLocation(destLocation, result.valueOut))
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
  override def calcSigned(input: ArithmeticOpInput): Int = calcUnsigned(input)
}

object Xor8b extends ArithmeticOperation("XOR_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCReset {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value ^ input.operand
  override def calcSigned(input: ArithmeticOpInput): Int = calcUnsigned(input)
}

object Or8b extends ArithmeticOperation("OR_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCReset {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value | input.operand
  override def calcSigned(input: ArithmeticOpInput): Int = calcUnsigned(input)
}

object Comp8b extends ArithmeticOperation("COMP_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - input.operand
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand)
  override def calcAux(input: ArithmeticOpInput): Int =
    (input.value & 0x0F)-(input.operand & 0x0F)
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
  override def calcSigned(input: ArithmeticOpInput): Int = calcUnsigned(input)
}

object Neg8b extends ArithmeticOperation("NEG_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {

  override def calcUnsigned(input: ArithmeticOpInput): Int = 0 - input.value
  override def calcSigned(input: ArithmeticOpInput): Int = 0 - Z80Utils.rawByteTo2Compl(input.value)
  override def calcAux(input: ArithmeticOpInput): Int = 0 - (input.value & 0x0F)
}

object Ccf8b extends ArithmeticOperation("CCF_8B") with ArithmeticCalculatorByte
  with FlagHCopyC with FlagNReset with FlagCInvert

object Scf8b extends ArithmeticOperation("SCF_8B") with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCSet
