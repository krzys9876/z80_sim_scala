package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

class AritheticOpLocationBase(val operation:ArithmeticOperationCalc)
class ArithmeticOpLocationAccum(override val operation:ArithmeticOperationCalc) extends AritheticOpLocationBase(operation)
class ArithmeticOpLocationFlags(override val operation:ArithmeticOperationCalc) extends AritheticOpLocationBase(operation)
class ArithmeticOpVariableLocation(override val operation:ArithmeticOperationCalc) extends AritheticOpLocationBase(operation)

object AritheticOpLocationBase {
  val empty:AritheticOpLocationBase=new AritheticOpLocationBase(NoneCalc8b)
}

object NoneCalc8b extends ArithmeticOperationCalc("NONE_8B") with ArithmeticCalculatorByte

object Arithmetic8Bit extends OperationSpec with OpCodeHandler {
  // Z80 manual page 50 (NOTE: ADD A,(HL) is 0x86, not 0x88!
  val operationListMap: Map[List[OpCode], AritheticOpLocationBase] = Map(
    List(OpCode(0x87), OpCode(0x80), OpCode(0x81), OpCode(0x82), OpCode(0x83), OpCode(0x84), OpCode(0x85),
      OpCode(0x86), OpCode(0xDD, 0x86), OpCode(0xFD, 0x86), OpCode(0xC6)) -> new ArithmeticOpLocationAccum(Add8b),
    List(OpCode(0x8F), OpCode(0x88), OpCode(0x89), OpCode(0x8A), OpCode(0x8B), OpCode(0x8C), OpCode(0x8D),
      OpCode(0x8E), OpCode(0xDD, 0x8E), OpCode(0xFD, 0x8E), OpCode(0xCE)) -> new ArithmeticOpLocationAccum(AddC8b),
    List(OpCode(0x97), OpCode(0x90), OpCode(0x91), OpCode(0x92), OpCode(0x93), OpCode(0x94), OpCode(0x95),
      OpCode(0x96), OpCode(0xDD, 0x96), OpCode(0xFD, 0x96), OpCode(0xD6)) -> new ArithmeticOpLocationAccum(Sub8b),
    List(OpCode(0x9F), OpCode(0x98), OpCode(0x99), OpCode(0x9A), OpCode(0x9B), OpCode(0x9C), OpCode(0x9D),
      OpCode(0x9E), OpCode(0xDD, 0x9E), OpCode(0xFD, 0x9E), OpCode(0xDE)) -> new ArithmeticOpLocationAccum(SubC8b),
    List(OpCode(0xA7), OpCode(0xA0), OpCode(0xA1), OpCode(0xA2), OpCode(0xA3), OpCode(0xA4), OpCode(0xA5),
      OpCode(0xA6), OpCode(0xDD, 0xA6), OpCode(0xFD, 0xA6), OpCode(0xE6)) -> new ArithmeticOpLocationAccum(And8b),
    List(OpCode(0xAF), OpCode(0xA8), OpCode(0xA9), OpCode(0xAA), OpCode(0xAB), OpCode(0xAC), OpCode(0xAD),
      OpCode(0xAE), OpCode(0xDD, 0xAE), OpCode(0xFD, 0xAE), OpCode(0xEE)) -> new ArithmeticOpLocationAccum(Xor8b),
    List(OpCode(0xB7), OpCode(0xB0), OpCode(0xB1), OpCode(0xB2), OpCode(0xB3), OpCode(0xB4), OpCode(0xB5),
      OpCode(0xB6), OpCode(0xDD, 0xB6), OpCode(0xFD, 0xB6), OpCode(0xF6)) -> new ArithmeticOpLocationAccum(Or8b),
    List(OpCode(0xBF), OpCode(0xB8), OpCode(0xB9), OpCode(0xBA), OpCode(0xBB), OpCode(0xBC), OpCode(0xBD),
      OpCode(0xBE), OpCode(0xDD, 0xBE), OpCode(0xFD, 0xBE), OpCode(0xFE)) -> new ArithmeticOpLocationFlags(Comp8b),
    List(OpCode(0x3C), OpCode(0x04), OpCode(0x0C), OpCode(0x14), OpCode(0x1C), OpCode(0x24), OpCode(0x2C),
      OpCode(0x34), OpCode(0xDD, 0x34), OpCode(0xFD, 0x34)) -> new ArithmeticOpVariableLocation(Inc8b),
    List(OpCode(0x3D), OpCode(0x05), OpCode(0x0D), OpCode(0x15), OpCode(0x1D), OpCode(0x25), OpCode(0x2D),
      OpCode(0x35), OpCode(0xDD, 0x35), OpCode(0xFD, 0x35)) -> new ArithmeticOpVariableLocation(Dec8b),
    List(OpCode(0x2F)) -> new ArithmeticOpLocationAccum(Cpl8b),
    List(OpCode(0xED, 0x44)) -> new ArithmeticOpLocationAccum(Neg8b),
    List(OpCode(0x3F)) -> new ArithmeticOpLocationFlags(Ccf8b),
    List(OpCode(0x37)) -> new ArithmeticOpLocationFlags(Scf8b)
  )

  val operation: OpCodeMap[AritheticOpLocationBase] = new OpCodeMap(operationListMap, AritheticOpLocationBase.empty)

  val operandListMap: Map[List[OpCode], LoadLocation] = Map(
    // accumulator
    List(OpCode(0x2F), OpCode(0xED, 0x44)) -> LoadLocation.register("A"),
    //indirect register
    List(OpCode(0x86), OpCode(0x8E), OpCode(0x96), OpCode(0x9E),
      OpCode(0xA6), OpCode(0xAE), OpCode(0xB6), OpCode(0xBE),
      OpCode(0x34), OpCode(0x35)
    ) -> LoadLocation.registerAddr("HL"),
    //indirect registers with offset
    List(OpCode(0xDD, 0x86), OpCode(0xDD, 0x8E), OpCode(0xDD, 0x96), OpCode(0xDD, 0x9E),
      OpCode(0xDD, 0xA6), OpCode(0xDD, 0xAE), OpCode(0xDD, 0xB6), OpCode(0xDD, 0xBE),
      OpCode(0xDD, 0x34), OpCode(0xDD, 0x35)
    ) -> LoadLocation.registerAddrIndirOffset("IX", 2),
    List(OpCode(0xFD, 0x86), OpCode(0xFD, 0x8E), OpCode(0xFD, 0x96), OpCode(0xFD, 0x9E),
      OpCode(0xFD, 0xA6), OpCode(0xFD, 0xAE), OpCode(0xFD, 0xB6), OpCode(0xFD, 0xBE),
      OpCode(0xFD, 0x34), OpCode(0xFD, 0x35)
    ) -> LoadLocation.registerAddrIndirOffset("IY", 2),
    // immediate
    List(OpCode(0xC6), OpCode(0xCE), OpCode(0xD6), OpCode(0xDE),
      OpCode(0xE6), OpCode(0xEE), OpCode(0xF6),
      OpCode(0xFE)) -> LoadLocation.registerAddrDirOffset("PC", 1),
    List(OpCode(0x3F), OpCode(0x37)) -> LoadLocation.empty
  ) ++
    //register
    OpCode.generateMapByReg(OpCode(0x80), 1, 0) ++
    OpCode.generateMapByReg(OpCode(0x88), 1, 0) ++
    OpCode.generateMapByReg(OpCode(0x90), 1, 0) ++
    OpCode.generateMapByReg(OpCode(0x98), 1, 0) ++
    OpCode.generateMapByReg(OpCode(0xA0), 1, 0) ++
    OpCode.generateMapByReg(OpCode(0xA8), 1, 0) ++
    OpCode.generateMapByReg(OpCode(0xB0), 1, 0) ++
    OpCode.generateMapByReg(OpCode(0xB8), 1, 0) ++
    OpCode.generateMapByReg(OpCode(0x04), 1, 3) ++
    OpCode.generateMapByReg(OpCode(0x05), 1, 3)

  val operand: OpCodeMap[LoadLocation] = new OpCodeMap(operandListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x86), OpCode(0x8E), OpCode(0x96), OpCode(0x9E), OpCode(0xA6), OpCode(0xAE), OpCode(0xB6), OpCode(0xBE),
      OpCode(0x34), OpCode(0x35),
      OpCode(0x2F), OpCode(0x3F), OpCode(0x37)) -> 1,
    List(OpCode(0xC6), OpCode(0xCE), OpCode(0xD6), OpCode(0xDE), OpCode(0xE6), OpCode(0xEE), OpCode(0xF6), OpCode(0xFE),
      OpCode(0xCE), OpCode(0xDE), OpCode(0xE6), OpCode(0xEE), OpCode(0xF6), OpCode(0xFE),
      OpCode(0xED, 0x44)) -> 2,
    List(OpCode(0xDD, 0x86), OpCode(0xDD, 0x8E), OpCode(0xDD, 0x96), OpCode(0xDD, 0x9E), OpCode(0xDD, 0xA6),
      OpCode(0xDD, 0xAE), OpCode(0xDD, 0xB6), OpCode(0xDD, 0xBE), OpCode(0xDD, 0x34), OpCode(0xDD, 0x35),
      OpCode(0xFD, 0x86), OpCode(0xFD, 0x8E), OpCode(0xFD, 0x96), OpCode(0xFD, 0x9E),
      OpCode(0xFD, 0xA6), OpCode(0xFD, 0xAE), OpCode(0xFD, 0xB6), OpCode(0xFD, 0xBE),
      OpCode(0xFD, 0x34), OpCode(0xFD, 0x35)) -> 3,
    OpCode.generateListByReg(OpCode(0x80), 1, 0) -> 1,
    OpCode.generateListByReg(OpCode(0x88), 1, 0) -> 1,
    OpCode.generateListByReg(OpCode(0x90), 1, 0) -> 1,
    OpCode.generateListByReg(OpCode(0x98), 1, 0) -> 1,
    OpCode.generateListByReg(OpCode(0xA0), 1, 0) -> 1,
    OpCode.generateListByReg(OpCode(0xA8), 1, 0) -> 1,
    OpCode.generateListByReg(OpCode(0xB0), 1, 0) -> 1,
    OpCode.generateListByReg(OpCode(0xB8), 1, 0) -> 1,
    OpCode.generateListByReg(OpCode(0x04), 1, 3) -> 1,
    OpCode.generateListByReg(OpCode(0x05), 1, 3) -> 1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val oper = Arithmetic8Bit.operation.find(code)
    val instrSize = Arithmetic8Bit.instSize.find(code)
    val operandLoc = Arithmetic8Bit.operand.find(code)
    val operand = system.getValueFromLocation(operandLoc)
    val prevFlags = system.getFlags

    val sourceLocation = oper match {
      case _: ArithmeticOpVariableLocation => operandLoc
      case _ => LoadLocation.register("A")
    }
    val prevValue = system.getValueFromLocation(sourceLocation)

    val destLocation = oper match {
      case _: ArithmeticOpLocationAccum => LoadLocation.register("A")
      case _: ArithmeticOpLocationFlags => LoadLocation.empty
      case _: ArithmeticOpVariableLocation => operandLoc
    }

    val (value, flags) = oper.operation.calcAll(ArithmeticOpInput(prevValue, operand, prevFlags))
    val chgList = List(system.putValueToLocation(destLocation, value))
    (chgList ++ List(new RegisterChange("F", flags())), instrSize)
  }
}

object Add8b extends ArithmeticOperationCalc("ADD_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHCarryByte with FlagPOverflowByte with FlagNReset with FlagCCarry {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value + input.operand
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) + Z80Utils.rawByteTo2Compl(input.operand)
  override def calcHalf(input: ArithmeticOpInput): Int = (input.value & 0x0F)+(input.operand & 0x0F)
}

object AddC8b extends ArithmeticOperationCalc("ADD_8B_CARRY") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHCarryByte with FlagPOverflowByte with FlagNReset with FlagCCarry {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value + input.operand + input.flags.flagValue(Flag.C)
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) + Z80Utils.rawByteTo2Compl(input.operand) + input.flags.flagValue(Flag.C)
  override def calcHalf(input: ArithmeticOpInput): Int =
    (input.value & 0x0F)+(input.operand & 0x0F)+input.flags.flagValue(Flag.C)
}

object Sub8b extends ArithmeticOperationCalc("SUB_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - input.operand
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand)
  override def calcHalf(input: ArithmeticOpInput): Int =
    (input.value & 0x0F)-(input.operand & 0x0F)
}

object SubC8b extends ArithmeticOperationCalc("SUB_8B_CARRY") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - input.operand - input.flags.flagValue(Flag.C)
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand) - input.flags.flagValue(Flag.C)
  override def calcHalf(input: ArithmeticOpInput): Int =
    (input.value & 0x0F) - (input.operand & 0x0F) - input.flags.flagValue(Flag.C)
}

object And8b extends ArithmeticOperationCalc("AND_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHSet with FlagPParity with FlagNReset with FlagCReset {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value & input.operand
  override def calcSigned(input: ArithmeticOpInput): Int = calcUnsigned(input)
}

object Xor8b extends ArithmeticOperationCalc("XOR_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCReset {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value ^ input.operand
  override def calcSigned(input: ArithmeticOpInput): Int = calcUnsigned(input)
}

object Or8b extends ArithmeticOperationCalc("OR_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHReset with FlagPParity with FlagNReset with FlagCReset {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value | input.operand
  override def calcSigned(input: ArithmeticOpInput): Int = calcUnsigned(input)
}

object Comp8b extends ArithmeticOperationCalc("COMP_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - input.operand
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand)
  override def calcHalf(input: ArithmeticOpInput): Int =
    (input.value & 0x0F)-(input.operand & 0x0F)
}

object Inc8b extends ArithmeticOperationCalc("INC_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHCarryByte with FlagPOverflowByte with FlagNReset {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value + 1
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) + 1
  override def calcHalf(input: ArithmeticOpInput): Int = (input.value & 0x0F)+1
}

object Dec8b extends ArithmeticOperationCalc("INC_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet {

  override def calcUnsigned(input: ArithmeticOpInput): Int = input.value - 1
  override def calcSigned(input: ArithmeticOpInput): Int =
    Z80Utils.rawByteTo2Compl(input.value) - 1
  override def calcHalf(input: ArithmeticOpInput): Int = (input.value & 0x0F)-1
}

object Cpl8b extends ArithmeticOperationCalc("CPL_8B") with ArithmeticCalculatorByte
  with FlagHSet with FlagNSet {

  override def calcUnsigned(input: ArithmeticOpInput): Int = ~input.value
  override def calcSigned(input: ArithmeticOpInput): Int = calcUnsigned(input)
}

object Neg8b extends ArithmeticOperationCalc("NEG_8B") with ArithmeticCalculatorByte
  with FlagSSignByte with FlagZZero with FlagHBorrow with FlagPOverflowByte with FlagNSet with FlagCBorrow {

  override def calcUnsigned(input: ArithmeticOpInput): Int = 0 - input.value
  override def calcSigned(input: ArithmeticOpInput): Int = 0 - Z80Utils.rawByteTo2Compl(input.value)
  override def calcHalf(input: ArithmeticOpInput): Int = 0 - (input.value & 0x0F)
}

object Ccf8b extends ArithmeticOperationCalc("CCF_8B") with ArithmeticCalculatorByte
  with FlagHCopyC with FlagNReset with FlagCInvert

object Scf8b extends ArithmeticOperationCalc("SCF_8B") with ArithmeticCalculatorByte
  with FlagHReset with FlagNReset with FlagCSet
