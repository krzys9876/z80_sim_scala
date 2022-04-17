package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object Arithmetic16Bit extends OperationSpec with OpCodeHandler {
  // Z80 manual page 52
  val operationListMap: Map[List[OpCode],ArithmeticOperationCalc] = Map(
    List(OpCode(0x09),OpCode(0x19),OpCode(0x29),OpCode(0x39),
      OpCode(0xDD,0x09),OpCode(0xFD,0x09),OpCode(0xDD,0x19),OpCode(0xFD,0x19),OpCode(0xDD,0x39),OpCode(0xFD,0x39),
      OpCode(0xDD,0x29),OpCode(0xFD,0x29)) -> AddCalc,
    List(OpCode(0xED,0x4A),OpCode(0xED,0x5A),OpCode(0xED,0x6A),OpCode(0xED,0x7A))
      -> AddCCalc,
    List(OpCode(0xED,0x42),OpCode(0xED,0x52),OpCode(0xED,0x62),OpCode(0xED,0x72))
      -> SubCCalc,
    List(OpCode(0x03),OpCode(0x13),OpCode(0x23),OpCode(0x33),OpCode(0xDD,0x23),OpCode(0xFD,0x23))
      -> IncCalc,
    List(OpCode(0x0B),OpCode(0x1B),OpCode(0x2B),OpCode(0x3B),OpCode(0xDD,0x2B),OpCode(0xFD,0x2B))
      -> DecCalc
  )
  val operation: OpCodeMap[ArithmeticOperationCalc] = new OpCodeMap(operationListMap, NoneCalc)

  val sourceListMap: Map[List[OpCode],LoadLocation] = Map(
    List(OpCode(0x09),OpCode(0xDD,0x09),OpCode(0xFD,0x09),OpCode(0xED,0x4A),OpCode(0xED,0x42),
      OpCode(0x03),OpCode(0x0B)) -> LoadLocation.register("BC"),
    List(OpCode(0x19),OpCode(0xDD,0x19),OpCode(0xFD,0x19),OpCode(0xED,0x5A),OpCode(0xED,0x52),
      OpCode(0x13),OpCode(0x1B)) -> LoadLocation.register("DE"),
    List(OpCode(0x29),OpCode(0xED,0x6A),OpCode(0xED,0x62),OpCode(0x23),OpCode(0x2B)) -> LoadLocation.register("HL"),
    List(OpCode(0x39),OpCode(0xDD,0x39),OpCode(0xFD,0x39),OpCode(0xED,0x7A),OpCode(0xED,0x72),
      OpCode(0x33),OpCode(0x3B)) -> LoadLocation.register("SP"),
    List(OpCode(0xDD,0x29),OpCode(0xDD,0x23),OpCode(0xDD,0x2B)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD,0x29),OpCode(0xFD,0x23),OpCode(0xFD,0x2B)) -> LoadLocation.register("IY")
  )
  val source: OpCodeMap[LoadLocation] = new OpCodeMap(sourceListMap, LoadLocation.empty)

  val destinationListMap: Map[List[OpCode],LoadLocation] = Map(
    List(OpCode(0x09),OpCode(0x19),OpCode(0x29),OpCode(0x39),OpCode(0xED,0x4A),OpCode(0xED,0x42),
      OpCode(0xED,0x5A),OpCode(0xED,0x52),OpCode(0xED,0x6A),OpCode(0xED,0x62),OpCode(0xED,0x7A),OpCode(0xED,0x72),
      OpCode(0x23),OpCode(0x2B)) -> LoadLocation.register("HL"),
    List(OpCode(0xDD,0x09),OpCode(0xDD,0x19),OpCode(0xDD,0x39),OpCode(0xDD,0x29),
      OpCode(0xDD,0x23),OpCode(0xDD,0x2B)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD,0x09),OpCode(0xFD,0x19),OpCode(0xFD,0x39),OpCode(0xFD,0x29),
      OpCode(0xFD,0x23),OpCode(0xFD,0x2B)) -> LoadLocation.register("IY"),
    List(OpCode(0x03),OpCode(0x0B)) -> LoadLocation.register("BC"),
    List(OpCode(0x13),OpCode(0x1B)) -> LoadLocation.register("DE"),
    List(OpCode(0x33),OpCode(0x3B)) -> LoadLocation.register("SP")
  )
  val destination: OpCodeMap[LoadLocation] = new OpCodeMap(destinationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x09),OpCode(0x19),OpCode(0x29),OpCode(0x39),OpCode(0x03),OpCode(0x13),
      OpCode(0x23),OpCode(0x33),OpCode(0x0B),OpCode(0x1B),OpCode(0x2B),OpCode(0x3B)) -> 1,
    List(OpCode(0xDD,0x09),OpCode(0xFD,0x09),OpCode(0xDD,0x19),OpCode(0xFD,0x19),OpCode(0xDD,0x39),OpCode(0xFD,0x39),
      OpCode(0xDD,0x29),OpCode(0xFD,0x29),OpCode(0xED,0x4A),OpCode(0xED,0x5A),OpCode(0xED,0x6A),OpCode(0xED,0x7A),
      OpCode(0xED,0x42),OpCode(0xED,0x52),OpCode(0xED,0x62),OpCode(0xED,0x72),
      OpCode(0xDD,0x23),OpCode(0xFD,0x23),OpCode(0xDD,0x2B),OpCode(0xFD,0x2B)) -> 2
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    val oper = operation.find(code)
    val sourceLoc=source.find(code)
    val destLoc=destination.find(code)

    val calcInput=ArithmeticOpInput(
      system.getValueFromLocation(destLoc),
      system.getValueFromLocation(sourceLoc),
      system.getFlags)

    val (value, flags) = oper.calcAll(calcInput)
    val chgList=List(system.putValueToLocation(destLoc,value), new RegisterChange("F", flags()))

    (chgList, instSize.find(code))
  }
}

object AddCalc extends ArithmeticOperationCalc("ADD_16B")
  with FlagCalculatorBase
  with FlagHCarryWord with FlagNReset with FlagCCarry
{
  override def calc(input:ArithmeticOpInput):ArithmeticOpResult= {
    new ArithmeticOpResultWord(
      input.value + input.operand,
      Z80Utils.rawWordTo2Compl(input.value) + Z80Utils.rawWordTo2Compl(input.operand),
      (input.value & 0x0FFF) + (input.operand & 0x0FFF)
    )
  }
}

object AddCCalc extends ArithmeticOperationCalc("ADD_CARRY_16B")
  with FlagSSignWord with FlagZZero with FlagHCarryWord
  with FlagPOverflowWord with FlagNReset with FlagCCarry {

  def calc(input:ArithmeticOpInput):ArithmeticOpResult=
    new ArithmeticOpResultWord(
      input.value + input.operand + input.flags.flagValue(Flag.C),
      Z80Utils.rawWordTo2Compl(input.value) + Z80Utils.rawWordTo2Compl(input.operand) + input.flags.flagValue(Flag.C),
      (input.value & 0x0FFF) + (input.operand & 0x0FFF) + input.flags.flagValue(Flag.C)
    )
}

object SubCCalc extends ArithmeticOperationCalc("SUB_CARRY_16B")
  with FlagSSignWord with FlagZZero with FlagHBorrow
  with FlagPOverflowWord with FlagNSet with FlagCBorrow {

  def calc(input:ArithmeticOpInput):ArithmeticOpResult=
    new ArithmeticOpResultWord(
      input.value - input.operand - input.flags.flagValue(Flag.C),
      Z80Utils.rawWordTo2Compl(input.value) - Z80Utils.rawWordTo2Compl(input.operand) - input.flags.flagValue(Flag.C),
      (input.value & 0x0FFF) - (input.operand & 0x0FFF) - input.flags.flagValue(Flag.C)
    )
}

object IncCalc extends ArithmeticOperationCalc("INC_16B") {

  def calc(input:ArithmeticOpInput):ArithmeticOpResult=
    new ArithmeticOpResultWord(
      input.value + 1,
      Z80Utils.rawWordTo2Compl(input.value) + 1,
      OpCode.ANY
    )
}

object DecCalc extends ArithmeticOperationCalc("DEC_16B") {
  def calc(input:ArithmeticOpInput):ArithmeticOpResult=
    new ArithmeticOpResultWord(
      input.value - 1,
      Z80Utils.rawWordTo2Compl(input.value) - 1,
      OpCode.ANY
    )
}

object NoneCalc extends ArithmeticOperationCalc("NONE_16B") {
  def calc(input:ArithmeticOpInput):ArithmeticOpResult=
    new ArithmeticOpResultWord(OpCode.ANY,OpCode.ANY,OpCode.ANY)
}
