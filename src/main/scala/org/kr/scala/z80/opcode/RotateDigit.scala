package org.kr.scala.z80.opcode

object RotateDigit extends OperationSpec {
  // Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
  val operationListMap: Map[List[OpCode],ArithmeticOperation] = Map(
    List(OpCode(0xED,0x6F)) -> ArithmeticOpType.Rld,
    List(OpCode(0xED,0x67)) -> ArithmeticOpType.Rrd
  )

  val operation: OpCodeMap[ArithmeticOperation] = new OpCodeMap(operationListMap, ArithmeticOpType.None)

  val locationListMap: Map[List[OpCode], LoadLocation] = Map(
    List(OpCode(0xED,0x6F),OpCode(0xED,0x67)) -> LoadLocation.registerAddr("HL")
  )

  val location: OpCodeMap[LoadLocation] = new OpCodeMap(locationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xED,0x6F),OpCode(0xED,0x67)) ->2
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}
