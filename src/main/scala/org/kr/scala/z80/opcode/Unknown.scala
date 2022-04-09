package org.kr.scala.z80.opcode

object Unknown extends OperationSpec {
  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x00)) -> 1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}
