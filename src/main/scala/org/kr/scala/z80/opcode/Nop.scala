package org.kr.scala.z80.opcode

object Nop extends OperationSpec(OpType.NopType) {

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x00)) -> 1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}
