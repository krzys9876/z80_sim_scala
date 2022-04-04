package org.kr.scala.z80.opcode

object Arithmetic8Bit extends OperationSpec {

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x87, OpCode.ANY),OpCode(0x80, OpCode.ANY),OpCode(0x81, OpCode.ANY),OpCode(0x82, OpCode.ANY),
      OpCode(0x83, OpCode.ANY),OpCode(0x84, OpCode.ANY),OpCode(0x85, OpCode.ANY)) -> 1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

}
