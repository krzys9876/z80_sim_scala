package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{SystemChangeBase, Z80System}

object Nop extends OperationSpec with OpCodeHandler {

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x00)) -> 1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code:OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    (List[SystemChangeBase](),instSize.find(code))
  }
}
