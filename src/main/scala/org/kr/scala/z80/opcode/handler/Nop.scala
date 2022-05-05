package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeMap, OpCodes, OperationSpec}
import org.kr.scala.z80.system.{SystemChangeBase, Z80System}

object Nop extends OperationSpec with OpCodeHandler {
  override val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)
  val nopMap:OpCodeMap[Any] = new OpCodeMap(OpCodes.nopMap, Nil)
  override lazy val isOper: OpCode => Boolean = opcode => nopMap.contains(opcode)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    (List[SystemChangeBase](), instSize.find(code))
  }
}
