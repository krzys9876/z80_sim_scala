package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeMap, OperationSpec, UnknownOperationException}
import org.kr.scala.z80.system.{SystemChangeBase, Z80System}

object Unknown extends OperationSpec with OpCodeHandler {
  override val instSize: OpCodeMap[Int] = new OpCodeMap(Map(OpCode(OpCode.ANY) -> 0), 0)
  override lazy val isOper: OpCode => Boolean = _ => false

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    throw new UnknownOperationException(f"Unknown operation $code at ${system.getRegValue("PC")}")
  }

}
