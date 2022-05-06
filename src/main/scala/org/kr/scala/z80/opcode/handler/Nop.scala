package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeMap, OpCodes, OperationSpec}
import org.kr.scala.z80.system.{Debugger, SystemChangeBase, Z80System}

object Nop extends OperationSpec with OpCodeHandler {
  // requires lazy initialization
  override lazy val instSize: OpCodeMap[Int] = new OpCodeMap(OpCodes.sizeMap, 0)

  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChangeBase], Int) =
    (List[SystemChangeBase](), instSize.find(code))
}
