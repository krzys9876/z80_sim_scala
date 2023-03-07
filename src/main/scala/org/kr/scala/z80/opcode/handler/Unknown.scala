package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, UnknownOperationException}
import org.kr.scala.z80.system.{Debugger, Regs, SystemChange, Z80System}

object Unknown extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChange], Int, Int) = {
    val message=f"Unknown operation $code at ${system.getRegValue(Regs.PC)}"
    debugger.error(message)
    throw new UnknownOperationException(f"Unknown operation $code at ${system.getRegValue(Regs.PC)}")
  }

}
