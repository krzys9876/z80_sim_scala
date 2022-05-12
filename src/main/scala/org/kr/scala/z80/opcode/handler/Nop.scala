package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeSize, OpCodeTCycles}
import org.kr.scala.z80.system.{Debugger, SystemChangeBase, Z80System}

object Nop extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChangeBase], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeSize with OpCodeTCycles](code)
    (List[SystemChangeBase](), actualCode.size, actualCode.t)
  }
}
