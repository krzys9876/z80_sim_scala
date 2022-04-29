package org.kr.scala.z80.system

import org.kr.scala.z80.opcode.OpCode

trait Debugger {
  def debug(pc:Int,opCode:OpCode):Unit
}

object DummyDebugger extends Debugger {
  override def debug(pc: Int, opCode: OpCode): Unit = {}
}

object ConsoleDebugger extends Debugger {
  override def debug(pc: Int, opCode: OpCode): Unit = println(f"PC:0x$pc%04X opcode: $opCode")
}
