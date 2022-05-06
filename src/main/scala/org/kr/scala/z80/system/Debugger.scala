package org.kr.scala.z80.system

import org.kr.scala.z80.opcode.OpCode

trait Debugger {
  def debug(pc:Int,opCode:OpCode):Unit={}
  def debug(system:Z80System):Unit=
    debug(system.registerController.get("PC"),OpCode.getOpCodeObject(system.getCurrentOpCode))
}

object DummyDebugger extends Debugger {
  override def debug(pc: Int, opCode: OpCode): Unit = {}
}

object ConsoleDebugger extends Debugger {
  override def debug(pc: Int, opCode: OpCode): Unit = println(f"PC:0x$pc%04X opcode: $opCode")
}

object ConsoleDetailedDebugger extends Debugger {
  override def debug(pc: Int, opCode: OpCode): Unit = println(f"PC:0x$pc%04X opcode: $opCode")
  override def debug(system: Z80System): Unit = {
    val pc=system.registerController.get("PC")
    val opCode=OpCode.getOpCodeObject(system.getCurrentOpCode)
    val regs=system.registerController.get.reg.mkString("|")
    println(f"PC:0x$pc%04X | $opCode | $regs")
  }
}
