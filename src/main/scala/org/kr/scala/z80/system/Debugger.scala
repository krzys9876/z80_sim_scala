package org.kr.scala.z80.system

import org.kr.scala.z80.opcode.OpCode

//TODO: add debugging functions for IN/OUT
trait Debugger {
  def stepBefore(system:Z80System):Unit= {}
  def stepAfter(system:Z80System):Unit= {}
}

object DummyDebugger extends Debugger {
}

object ConsoleDebugger extends Debugger {
  override def stepBefore(system: Z80System): Unit = {
    val pc=system.registerController.get("PC")
    val opCode=OpCode.getOpCodeObject(system.getCurrentOpCode)
    println(f"PC:0x$pc%04X opcode: $opCode")
  }
}

object ConsoleDetailedDebugger extends Debugger {
  override def stepBefore(system: Z80System): Unit = {
    val pc=system.registerController.get("PC")
    val opCode=OpCode.getOpCodeObject(system.getCurrentOpCode)
    val regs=system.registerController.get.reg.mkString("|")
    print(f"PC:0x$pc%04X | $opCode | before: $regs")
  }
  override def stepAfter(system: Z80System): Unit = {
    val regs=system.registerController.get.reg.mkString("|")
    println(f" after: $regs")
  }
}
