package org.kr.scala.z80.system

import org.kr.scala.z80.opcode.OpCodes

//TODO: add debugging functions for IN/OUT
trait Debugger {
  def stepBefore(system:Z80System):Unit= {}
  def stepAfter(system:Z80System):Unit= {}
  def output(port:Int,value:Int):Unit= {}
  def input(port:Int,value:Int):Unit= {}
}

object DummyDebugger extends Debugger {
}

object ConsoleDebugger extends Debugger {
  override def output(port:Int,value:Int):Unit= print(value.toChar)
}

object ConsoleDetailedDebugger extends Debugger {
  override def stepBefore(system: Z80System): Unit = {
    val pc=system.registerController.get(Regs.PC)
    val opCode=OpCodes.getOpCodeObject(system.getCurrentOpCode)
    val regs=system.registerController.get.toString
    print(f"PC:0x$pc%04X | $opCode | before: $regs")
  }
  override def stepAfter(system: Z80System): Unit = {
    val regs=system.registerController.get.toString
    println(f" after: $regs")
  }
  override def output(port:Int,value:Int):Unit= print(f" | OUT port: 0x$port%02X value: 0x$value%02X |")
  override def input(port:Int,value:Int):Unit= print(f" | IN port: 0x$port%02X value: 0x$value%02X |")
}
