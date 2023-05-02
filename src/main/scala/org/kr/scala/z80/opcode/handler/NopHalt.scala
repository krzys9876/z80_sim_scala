package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeSize, OpCodeTCycles}
import org.kr.scala.z80.system.{Debugger, SystemChange, Z80System}

object Nop extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (Z80System,List[SystemChange], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeSize with OpCodeTCycles](code)
    (system,List[SystemChange](), actualCode.size, actualCode.t)
  }
}

object Halt extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (Z80System,List[SystemChange], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeTCycles](code)
    (system,List[SystemChange](), 0, actualCode.t) // Halt is like NOP, but does not move PC until interrupt
  }
}
