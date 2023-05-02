package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, Regs, Z80System}

object Load16Bit extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (Z80System, Int, Int) = {
    val actualCode=castType[OpCode with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize with OpCodeTCycles](code)
    val sourceLoc = actualCode.source
    val value = system.getValueFromLocation(sourceLoc)
    val destLoc = actualCode.destination
    val chgSystem = system.putValueToLocation2(destLoc, value, isWord = true)
    val chgSystemStack = code match {
      case change: OpStackChange => chgSystem.changeRegisterRelative(Regs.SP, change.stackChange)
      case _ => chgSystem
    }
    (chgSystemStack, actualCode.size, actualCode.t)
  }
}
