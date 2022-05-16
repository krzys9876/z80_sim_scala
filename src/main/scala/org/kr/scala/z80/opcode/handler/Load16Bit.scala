package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, RegisterChangeRelative, Regs, SystemChange, Z80System}

object Load16Bit extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChange], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize with OpCodeTCycles](code)
    val sourceLoc = actualCode.source
    val value = system.getValueFromLocation(sourceLoc)
    val destLoc = actualCode.destination
    val chgList = List(system.putValueToLocation(destLoc, value, isWord = true))
    val stackChgList = code match {
      case change: OpStackChange => List(new RegisterChangeRelative(Regs.SP, change.stackChange))
      case _ => List()
    }
    (chgList ++ stackChgList, actualCode.size, actualCode.t)
  }
}
