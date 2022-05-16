package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.system.{Debugger, RegisterChangeRelative, Regs, SystemChange, Z80System}
import org.kr.scala.z80.utils.IntValue

object Load16Bit extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChange], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize with OpCodeTCycles](code)
    val sourceLoc = actualCode.source
    val value = system.getValueFromLocation(sourceLoc)
    val destLoc = actualCode.destination
    val stackChange = code match {
      case change: OpStackChange => change.stackChange
      case _ => 0
    }

    val chgList = List(system.putValueToLocation(destLoc, value, isWord = true))
    val stackChgList = destLoc match {
      case Location(r, _, _, rd, dirO, _, _) if r != Regs.NONE || (rd != Regs.NONE && dirO != OpCode.ANY) =>
        List(new RegisterChangeRelative(Regs.SP, stackChange))
      case _ => List()
    }
    (chgList ++ stackChgList, actualCode.size, actualCode.t)
  }
}
