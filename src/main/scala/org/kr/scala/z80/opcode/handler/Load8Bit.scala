package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.{OpCode, OpCodeDestLocation, OpCodeSize, OpCodeSourceLocation, _}
import org.kr.scala.z80.system.{Debugger, SystemChange, Z80System}

object Load8Bit extends OpCodeHandler {
  override def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (Z80System,List[SystemChange], Int, Int) = {
    val actualCode=castType[OpCode with OpCodeSourceLocation with OpCodeDestLocation with OpCodeSize with OpCodeTCycles](code)
    val value = system.getValueFromLocation(actualCode.source)
    (system,List(system.putValueToLocation(actualCode.destination, value)), actualCode.size, actualCode.t)
  }
}
