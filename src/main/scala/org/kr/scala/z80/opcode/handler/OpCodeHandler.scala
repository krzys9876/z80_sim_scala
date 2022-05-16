package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.OpCode
import org.kr.scala.z80.system.{Debugger, SystemChange, Z80System}

trait OpCodeHandler {
  def handle(code: OpCode)(implicit system: Z80System, debugger:Debugger): (List[SystemChange], Int, Int)
  def castType[T<:OpCode](code:OpCode):T=code.asInstanceOf[T]
}
