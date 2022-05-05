package org.kr.scala.z80.opcode.handler

import org.kr.scala.z80.opcode.OpCode
import org.kr.scala.z80.system.{SystemChangeBase, Z80System}

trait OpCodeHandler {
  def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int)
}
