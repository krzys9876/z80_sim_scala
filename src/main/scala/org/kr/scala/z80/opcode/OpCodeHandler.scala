package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{SystemChangeBase, Z80System}

trait OpCodeHandler {
  def handle(code:OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int)
}
