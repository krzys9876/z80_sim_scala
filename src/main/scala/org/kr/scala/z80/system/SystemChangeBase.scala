package org.kr.scala.z80.system

import org.kr.scala.z80.opcode.OpCode

abstract class SystemChangeBase(val value: Int) {
  def handle(systemC:Z80SystemController):Z80SystemController
}

class RegisterChange(val regSymbol: String, override val value: Int) extends SystemChangeBase(value) {
  override def handle(systemC:Z80SystemController):Z80SystemController=
    systemC >>= Z80SystemController.changeRegister(regSymbol,value)
}

class RegisterChangeRelative(val regSymbol: String, override val value: Int) extends SystemChangeBase(value) {
  override def handle(systemC:Z80SystemController):Z80SystemController=
    systemC >>= Z80SystemController.changeRegisterRelative(regSymbol,value)
}

class MemoryChangeByte(val address: Int, override val value: Int) extends SystemChangeBase(value) {
  override def handle(systemC:Z80SystemController):Z80SystemController=
    systemC >>= Z80SystemController.changeMemoryByte(address,value)
}

class MemoryChangeWord(val address: Int, override val value: Int) extends SystemChangeBase(value) {
  override def handle(systemC:Z80SystemController):Z80SystemController=
    systemC >>= Z80SystemController.changeMemoryWord(address,value)
}

class OutputChange(val port: Int, override val value: Int)(implicit debugger:Debugger) extends SystemChangeBase(value) {
  override def handle(systemC:Z80SystemController):Z80SystemController=
    systemC >>= Z80SystemController.outputByte(debugger)(port,value)
}

class InputRefreshChange(val port: Int, override val value: Int= OpCode.ANY) extends SystemChangeBase(value) {
  override def handle(systemC:Z80SystemController):Z80SystemController=
    systemC >>= Z80SystemController.refreshInput(port)
}

class DummyChange() extends SystemChangeBase(OpCode.ANY) {
  override def handle(systemC:Z80SystemController):Z80SystemController= systemC
}
