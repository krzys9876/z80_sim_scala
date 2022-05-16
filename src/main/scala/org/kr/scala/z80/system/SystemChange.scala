package org.kr.scala.z80.system

import org.kr.scala.z80.opcode.OpCode

abstract class SystemChange(val value: Int, val valueSupp:Int=0) {
  def handle(systemC:Z80System):Z80System
}

class RegisterChange(val regSymbol: RegSymbol, override val value: Int) extends SystemChange(value) {
  override def handle(system:Z80System):Z80System=
  system.changeRegister(regSymbol,value)
}

class PCChange(override val value: Int, override val valueSupp: Int) extends SystemChange(value) {
  override def handle(system:Z80System):Z80System=
    system.changePCAndCycles(value,valueSupp)
}

class RegisterChangeRelative(val regSymbol: RegSymbol, override val value: Int) extends SystemChange(value) {
  override def handle(system:Z80System):Z80System=
    system.changeRegisterRelative(regSymbol,value)
}

class MemoryChangeByte(val address: Int, override val value: Int) extends SystemChange(value) {
  override def handle(system:Z80System):Z80System=
    system.changeMemoryByte(address,value)
}

class MemoryChangeWord(val address: Int, override val value: Int) extends SystemChange(value) {
  override def handle(system:Z80System):Z80System=
    system.changeMemoryWord(address,value)
}

class OutputChange(val port: Int, override val value: Int)(implicit debugger:Debugger) extends SystemChange(value) {
  override def handle(system:Z80System):Z80System=
    system.outputByte(port,value)
}

class InputRefreshChange(val port: Int, override val value: Int= OpCode.ANY) extends SystemChange(value) {
  override def handle(system:Z80System):Z80System=
    system.refreshInput(port)
}

class DummyChange() extends SystemChange(OpCode.ANY) {
  override def handle(system:Z80System):Z80System= system
}
