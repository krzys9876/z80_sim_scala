package org.kr.scala.z80.system

import org.kr.scala.z80.utils.{AnyInt, OptionInt}

abstract class SystemChange {
  def handle(systemC:Z80System):Z80System
}

class RegisterChange(val regSymbol: RegSymbol, val value: Int) extends SystemChange {
  override def handle(system:Z80System):Z80System=
  system.changeRegister(regSymbol,value)
}

class PCChange(val value: Int, val valueSupp: Int) extends SystemChange {
  override def handle(system:Z80System):Z80System=
    system.changePCAndCycles(value,valueSupp)
}

class RegisterChangeRelative(val regSymbol: RegSymbol, val value: Int) extends SystemChange {
  override def handle(system:Z80System):Z80System=
    system.changeRegisterRelative(regSymbol,value)
}

class MemoryChangeByte(val address: Int, val value: Int) extends SystemChange {
  override def handle(system:Z80System):Z80System=
    system.changeMemoryByte(address,value)
}

class MemoryChangeWord(val address: Int, val value: Int) extends SystemChange {
  override def handle(system:Z80System):Z80System=
    system.changeMemoryWord(address,value)
}

class OutputChange(val port: Int, val value: Int)(implicit debugger:Debugger) extends SystemChange {
  override def handle(system:Z80System):Z80System=
    system.outputByte(port,value)
}

class InputRefreshChange(val port: Int) extends SystemChange {
  override def handle(system:Z80System):Z80System=
    system.refreshInput(port)
}

class DummyChange() extends SystemChange {
  override def handle(system:Z80System):Z80System= system
}
