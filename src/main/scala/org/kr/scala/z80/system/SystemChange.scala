package org.kr.scala.z80.system

abstract class SystemChange {
  def handle(systemC:Z80System):Z80System
}

class RegisterChange(val regSymbol: RegSymbol, val value: Int) extends SystemChange {
  override def handle(system:Z80System):Z80System=
    system.changeRegister(regSymbol,value)
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

class OutputChange(val port: PortIDWithUpper, val value: Int)(implicit debugger:Debugger) extends SystemChange {
  override def handle(system:Z80System):Z80System=
    system.outputByte(port,value)
}

class InputRefreshChange(val port: PortIDWithUpper) extends SystemChange {
  override def handle(system:Z80System):Z80System=
    system.refreshInput(port)
}

class DummyChange() extends SystemChange {
  override def handle(system:Z80System):Z80System= system
}
