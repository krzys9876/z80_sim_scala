package org.kr.scala.z80.system

import org.kr.scala.z80.opcode.OpCode

abstract class SystemChangeBase(val value: Int, val valueSupp:Int=0) {
  def handle(systemC:Z80System):Z80System
}

class RegisterChange(val regSymbol: RegSymbol, override val value: Int) extends SystemChangeBase(value) {
  override def handle(system:Z80System):Z80System=
    (BaseStateMonad[Z80System](system) >>== Z80System.changeRegister(regSymbol,value)).get
}

class PCChange(override val value: Int, override val valueSupp: Int) extends SystemChangeBase(value) {
  override def handle(system:Z80System):Z80System=
    (BaseStateMonad[Z80System](system) >>== Z80System.changePCAndCycles(value,valueSupp)).get
}

class RegisterChangeRelative(val regSymbol: RegSymbol, override val value: Int) extends SystemChangeBase(value) {
  override def handle(system:Z80System):Z80System=
    (BaseStateMonad[Z80System](system) >>== Z80System.changeRegisterRelative(regSymbol,value)).get
}

class MemoryChangeByte(val address: Int, override val value: Int) extends SystemChangeBase(value) {
  override def handle(system:Z80System):Z80System=
    (BaseStateMonad[Z80System](system) >>== Z80System.changeMemoryByte(address,value)).get
}

class MemoryChangeWord(val address: Int, override val value: Int) extends SystemChangeBase(value) {
  override def handle(system:Z80System):Z80System=
    (BaseStateMonad[Z80System](system) >>== Z80System.changeMemoryWord(address,value)).get
}

class OutputChange(val port: Int, override val value: Int)(implicit debugger:Debugger) extends SystemChangeBase(value) {
  override def handle(system:Z80System):Z80System=
    (BaseStateMonad[Z80System](system) >>== Z80System.outputByte(debugger)(port,value)).get
}

class InputRefreshChange(val port: Int, override val value: Int= OpCode.ANY) extends SystemChangeBase(value) {
  override def handle(system:Z80System):Z80System=
    (BaseStateMonad[Z80System](system) >>== Z80System.refreshInput(port)).get
}

class DummyChange() extends SystemChangeBase(OpCode.ANY) {
  override def handle(system:Z80System):Z80System= system
}
