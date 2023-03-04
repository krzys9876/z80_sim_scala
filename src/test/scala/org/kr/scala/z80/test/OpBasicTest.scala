package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, ImmutableMemory, MemoryHandler, Regs, StateWatcher, Z80System}
import org.scalatest.funsuite.AnyFunSuite

class OpBasicTest extends AnyFunSuite {
  implicit val debugger:Debugger=DummyDebugger
  implicit val memoryHandler:MemoryHandler=ImmutableMemory

  // TEST NOP
  test("run NOP and move PC") {
    implicit val debugger: Debugger = DummyDebugger
    //given
    val sys1=StateWatcher[Z80System](Z80System.blank)
    //when
    val sys2=sys1 >>== Z80System.run(debugger)(1000L)
    //then
    assert(sys2.get.register(Regs.PC)==1000)
  }

  test("run NOP with memory overflow") {
    //given
    val sys1=Z80System.blank.changeRegister(Regs.PC,65534)
    //when
    val sys2=StateWatcher(sys1) >>== Z80System.run(debugger)(3L)
    //then
    assert(sys2.get.register(Regs.PC)==1)
  }
}
