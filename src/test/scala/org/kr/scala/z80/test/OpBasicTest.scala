package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, ImmutableMemoryHandler, ImmutableRegisterHandler, MemoryHandler, RegisterHandler, Regs, StateWatcher, StateWatcherHandler, StateWatcherHandlerBase, TCycleCounterHandler, TCycleCounterHandlerImmutable, Z80System}
import org.scalatest.funsuite.AnyFunSuite

class OpBasicTest extends AnyFunSuite {
  implicit val debugger:Debugger=DummyDebugger
  implicit val memoryHandler:MemoryHandler=new ImmutableMemoryHandler()
  implicit val registerHandler:RegisterHandler=new ImmutableRegisterHandler()
  implicit val tCycleCounterHandler:TCycleCounterHandler=new TCycleCounterHandlerImmutable()
  implicit val stateWatcherHandler:StateWatcherHandlerBase[Z80System] = new StateWatcherHandler[Z80System]()

  // TEST NOP
  test("run NOP and move PC") {
    implicit val debugger: Debugger = DummyDebugger
    //given
    val sys1=StateWatcher[Z80System](Z80System.blank)
    //when
    val sys2=sys1 >>== Z80System.run(debugger,stateWatcherHandler)(1000L)
    //then
    assert(sys2.get.register(Regs.PC)==1000)
  }

  test("run NOP with memory overflow") {
    //given
    val sys1=Z80System.blank.changeRegister(Regs.PC,65534)
    //when
    val sys2=StateWatcher(sys1) >>== Z80System.run(debugger,stateWatcherHandler)(3L)
    //then
    assert(sys2.get.register(Regs.PC)==1)
  }
}
