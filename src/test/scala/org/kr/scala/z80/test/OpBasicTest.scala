package org.kr.scala.z80.test

import org.kr.scala.z80.system.{StateWatcher, Debugger, DummyDebugger, Regs, Z80System}
import org.scalatest.funsuite.AnyFunSuite

class OpBasicTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  implicit val debugger:Debugger=DummyDebugger

  // TEST NOP

  test("run NOP and move PC") {
    //given
    val sys1=StateWatcher[Z80System](Z80System.blank)
    //when
    val sys2=sys1 >>== Z80System.run(debugger)(1000L)
    //then
    assert(sys2.get.registerController.get(Regs.PC)==1000)
  }

  test("run NOP with memory overflow") {
    //given
    val sys1=StateWatcher[Z80System](Z80System.blank) >>== Z80System.changeRegister(Regs.PC,65534)
    //when
    val sys2=sys1 >>== Z80System.run(debugger)(3L)
    //then
    assert(sys2.get.registerController.get(Regs.PC)==1)
  }
}
