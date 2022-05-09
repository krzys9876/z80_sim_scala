package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class OpBasicTest extends AnyFunSuite {
  test("always pass") {
    assert(1==1)
  }

  implicit val debugger:Debugger=DummyDebugger

  // TEST NOP

  test("run NOP and move PC") {
    //given
    val sys1=Z80SystemController.blank
    //when
    val sys2=sys1 >>= Z80SystemController.run(debugger)(1000L)
    //then
    assert(sys2.get.registerController.get("PC")==1000)
  }

  test("run NOP with memory overflow") {
    //given
    val sys1=Z80SystemController.blank >>= Z80SystemController.changeRegister("PC",65534)
    //when
    val sys2=sys1 >>= Z80SystemController.run(debugger)(3L)
    //then
    assert(sys2.get.registerController.get("PC")==1)
  }
}
