package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, Regs}
import org.scalatest.funsuite.AnyFunSuite

class BlockTransferTest extends AnyFunSuite {

  implicit val debugger: Debugger = DummyDebugger

  //LDDR LDIR CPDR
  test("run LDI (no overflow)") {
    //given
    //when
    val sysTest = TestUtils.prepareTest(List((Regs.HL,0x0010),(Regs.DE,0x0020),(Regs.BC,0x0002),(Regs.F,0xFF)),
      List((0x0000, 0xED), (0x0001, 0xA0), //LDI
      (0x0010, 0xAB),(0x0020, 0x89))) //from / to locations
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.memory(0x0020)==0xAB)
    assert(sysTest.get.memory(0x0010)==0xAB)
    assert(sysTest.get.register(Regs.HL)==0x0011)
    assert(sysTest.get.register(Regs.DE)==0x0021)
    assert(sysTest.get.register(Regs.BC)==0x0001)
    TestUtils.testFlags(sysTest.get.register,"11_0_001")
  }
  test("run LDI (overflow)") {
    //given
    //when
    val sysTest = TestUtils.prepareTest(List((Regs.HL, 0x0010), (Regs.DE, 0x0020), (Regs.BC, 0x0001)),
      List((0x0000, 0xED), (0x0001, 0xA0), //LDI
        (0x0010, 0xAB), (0x0020, 0x89))) //from / to locations
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.memory(0x0020) == 0xAB)
    assert(sysTest.get.memory(0x0010) == 0xAB)
    assert(sysTest.get.register(Regs.HL) == 0x0011)
    assert(sysTest.get.register(Regs.DE) == 0x0021)
    assert(sysTest.get.register(Regs.BC) == 0x0000)
    TestUtils.testFlags(sysTest.get.register,"11_0_101")
  }
  test("run LDD (no overflow)") {
    //given
    //when
    val sysTest = TestUtils.prepareTest(List((Regs.HL, 0x0010), (Regs.DE, 0x0020), (Regs.BC, 0x0002)),
      List((0x0000, 0xED), (0x0001, 0xA8), //LDD
        (0x0010, 0xAB), (0x0020, 0x89))) //from / to locations
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.memory(0x0020) == 0xAB)
    assert(sysTest.get.memory(0x0010) == 0xAB)
    assert(sysTest.get.register(Regs.HL) == 0x000F)
    assert(sysTest.get.register(Regs.DE) == 0x001F)
    assert(sysTest.get.register(Regs.BC) == 0x0001)
    TestUtils.testFlags(sysTest.get.register,"11_0_001")
  }
  test("run LDD (overflow)") {
    //given
    //when
    val sysTest = TestUtils.prepareTest(List((Regs.HL, 0x0010), (Regs.DE, 0x0020), (Regs.BC, 0x0001)),
      List((0x0000, 0xED), (0x0001, 0xA8), //LDD
        (0x0010, 0xAB), (0x0020, 0x89))) //from / to locations
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.memory(0x0020) == 0xAB)
    assert(sysTest.get.memory(0x0010) == 0xAB)
    assert(sysTest.get.register(Regs.HL) == 0x000F)
    assert(sysTest.get.register(Regs.DE) == 0x001F)
    assert(sysTest.get.register(Regs.BC) == 0x0000)
    TestUtils.testFlags(sysTest.get.register,"11_0_101")
  }
}
