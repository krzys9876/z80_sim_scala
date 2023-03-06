package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, Regs}
import org.scalatest.funsuite.AnyFunSuite

class BlockTransferTest extends AnyFunSuite {

  implicit val debugger: Debugger = DummyDebugger

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

  test("run LDIR") {
    //given
    //when
    val sysTest = TestUtils.prepareTest(List((Regs.HL, 0x0010), (Regs.DE, 0x0020), (Regs.BC, 0x0003), (Regs.F, 0xFF)),
      List((0x0000, 0xED), (0x0001, 0xB0), //LDIR
        (0x0010, 0xAB),(0x0011, 0xCD),(0x0012, 0xEF), (0x0020, 0x12),(0x0020, 0x34),(0x0020, 0x56)), //from / to locations
      3)
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.memory(0x0020) == 0xAB)
    assert(sysTest.get.memory(0x0021) == 0xCD)
    assert(sysTest.get.memory(0x0022) == 0xEF)
    assert(sysTest.get.memory(0x0010) == 0xAB)
    assert(sysTest.get.memory(0x0011) == 0xCD)
    assert(sysTest.get.memory(0x0012) == 0xEF)
    assert(sysTest.get.register(Regs.HL) == 0x0013)
    assert(sysTest.get.register(Regs.DE) == 0x0023)
    assert(sysTest.get.register(Regs.BC) == 0x0000)
    TestUtils.testFlags(sysTest.get.register, "11_0_101")
  }
  test("run LDDR") {
    //given
    //when
    val sysTest = TestUtils.prepareTest(List((Regs.HL, 0x0012), (Regs.DE, 0x0022), (Regs.BC, 0x0003), (Regs.F, 0xFF)),
      List((0x0000, 0xED), (0x0001, 0xB8), //LDDR
        (0x0010, 0xAB), (0x0011, 0xCD), (0x0012, 0xEF), (0x0020, 0x12), (0x0020, 0x34), (0x0020, 0x56)), //from / to locations
      3)
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.memory(0x0020) == 0xAB)
    assert(sysTest.get.memory(0x0021) == 0xCD)
    assert(sysTest.get.memory(0x0022) == 0xEF)
    assert(sysTest.get.memory(0x0010) == 0xAB)
    assert(sysTest.get.memory(0x0011) == 0xCD)
    assert(sysTest.get.memory(0x0012) == 0xEF)
    assert(sysTest.get.register(Regs.HL) == 0x000F)
    assert(sysTest.get.register(Regs.DE) == 0x001F)
    assert(sysTest.get.register(Regs.BC) == 0x0000)
    TestUtils.testFlags(sysTest.get.register, "11_0_101")
  }
}
