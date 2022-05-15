package org.kr.scala.z80.test

import org.kr.scala.z80.system.{ConsoleDebugger, Debugger, DummyDebugger, Regs}
import org.scalatest.funsuite.AnyFunSuite

class OpExchangeTypeTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  test("run EX DE,HL") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List((Regs.DE,0x0102),(Regs.HL,0x0304)),
      List((0,0xEB), // EX DE,HL
      ))
    //then
    assert(sysTest.get.register(Regs.PC) == 1)
    assert(sysTest.get.register(Regs.DE) == 0x0304)
    assert(sysTest.get.register(Regs.HL) == 0x0102)
  }

  test("run EX AF,AF1") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List((Regs.AF,0x0102),(Regs.AF1,0x0304)),
      List((0,0x08), // EX AF,AF1
      ))
    //then
    assert(sysTest.get.register(Regs.PC) == 1)
    assert(sysTest.get.register(Regs.AF) == 0x0304)
    assert(sysTest.get.register(Regs.AF1) == 0x0102)
  }

  test("run EXX") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List((Regs.AF,0xF1F2),(Regs.BC,0xF3F4),(Regs.DE,0xF5F6),(Regs.HL,0xF7F8),
      (Regs.AF1,0xA1A2),(Regs.BC1,0xA3A4),(Regs.DE1,0xA5A6),(Regs.HL1,0xA7A8)),
      List((0,0xD9), // EXX
      ))
    //then
    assert(sysTest.get.register(Regs.PC) == 1)
    assert(sysTest.get.register(Regs.AF) == 0xF1F2)
    assert(sysTest.get.register(Regs.AF1) == 0xA1A2)
    assert(sysTest.get.register(Regs.BC) == 0xA3A4)
    assert(sysTest.get.register(Regs.BC1) == 0xF3F4)
    assert(sysTest.get.register(Regs.DE) == 0xA5A6)
    assert(sysTest.get.register(Regs.DE1) == 0xF5F6)
    assert(sysTest.get.register(Regs.HL) == 0xA7A8)
    assert(sysTest.get.register(Regs.HL1) == 0xF7F8)
  }

  test("run EX (SP),HL") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List((Regs.HL,0xA1A2), (Regs.SP,0x0102)),
      List((0,0xE3), // EX (SP),HL
        (0x0102,0xF1),(0x0103,0xF2)
      ))
    //then
    assert(sysTest.get.register(Regs.PC) == 1)
    assert(sysTest.get.register(Regs.HL) == 0xF2F1)
    assert(sysTest.get.register(Regs.SP) == 0x0102)
    assert(sysTest.get.memory(0x0102)==0xA2)
    assert(sysTest.get.memory(0x0103)==0xA1)
  }

  test("run EX (SP),IX") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List((Regs.IX,0xB1B2),(Regs.SP,0x0103)),
      List((0,0xDD),(1,0xE3), // EX (SP),IX
        (0x0103,0xE1),(0x0104,0xE2)
      ))
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.register(Regs.IX) == 0xE2E1)
    assert(sysTest.get.register(Regs.SP) == 0x0103)
    assert(sysTest.get.memory(0x0103)==0xB2)
    assert(sysTest.get.memory(0x0104)==0xB1)
  }

  test("run EX (SP),IY") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List((Regs.IY,0xB1B2),(Regs.SP,0x0103)),
      List((0,0xFD),(1,0xE3), // EX (SP),IY
        (0x0103,0xE1),(0x0104,0xE2)
      ))
    //then
    assert(sysTest.get.register(Regs.PC) == 2)
    assert(sysTest.get.register(Regs.IY) == 0xE2E1)
    assert(sysTest.get.register(Regs.SP) == 0x0103)
    assert(sysTest.get.memory(0x0103)==0xB2)
    assert(sysTest.get.memory(0x0104)==0xB1)
  }
}
