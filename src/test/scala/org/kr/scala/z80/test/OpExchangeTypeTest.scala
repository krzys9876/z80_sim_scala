package org.kr.scala.z80.test

import org.kr.scala.z80.system.{ConsoleDebugger, Debugger, DummyDebugger}
import org.scalatest.funsuite.AnyFunSuite

class OpExchangeTypeTest extends AnyFunSuite {

  implicit val debugger:Debugger=ConsoleDebugger

  test("run EX DE,HL") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("DE",0x0102),("HL",0x0304)),
      List((0,0xEB), // EX DE,HL
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("DE") == 0x0304)
    assert(sysTest.get.registerController.get("HL") == 0x0102)
  }

  test("run EX AF,AF1") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("AF",0x0102),("AF1",0x0304)),
      List((0,0x08), // EX AF,AF1
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("AF") == 0x0304)
    assert(sysTest.get.registerController.get("AF1") == 0x0102)
  }

  test("run EXX") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("AF",0xF1F2),("BC",0xF3F4),("DE",0xF5F6),("HL",0xF7F8),
      ("AF1",0xA1A2),("BC1",0xA3A4),("DE1",0xA5A6),("HL1",0xA7A8)),
      List((0,0xD9), // EXX
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("AF") == 0xF1F2)
    assert(sysTest.get.registerController.get("AF1") == 0xA1A2)
    assert(sysTest.get.registerController.get("BC") == 0xA3A4)
    assert(sysTest.get.registerController.get("BC1") == 0xF3F4)
    assert(sysTest.get.registerController.get("DE") == 0xA5A6)
    assert(sysTest.get.registerController.get("DE1") == 0xF5F6)
    assert(sysTest.get.registerController.get("HL") == 0xA7A8)
    assert(sysTest.get.registerController.get("HL1") == 0xF7F8)
  }

  test("run EX (SP),HL") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("HL",0xA1A2), ("SP",0x0102)),
      List((0,0xE3), // EX (SP),HL
        (0x0102,0xF1),(0x0103,0xF2)
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("HL") == 0xF2F1)
    assert(sysTest.get.registerController.get("SP") == 0x0102)
    assert(sysTest.get.memoryController.get(0x0102)==0xA2)
    assert(sysTest.get.memoryController.get(0x0103)==0xA1)
  }

  test("run EX (SP),IX") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("IX",0xB1B2),("SP",0x0103)),
      List((0,0xDD),(1,0xE3), // EX (SP),IX
        (0x0103,0xE1),(0x0104,0xE2)
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IX") == 0xE2E1)
    assert(sysTest.get.registerController.get("SP") == 0x0103)
    assert(sysTest.get.memoryController.get(0x0103)==0xB2)
    assert(sysTest.get.memoryController.get(0x0104)==0xB1)
  }

  test("run EX (SP),IY") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("IY",0xB1B2),("SP",0x0103)),
      List((0,0xFD),(1,0xE3), // EX (SP),IY
        (0x0103,0xE1),(0x0104,0xE2)
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IY") == 0xE2E1)
    assert(sysTest.get.registerController.get("SP") == 0x0103)
    assert(sysTest.get.memoryController.get(0x0103)==0xB2)
    assert(sysTest.get.memoryController.get(0x0104)==0xB1)
  }
}
