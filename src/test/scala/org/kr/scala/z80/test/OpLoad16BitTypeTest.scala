package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger}
import org.scalatest.funsuite.AnyFunSuite

class OpLoad16BitTypeTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  test("run LD dd,nn") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("H",0x01),("L",0x02)),
      List((0,0x01),(1,0x01),(2,0x02), //LD BC,nn
        (3,0x11),(4,0x03),(5,0x04), //LD DE,nn
        (6,0x21),(7,0x05),(8,0x06), //LD HL,nn
        (9,0x31),(10,0x07),(11,0x08), //LD SP,nn
        (12,0xDD),(13,0x21),(14,0x09),(15,0x0A), //LD IX,nn
        (16,0xFD),(17,0x21),(18,0x0B),(19,0x0C), //LD IY,nn
        ),6)
    //then
    assert(sysTest.get.registerController.get("PC") == 20)
    assert(sysTest.get.registerController.get("B") == 2)
    assert(sysTest.get.registerController.get("C") == 1)
    assert(sysTest.get.registerController.get("D") == 4)
    assert(sysTest.get.registerController.get("E") == 3)
    assert(sysTest.get.registerController.get("H") == 6)
    assert(sysTest.get.registerController.get("L") == 5)
    assert(sysTest.get.registerController.get("SP") == 0x0807)
    assert(sysTest.get.registerController.get("IX") == 0x0A09)
    assert(sysTest.get.registerController.get("IY") == 0x0C0B)
  }

  test("run LD dd,(nn)") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(),
      List((0,0xED),(1,0x4B),(2,0x01),(3,0x02), //LD BC,(nn)
        (4,0xED),(5,0x5B),(6,0x03),(7,0x04), //LD DE,(nn)
        (8,0x2A),(9,0x05),(10,0x06), //LD HL,(nn)
        (11,0xED),(12,0x7B),(13,0x07),(14,0x08), //LD SP,(nn)
        (15,0xDD),(16,0x2A),(17,0x09),(18,0x0A), //LD IX,(nn)
        (19,0xFD),(20,0x2A),(21,0x0B),(22,0x0C), //LD IY,(nn)
        (0x0201,0x10),(0x0202,0x11),
        (0x0403,0x12),(0x0404,0x13),
        (0x0605,0x14),(0x0606,0x15),
        (0x0807,0x16),(0x0808,0x17),
        (0x0A09,0x18),(0x0A0A,0x19),
        (0x0C0B,0x1A),(0x0C0C,0x1B)
      ),6)
    //then
    assert(sysTest.get.registerController.get("PC") == 23)
    assert(sysTest.get.registerController.get("B") == 0x11)
    assert(sysTest.get.registerController.get("C") == 0x10)
    assert(sysTest.get.registerController.get("D") == 0x13)
    assert(sysTest.get.registerController.get("E") == 0x12)
    assert(sysTest.get.registerController.get("H") == 0x15)
    assert(sysTest.get.registerController.get("L") == 0x14)
    assert(sysTest.get.registerController.get("SP") == 0x1716)
    assert(sysTest.get.registerController.get("IX") == 0x1918)
    assert(sysTest.get.registerController.get("IY") == 0x1B1A)
  }

  test("run LD SP,HL") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("H",0x01),("L",0x02)),
      List((0,0xF9)))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("SP") == 0x0102)
  }

  test("run LD SP,IX") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("IX",0x0304)),
      List((0,0xDD),(1,0xF9)))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IX") == 0x0304)
  }

  test("run LD SP,IY") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("IY",0x0405)),
      List((0,0xFD),(1,0xF9)))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IY") == 0x0405)
  }

  test("run POP AF") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("SP",0x0102)),
      List((0,0xF1),(0x0102,0xF1),(0x0103,0xF2)))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("A") == 0xF2)
    assert(sysTest.get.registerController.get("F") == 0xF1)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
  }

  test("run POP BC") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("SP",0x0102)),
      List((0,0xC1),(0x0102,0xF3),(0x0103,0xF4)))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("B") == 0xF4)
    assert(sysTest.get.registerController.get("C") == 0xF3)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
  }

  test("run POP DE") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("SP",0x0102)),
      List((0,0xD1),(0x0102,0xF5),(0x0103,0xF6)))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("D") == 0xF6)
    assert(sysTest.get.registerController.get("E") == 0xF5)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
  }

  test("run POP HL") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("SP",0x0102)),
      List((0,0xE1),(0x0102,0xF7),(0x0103,0xF8)))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("H") == 0xF8)
    assert(sysTest.get.registerController.get("L") == 0xF7)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
  }

  test("run POP IX") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("SP",0x0102)),
      List((0,0xDD),(1,0xE1),(0x0102,0xF9),(0x0103,0xFA)))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IX") == 0xFAF9)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
  }

  test("run POP IY") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("SP",0x0102)),
      List((0,0xFD),(1,0xE1),(0x0102,0xFB),(0x0103,0xFC)))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("IY") == 0xFCFB)
    assert(sysTest.get.registerController.get("SP") == 0x0104)
  }

  test("run LD (nn),dd") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("B",0x11),("C",0x12),("D",0x13),("E",0x14),("H",0x15),("L",0x16),
      ("SP",0x1817),("IX",0x1A19),("IY",0x1C1B)),
      List((0,0xED),(1,0x43),(2,0x02),(3,0x01),
        (4,0xED),(5,0x53),(6,0x04),(7,0x03),
        (8,0x22),(9,0x06),(10,0x05),
        (11,0xED),(12,0x73),(13,0x08),(14,0x07),
        (15,0xDD),(16,0x22),(17,0x0A),(18,0x09),
        (19,0xFD),(20,0x22),(21,0x0C),(22,0x0B)
      ),6)
    //then
    assert(sysTest.get.registerController.get("PC") == 23)
    assert(sysTest.get.memoryController.get(0x0102) == 0x12)
    assert(sysTest.get.memoryController.get(0x0103) == 0x11)
    assert(sysTest.get.memoryController.get(0x0304) == 0x14)
    assert(sysTest.get.memoryController.get(0x0305) == 0x13)
    assert(sysTest.get.memoryController.get(0x0506) == 0x16)
    assert(sysTest.get.memoryController.get(0x0507) == 0x15)
    assert(sysTest.get.memoryController.get(0x0708) == 0x17)
    assert(sysTest.get.memoryController.get(0x0709) == 0x18)
    assert(sysTest.get.memoryController.get(0x090A) == 0x19)
    assert(sysTest.get.memoryController.get(0x090B) == 0x1A)
    assert(sysTest.get.memoryController.get(0x0B0C) == 0x1B)
    assert(sysTest.get.memoryController.get(0x0B0D) == 0x1C)
  }

  test("run PUSH qq") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("SP",0x0100),
      ("A",0x02),("F",0x03),("B",0x04),("C",0x05),("D",0x06),("E",0x07),
      ("H",0x08),("L",0x09),("IX",0x0A0B),("IY",0x00C0D)),
      List((0,0xF5), // PUSH AF
        (1,0xC5), // PUSH BC
        (2,0xD5), // PUSH DE
        (3,0xE5), // PUSH HL
        (4,0xDD),(5,0xE5), // PUSH IX
        (6,0xFD),(7,0xE5) // PUSH IY
      ),6)
    //then
    assert(sysTest.get.registerController.get("PC") == 8)
    assert(sysTest.get.memoryController.get(0x00FF) == 0x02)
    assert(sysTest.get.memoryController.get(0x00FE) == 0x03)
    assert(sysTest.get.memoryController.get(0x00FD) == 0x04)
    assert(sysTest.get.memoryController.get(0x00FC) == 0x05)
    assert(sysTest.get.memoryController.get(0x00FB) == 0x06)
    assert(sysTest.get.memoryController.get(0x00FA) == 0x07)
    assert(sysTest.get.memoryController.get(0x00F9) == 0x08)
    assert(sysTest.get.memoryController.get(0x00F8) == 0x09)
    assert(sysTest.get.memoryController.get(0x00F7) == 0x0A)
    assert(sysTest.get.memoryController.get(0x00F6) == 0x0B)
    assert(sysTest.get.memoryController.get(0x00F5) == 0x0C)
    assert(sysTest.get.memoryController.get(0x00F4) == 0x0D)
    assert(sysTest.get.registerController.get("SP") == 0x00F4)
  }
}
