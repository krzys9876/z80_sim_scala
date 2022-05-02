package org.kr.scala.z80.test

import org.kr.scala.z80.opcode.OpCode
import org.kr.scala.z80.system.{ConsoleDebugger, Debugger, DummyDebugger}
import org.scalatest.funsuite.AnyFunSuite

class OpArithmetic8BitTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  test("always pass") {
    assert(1 == 1)
  }

  private def testArithAccum(regList: List[(String, Int)], memList: List[(Int, Int)], result: Int,
                             flagsAsString: String, pcAfter: Int = 1): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.registerController.get("PC") == pcAfter)
    assert(sysTest.get.registerController.get("A") == result || result == OpCode.ANY)
    TestUtils.testFlags(sysTest.get.registerController.get, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.registerController.get.reg)
  }

  test("run ADD A,r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List(("A", 0x25), ("B", 0x3E)), List((0x0000, 0x80)), 0x63, "00_1_000")
    testArithAccum(List(("A", 0x0E), ("B", 0x02)), List((0x0000, 0x80)), 0x10, "00_1_000")
    testArithAccum(List(("A", 0x63), ("HL", 0x0102)), List((0x0000, 0x86), (0x0102, 0x3E)), 0xA1, "10_1_100")
    testArithAccum(List(("A", 0xA1), ("IX", 0x0202)), List((0x0000, 0xDD), (0x0001, 0x86), (0x0002, 0x01), (0x0203, 0x3E)), 0xDF, "10_0_000", 3)
    testArithAccum(List(("A", 0xDF), ("IY", 0x0108)), List((0x0000, 0xFD), (0x0001, 0x86), (0x0002, 0xFE), (0x0106, 0x3E)), 0x1D, "00_1_001", 3)

    testArithAccum(List(("A", 0x00)), List((0x0000, 0xC6), (0x0001, 0x40)), 0x40, "00_0_000", 2)
    testArithAccum(List(("A", 0x40), ("L", 0x40)), List((0x0000, 0x85)), 0x80, "10_0_100")
    testArithAccum(List(("A", 0x80), ("B", 0x40)), List((0x0000, 0x80)), 0xC0, "10_0_000")
    testArithAccum(List(("A", 0xC0), ("C", 0x40)), List((0x0000, 0x81)), 0x00, "01_0_001")

    testArithAccum(List(("A", 0x20), ("D", 0xF0)), List((0x0000, 0x82)), 0x10, "00_0_001")
    testArithAccum(List(("A", 0x10), ("E", 0xF0)), List((0x0000, 0x83)), 0x00, "01_0_001")
    testArithAccum(List(("A", 0x00), ("H", 0xF0)), List((0x0000, 0x84)), 0xF0, "10_0_000")
    testArithAccum(List(("A", 0xF0), ("A", 0xF0)), List((0x0000, 0x87)), 0xE0, "10_0_001")

    testArithAccum(List(("A", 0x80), ("B", 0xF0)), List((0x0000, 0x80)), 0x70, "00_0_101")
  }

  test("run ADC A,r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List(("F", 0x00), ("A", 0x9F), ("B", 0x3F)), List((0x0000, 0x88)), 0xDE, "10_1_000")
    testArithAccum(List(("F", 0x01), ("A", 0x0E), ("B", 0x01)), List((0x0000, 0x88)), 0x10, "00_1_000")
    testArithAccum(List(("F", 0x00), ("A", 0xDE), ("HL", 0x0123)), List((0x0000, 0x8E), (0x0123, 0x3F)), 0x1D, "00_1_001")
    testArithAccum(List(("F", 0x01), ("A", 0x1D), ("IX", 0x1111)), List((0x0000, 0xDD), (0x0001, 0x8E), (0x0002, 0xFF), (0x1110, 0x3F)), 0x5D, "00_1_000", 3)
    testArithAccum(List(("F", 0x01), ("A", 0x1D), ("IY", 0x1111)), List((0x0000, 0xFD), (0x0001, 0x8E), (0x0002, 0xFF), (0x1110, 0x3F)), 0x5D, "00_1_000", 3)

    testArithAccum(List(("F", 0x00), ("A", 0xC0)), List((0x0000, 0xCE), (0x0001, 0x40)), 0x00, "01_0_001", 2)
    testArithAccum(List(("F", 0x01), ("A", 0x20)), List((0x0000, 0x8F)), 0x41, "00_0_000")
    testArithAccum(List(("F", 0x01), ("A", 0x01), ("B", 0x40)), List((0x0000, 0x88)), 0x42, "00_0_000")
    testArithAccum(List(("F", 0x01), ("A", 0x02), ("C", 0x40)), List((0x0000, 0x89)), 0x43, "00_0_000")
    testArithAccum(List(("F", 0x01), ("A", 0x03), ("D", 0x40)), List((0x0000, 0x8A)), 0x44, "00_0_000")
    testArithAccum(List(("F", 0x01), ("A", 0x04), ("E", 0x40)), List((0x0000, 0x8B)), 0x45, "00_0_000")
    testArithAccum(List(("F", 0x01), ("A", 0x05), ("H", 0x40)), List((0x0000, 0x8C)), 0x46, "00_0_000")
    testArithAccum(List(("F", 0x01), ("A", 0x06), ("L", 0x40)), List((0x0000, 0x8D)), 0x47, "00_0_000")
  }

  test("run SUB A,r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List(("A", 0x1D), ("B", 0x3E)), List((0x0000, 0x90)), 0xDF, "10_1_011")
    testArithAccum(List(("A", 0x10), ("B", 0x01)), List((0x0000, 0x90)), 0x0F, "00_1_010")
    testArithAccum(List(("A", 0xDF), ("IX", 0x0302)), List((0x0000, 0xDD), (0x0001, 0x96), (0x0002, 0x01), (0x0303, 0x3E)), 0xA1, "10_0_010", 3)
    testArithAccum(List(("A", 0xA1), ("IY", 0x0405)), List((0x0000, 0xFD), (0x0001, 0x96), (0x0002, 0xFD), (0x0402, 0x3E)), 0x63, "00_1_110", 3)
    testArithAccum(List(("A", 0x63), ("HL", 0x0503)), List((0x0000, 0x96), (0x0503, 0x3E)), 0x25, "00_1_010")

    testArithAccum(List(("A", 0x00)), List((0x0000, 0xD6), (0x0001, 0x40)), 0xC0, "10_0_011", 2)
    testArithAccum(List(("A", 0xC0), ("L", 0x40)), List((0x0000, 0x95)), 0x80, "10_0_010")
    testArithAccum(List(("A", 0x80), ("B", 0x40)), List((0x0000, 0x90)), 0x40, "00_0_110")
    testArithAccum(List(("A", 0x40), ("C", 0x40)), List((0x0000, 0x91)), 0x00, "01_0_010")

    testArithAccum(List(("A", 0xE0), ("D", 0xF0)), List((0x0000, 0x92)), 0xF0, "10_0_011")
    testArithAccum(List(("A", 0xF0), ("A", 0xF0)), List((0x0000, 0x97)), 0x00, "01_0_010")
    testArithAccum(List(("A", 0x00), ("H", 0xF0)), List((0x0000, 0x94)), 0x10, "00_0_011")
    testArithAccum(List(("A", 0x10), ("L", 0xF0)), List((0x0000, 0x95)), 0x20, "00_0_011")

    testArithAccum(List(("A", 0x70), ("B", 0xF0)), List((0x0000, 0x90)), 0x80, "10_0_111")
  }

  test("run SBC A,r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List(("F", 0x00), ("A", 0x1D), ("B", 0x3F)), List((0x0000, 0x98)), 0xDE, "10_1_011")
    testArithAccum(List(("F", 0x01), ("A", 0x11), ("B", 0x01)), List((0x0000, 0x98)), 0x0F, "00_1_010")
    testArithAccum(List(("F", 0x01), ("A", 0xDE), ("HL", 0x0123)), List((0x0000, 0x9E), (0x0123, 0x3F)), 0x9E, "10_1_010")
    testArithAccum(List(("F", 0x01), ("A", 0xDE), ("IX", 0x1111)), List((0x0000, 0xDD), (0x0001, 0x9E), (0x0002, 0x01), (0x1112, 0x3F)), 0x9E, "10_1_010", 3)
    testArithAccum(List(("F", 0x01), ("A", 0xDE), ("IY", 0x1111)), List((0x0000, 0xFD), (0x0001, 0x9E), (0x0002, 0x02), (0x1113, 0x3F)), 0x9E, "10_1_010", 3)

    testArithAccum(List(("F", 0x00), ("A", 0x00)), List((0x0000, 0xDE), (0x0001, 0x40)), 0xC0, "10_0_011", 2)
    testArithAccum(List(("F", 0x01), ("A", 0x20)), List((0x0000, 0x9F)), 0xFF, "10_1_011")
    testArithAccum(List(("F", 0x01), ("A", 0x51), ("B", 0x4A)), List((0x0000, 0x98)), 0x06, "00_1_010")
    testArithAccum(List(("F", 0x01), ("A", 0x52), ("C", 0x4A)), List((0x0000, 0x99)), 0x07, "00_1_010")
    testArithAccum(List(("F", 0x01), ("A", 0x53), ("D", 0x4A)), List((0x0000, 0x9A)), 0x08, "00_1_010")
    testArithAccum(List(("F", 0x01), ("A", 0x54), ("E", 0x40)), List((0x0000, 0x9B)), 0x13, "00_0_010")
    testArithAccum(List(("F", 0x01), ("A", 0x56), ("L", 0x40)), List((0x0000, 0x9D)), 0x15, "00_0_010")
  }

  test("run AND r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List(("A", 0x00)), List((0x0000, 0xA7)), 0x00, "01_1_100")
    testArithAccum(List(("A", 0xFE)), List((0x0000, 0xA7)), 0xFE, "10_1_000")
    testArithAccum(List(("A", 0xEF), ("B", 0xFE)), List((0x0000, 0xA0)), 0xEE, "10_1_100")
    testArithAccum(List(("A", 0xEC), ("C", 0x04)), List((0x0000, 0xA1)), 0x04, "00_1_000")
    testArithAccum(List(("A", 0xF0), ("D", 0xA2)), List((0x0000, 0xA2)), 0xA0, "10_1_100")
    testArithAccum(List(("A", 0xF0), ("E", 0xA2)), List((0x0000, 0xA3)), 0xA0, "10_1_100")
    testArithAccum(List(("A", 0xF0), ("H", 0xA2)), List((0x0000, 0xA4)), 0xA0, "10_1_100")
    testArithAccum(List(("A", 0xF0), ("L", 0xA2)), List((0x0000, 0xA5)), 0xA0, "10_1_100")
    testArithAccum(List(("A", 0xF0), ("HL", 0x1010)), List((0x0000, 0xA6), (0x1010, 0xA2)), 0xA0, "10_1_100")
    testArithAccum(List(("A", 0xF0), ("IX", 0x2020)), List((0x0000, 0xDD), (0x0001, 0xA6), (0x0002, 0x10), (0x2030, 0xA2)), 0xA0, "10_1_100", 3)
    testArithAccum(List(("A", 0xF0), ("IY", 0x2020)), List((0x0000, 0xFD), (0x0001, 0xA6), (0x0002, 0xF0), (0x2010, 0xA2)), 0xA0, "10_1_100", 3)
    testArithAccum(List(("A", 0xF0)), List((0x0000, 0xE6), (0x0001, 0xA2)), 0xA0, "10_1_100", 2)
  }

  test("run XOR r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List(("A", 0x11)), List((0x0000, 0xAF)), 0x00, "01_0_100")
    testArithAccum(List(("A", 0xEF), ("B", 0xFE)), List((0x0000, 0xA8)), 0x11, "00_0_100")
    testArithAccum(List(("A", 0xEF), ("C", 0xFF)), List((0x0000, 0xA9)), 0x10, "00_0_000")
    testArithAccum(List(("A", 0xFF), ("D", 0x00)), List((0x0000, 0xAA)), 0xFF, "10_0_100")
    testArithAccum(List(("A", 0xFC), ("E", 0xFF)), List((0x0000, 0xAB)), 0x03, "00_0_100")
    testArithAccum(List(("A", 0xF0), ("H", 0xCE)), List((0x0000, 0xAC)), 0x3E, "00_0_000")
    testArithAccum(List(("A", 0xF0), ("L", 0xCE)), List((0x0000, 0xAD)), 0x3E, "00_0_000")
    testArithAccum(List(("A", 0xF0), ("HL", 0x1011)), List((0x0000, 0xAE), (0x1011, 0xCE)), 0x3E, "00_0_000")
    testArithAccum(List(("A", 0xF0), ("IX", 0x2120)), List((0x0000, 0xDD), (0x0001, 0xAE), (0x0002, 0x10), (0x2130, 0xCE)), 0x3E, "00_0_000", 3)
    testArithAccum(List(("A", 0xF0), ("IY", 0x2120)), List((0x0000, 0xFD), (0x0001, 0xAE), (0x0002, 0xF0), (0x2110, 0xCE)), 0x3E, "00_0_000", 3)
    testArithAccum(List(("A", 0xF0)), List((0x0000, 0xEE), (0x0001, 0xCE)), 0x3E, "00_0_000", 2)
  }

  test("run OR r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List(("A", 0x00)), List((0x0000, 0xB7)), 0x00, "01_0_100")
    testArithAccum(List(("A", 0xAB)), List((0x0000, 0xB7)), 0xAB, "10_0_000")
    testArithAccum(List(("A", 0x30), ("B", 0x02)), List((0x0000, 0xB0)), 0x32, "00_0_000")
    testArithAccum(List(("A", 0x40), ("C", 0x03)), List((0x0000, 0xB1)), 0x43, "00_0_000")
    testArithAccum(List(("A", 0x50), ("D", 0x04)), List((0x0000, 0xB2)), 0x54, "00_0_000")
    testArithAccum(List(("A", 0x60), ("E", 0x05)), List((0x0000, 0xB3)), 0x65, "00_0_100")
    testArithAccum(List(("A", 0x70), ("H", 0x06)), List((0x0000, 0xB4)), 0x76, "00_0_000")
    testArithAccum(List(("A", 0x80), ("L", 0x07)), List((0x0000, 0xB5)), 0x87, "10_0_100")
    testArithAccum(List(("A", 0x80), ("HL", 0x1010)), List((0x0000, 0xB6), (0x1010, 0x07)), 0x87, "10_0_100")
    testArithAccum(List(("A", 0x80), ("IX", 0x1020)), List((0x0000, 0xDD), (0x0001, 0xB6), (0x0002, 0x10), (0x1030, 0x07)), 0x87, "10_0_100", 3)
    testArithAccum(List(("A", 0x80), ("IY", 0x1020)), List((0x0000, 0xFD), (0x0001, 0xB6), (0x0002, 0xF0), (0x1010, 0x07)), 0x87, "10_0_100", 3)
    testArithAccum(List(("A", 0x80)), List((0x0000, 0xF6), (0x0001, 0x07)), 0x87, "10_0_100", 2)
  }

  test("run CP r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List(("A", 0x1D), ("B", 0x3E)), List((0x0000, 0xB8)), OpCode.ANY, "10_1_011")
    testArithAccum(List(("A", 0xDF), ("IX", 0x0302)), List((0x0000, 0xDD), (0x0001, 0xBE), (0x0002, 0x01), (0x0303, 0x3E)), OpCode.ANY, "10_0_010", 3)
    testArithAccum(List(("A", 0xA1), ("IY", 0x0405)), List((0x0000, 0xFD), (0x0001, 0xBE), (0x0002, 0xFD), (0x0402, 0x3E)), OpCode.ANY, "00_1_110", 3)
    testArithAccum(List(("A", 0x63), ("HL", 0x0503)), List((0x0000, 0xBE), (0x0503, 0x3E)), OpCode.ANY, "00_1_010")

    testArithAccum(List(("A", 0x00)), List((0x0000, 0xFE), (0x0001, 0x40)), OpCode.ANY, "10_0_011", 2)
    testArithAccum(List(("A", 0xC0), ("E", 0x40)), List((0x0000, 0xBB)), OpCode.ANY, "10_0_010")
    testArithAccum(List(("A", 0x80), ("B", 0x40)), List((0x0000, 0xB8)), OpCode.ANY, "00_0_110")
    testArithAccum(List(("A", 0x40), ("C", 0x40)), List((0x0000, 0xB9)), OpCode.ANY, "01_0_010")

    testArithAccum(List(("A", 0xE0), ("D", 0xF0)), List((0x0000, 0xBA)), OpCode.ANY, "10_0_011")
    testArithAccum(List(("A", 0xF0)), List((0x0000, 0xBF)), OpCode.ANY, "01_0_010")
    testArithAccum(List(("A", 0x00), ("H", 0xF0)), List((0x0000, 0xBC)), OpCode.ANY, "00_0_011")
    testArithAccum(List(("A", 0x10), ("L", 0xF0)), List((0x0000, 0xBD)), OpCode.ANY, "00_0_011")

    testArithAccum(List(("A", 0x70), ("B", 0xF0)), List((0x0000, 0xB8)), OpCode.ANY, "10_0_111")
  }

  test("run INC r/(HL)/(IX+d)/(IY+d)") {
    // based on "real" Z80 emulator
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x00)), List((0x0000, 0x3C)), "A",0, 0x01, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x01)), List((0x0000, 0x04)), "B",0, 0x02, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("C", 0x3F)), List((0x0000, 0x0C)), "C",0, 0x40, "00_1_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("D", 0xFE)), List((0x0000, 0x14)), "D",0, 0xFF, "10_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("E", 0xFE)), List((0x0000, 0x1C)), "E",0, 0xFF, "10_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("H", 0xFF)), List((0x0000, 0x24)), "H",0, 0x00, "01_1_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("L", 0x7F)), List((0x0000, 0x2C)), "L",0, 0x80, "10_1_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x0102)), List((0x0000, 0x34),(0x0102,0x7F)), "",0x0102, 0x80, "10_1_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x0201)), List((0x0000, 0xDD),(0x0001, 0x34),(0x0002, 0x02),(0x0203,0x7F)),
      "",0x0203, 0x80, "10_1_100",3)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IY", 0x0301)), List((0x0000, 0xFD),(0x0001, 0x34),(0x0002, 0xFF),(0x0300,0x7F)),
      "",0x0300, 0x80, "10_1_100",3)
  }

  test("run DEC r/(HL)/(IX+d)/(IY+d)") {
    // based on "real" Z80 emulator
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x3D)), "A",0, 0x00, "01_0_010")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x02)), List((0x0000, 0x05)), "B",0, 0x01, "00_0_010")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("C", 0x40)), List((0x0000, 0x0D)), "C",0, 0x3F, "00_1_010")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("D", 0xFF)), List((0x0000, 0x15)), "D",0, 0xFE, "10_0_010")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("E", 0xFF)), List((0x0000, 0x1D)), "E",0, 0xFE, "10_0_011")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("H", 0x00)), List((0x0000, 0x25)), "H",0, 0xFF, "10_1_010")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("L", 0x80)), List((0x0000, 0x2D)), "L",0, 0x7F, "00_1_110")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x0102)), List((0x0000, 0x35),(0x0102,0x80)), "",0x0102, 0x7F, "00_1_110")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x0201)), List((0x0000, 0xDD),(0x0001, 0x35),(0x0002, 0x02),(0x0203,0x80)),
      "",0x0203, 0x7F, "00_1_110",3)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IY", 0x0301)), List((0x0000, 0xFD),(0x0001, 0x35),(0x0002, 0xFF),(0x0300,0x80)),
      "",0x0300, 0x7F, "00_1_110",3)
  }

  test("run CPL") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x2F)), "A",OpCode.ANY, 0xFE, "00_1_010")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x01)), List((0x0000, 0x2F)), "A",OpCode.ANY, 0xFE, "11_1_111")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x55)), List((0x0000, 0x2F)), "A",OpCode.ANY, 0xAA, "00_1_010")
  }

  test("run NEG") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0xED),(0x0001, 0x44)), "A",OpCode.ANY, 0xFF, "10_1_011",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x00)), List((0x0000, 0xED),(0x0001, 0x44)), "A",OpCode.ANY, 0x00, "01_0_010",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0xFF)), List((0x0000, 0xED),(0x0001, 0x44)), "A",OpCode.ANY, 0x01, "00_1_011",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x7F)), List((0x0000, 0xED),(0x0001, 0x44)), "A",OpCode.ANY, 0x81, "10_1_011",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x80)), List((0x0000, 0xED),(0x0001, 0x44)), "A",OpCode.ANY, 0x80, "10_0_111",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x40)), List((0x0000, 0xED),(0x0001, 0x44)), "A",OpCode.ANY, 0xC0, "10_0_011",2)
  }

  test("run CCF") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x00)), List((0x0000, 0x3F)), "",OpCode.ANY, OpCode.ANY, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("A", 0x00)), List((0x0000, 0x3F)), "",OpCode.ANY, OpCode.ANY, "00_1_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x00)), List((0x0000, 0x3F)), "",OpCode.ANY, OpCode.ANY, "11_1_100")
  }

  test("run SCF") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x00)), List((0x0000, 0x37)), "",OpCode.ANY, OpCode.ANY, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("A", 0x00)), List((0x0000, 0x37)), "",OpCode.ANY, OpCode.ANY, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x00)), List((0x0000, 0x37)), "",OpCode.ANY, OpCode.ANY, "11_0_101")
  }

}