package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, RegSymbol, Regs}
import org.kr.scala.z80.utils.{AnyInt, IntValue, OptionInt}
import org.scalatest.funsuite.AnyFunSuite

class OpArithmetic8BitTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  test("always pass") {
    assert(1 == 1)
  }

  private def testArithAccum(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], result: Int,
                             flagsAsString: String, pcAfter: Int = 1): Unit = {
    //given when
    val sysTest = TestUtils.prepareTest(regList, memList)
    //then
    assert(sysTest.get.register(Regs.PC) == pcAfter)
    assert(sysTest.get.register(Regs.A) == result)
    TestUtils.testFlags(sysTest.get.register, flagsAsString)
    //println(sysTest.get.memoryController.get.mem.slice(0,300))
    //println(sysTest.get.register.reg)
  }

  test("run ADD A,r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List((Regs.A, 0x25), (Regs.B, 0x3E)), List((0x0000, 0x80)), 0x63, "00_1_000")
    testArithAccum(List((Regs.A, 0x0E), (Regs.B, 0x02)), List((0x0000, 0x80)), 0x10, "00_1_000")
    testArithAccum(List((Regs.A, 0x63), (Regs.HL, 0x0102)), List((0x0000, 0x86), (0x0102, 0x3E)), 0xA1, "10_1_100")
    testArithAccum(List((Regs.A, 0xA1), (Regs.IX, 0x0202)), List((0x0000, 0xDD), (0x0001, 0x86), (0x0002, 0x01), (0x0203, 0x3E)), 0xDF, "10_0_000", 3)
    testArithAccum(List((Regs.A, 0xDF), (Regs.IY, 0x0108)), List((0x0000, 0xFD), (0x0001, 0x86), (0x0002, 0xFE), (0x0106, 0x3E)), 0x1D, "00_1_001", 3)

    testArithAccum(List((Regs.A, 0x00)), List((0x0000, 0xC6), (0x0001, 0x40)), 0x40, "00_0_000", 2)
    testArithAccum(List((Regs.A, 0x40), (Regs.L, 0x40)), List((0x0000, 0x85)), 0x80, "10_0_100")
    testArithAccum(List((Regs.A, 0x80), (Regs.B, 0x40)), List((0x0000, 0x80)), 0xC0, "10_0_000")
    testArithAccum(List((Regs.A, 0xC0), (Regs.C, 0x40)), List((0x0000, 0x81)), 0x00, "01_0_001")

    testArithAccum(List((Regs.A, 0x20), (Regs.D, 0xF0)), List((0x0000, 0x82)), 0x10, "00_0_001")
    testArithAccum(List((Regs.A, 0x10), (Regs.E, 0xF0)), List((0x0000, 0x83)), 0x00, "01_0_001")
    testArithAccum(List((Regs.A, 0x00), (Regs.H, 0xF0)), List((0x0000, 0x84)), 0xF0, "10_0_000")
    testArithAccum(List((Regs.A, 0xF0), (Regs.A, 0xF0)), List((0x0000, 0x87)), 0xE0, "10_0_001")

    testArithAccum(List((Regs.A, 0x80), (Regs.B, 0xF0)), List((0x0000, 0x80)), 0x70, "00_0_101")
  }

  test("run ADC A,r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List((Regs.F, 0x00), (Regs.A, 0x9F), (Regs.B, 0x3F)), List((0x0000, 0x88)), 0xDE, "10_1_000")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x0E), (Regs.B, 0x01)), List((0x0000, 0x88)), 0x10, "00_1_000")
    testArithAccum(List((Regs.F, 0x00), (Regs.A, 0xDE), (Regs.HL, 0x0123)), List((0x0000, 0x8E), (0x0123, 0x3F)), 0x1D, "00_1_001")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x1D), (Regs.IX, 0x1111)), List((0x0000, 0xDD), (0x0001, 0x8E), (0x0002, 0xFF), (0x1110, 0x3F)), 0x5D, "00_1_000", 3)
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x1D), (Regs.IY, 0x1111)), List((0x0000, 0xFD), (0x0001, 0x8E), (0x0002, 0xFF), (0x1110, 0x3F)), 0x5D, "00_1_000", 3)

    testArithAccum(List((Regs.F, 0x00), (Regs.A, 0xC0)), List((0x0000, 0xCE), (0x0001, 0x40)), 0x00, "01_0_001", 2)
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x20)), List((0x0000, 0x8F)), 0x41, "00_0_000")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x01), (Regs.B, 0x40)), List((0x0000, 0x88)), 0x42, "00_0_000")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x02), (Regs.C, 0x40)), List((0x0000, 0x89)), 0x43, "00_0_000")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x03), (Regs.D, 0x40)), List((0x0000, 0x8A)), 0x44, "00_0_000")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x04), (Regs.E, 0x40)), List((0x0000, 0x8B)), 0x45, "00_0_000")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x05), (Regs.H, 0x40)), List((0x0000, 0x8C)), 0x46, "00_0_000")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x06), (Regs.L, 0x40)), List((0x0000, 0x8D)), 0x47, "00_0_000")
  }

  test("run SUB A,r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List((Regs.A, 0x1D), (Regs.B, 0x3E)), List((0x0000, 0x90)), 0xDF, "10_1_011")
    testArithAccum(List((Regs.A, 0x10), (Regs.B, 0x01)), List((0x0000, 0x90)), 0x0F, "00_1_010")
    testArithAccum(List((Regs.A, 0xDF), (Regs.IX, 0x0302)), List((0x0000, 0xDD), (0x0001, 0x96), (0x0002, 0x01), (0x0303, 0x3E)), 0xA1, "10_0_010", 3)
    testArithAccum(List((Regs.A, 0xA1), (Regs.IY, 0x0405)), List((0x0000, 0xFD), (0x0001, 0x96), (0x0002, 0xFD), (0x0402, 0x3E)), 0x63, "00_1_110", 3)
    testArithAccum(List((Regs.A, 0x63), (Regs.HL, 0x0503)), List((0x0000, 0x96), (0x0503, 0x3E)), 0x25, "00_1_010")

    testArithAccum(List((Regs.A, 0x00)), List((0x0000, 0xD6), (0x0001, 0x40)), 0xC0, "10_0_011", 2)
    testArithAccum(List((Regs.A, 0xC0), (Regs.L, 0x40)), List((0x0000, 0x95)), 0x80, "10_0_010")
    testArithAccum(List((Regs.A, 0x80), (Regs.B, 0x40)), List((0x0000, 0x90)), 0x40, "00_0_110")
    testArithAccum(List((Regs.A, 0x40), (Regs.C, 0x40)), List((0x0000, 0x91)), 0x00, "01_0_010")

    testArithAccum(List((Regs.A, 0xE0), (Regs.D, 0xF0)), List((0x0000, 0x92)), 0xF0, "10_0_011")
    testArithAccum(List((Regs.A, 0xF0), (Regs.A, 0xF0)), List((0x0000, 0x97)), 0x00, "01_0_010")
    testArithAccum(List((Regs.A, 0x00), (Regs.H, 0xF0)), List((0x0000, 0x94)), 0x10, "00_0_011")
    testArithAccum(List((Regs.A, 0x10), (Regs.L, 0xF0)), List((0x0000, 0x95)), 0x20, "00_0_011")

    testArithAccum(List((Regs.A, 0x70), (Regs.B, 0xF0)), List((0x0000, 0x90)), 0x80, "10_0_111")
  }

  test("run SBC A,r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List((Regs.F, 0x00), (Regs.A, 0x1D), (Regs.B, 0x3F)), List((0x0000, 0x98)), 0xDE, "10_1_011")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x11), (Regs.B, 0x01)), List((0x0000, 0x98)), 0x0F, "00_1_010")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0xDE), (Regs.HL, 0x0123)), List((0x0000, 0x9E), (0x0123, 0x3F)), 0x9E, "10_1_010")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0xDE), (Regs.IX, 0x1111)), List((0x0000, 0xDD), (0x0001, 0x9E), (0x0002, 0x01), (0x1112, 0x3F)), 0x9E, "10_1_010", 3)
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0xDE), (Regs.IY, 0x1111)), List((0x0000, 0xFD), (0x0001, 0x9E), (0x0002, 0x02), (0x1113, 0x3F)), 0x9E, "10_1_010", 3)

    testArithAccum(List((Regs.F, 0x00), (Regs.A, 0x00)), List((0x0000, 0xDE), (0x0001, 0x40)), 0xC0, "10_0_011", 2)
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x20)), List((0x0000, 0x9F)), 0xFF, "10_1_011")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x51), (Regs.B, 0x4A)), List((0x0000, 0x98)), 0x06, "00_1_010")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x52), (Regs.C, 0x4A)), List((0x0000, 0x99)), 0x07, "00_1_010")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x53), (Regs.D, 0x4A)), List((0x0000, 0x9A)), 0x08, "00_1_010")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x54), (Regs.E, 0x40)), List((0x0000, 0x9B)), 0x13, "00_0_010")
    testArithAccum(List((Regs.F, 0x01), (Regs.A, 0x56), (Regs.L, 0x40)), List((0x0000, 0x9D)), 0x15, "00_0_010")
  }

  test("run AND r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List((Regs.A, 0x00)), List((0x0000, 0xA7)), 0x00, "01_1_100")
    testArithAccum(List((Regs.A, 0xFE)), List((0x0000, 0xA7)), 0xFE, "10_1_000")
    testArithAccum(List((Regs.A, 0xEF), (Regs.B, 0xFE)), List((0x0000, 0xA0)), 0xEE, "10_1_100")
    testArithAccum(List((Regs.A, 0xEC), (Regs.C, 0x04)), List((0x0000, 0xA1)), 0x04, "00_1_000")
    testArithAccum(List((Regs.A, 0xF0), (Regs.D, 0xA2)), List((0x0000, 0xA2)), 0xA0, "10_1_100")
    testArithAccum(List((Regs.A, 0xF0), (Regs.E, 0xA2)), List((0x0000, 0xA3)), 0xA0, "10_1_100")
    testArithAccum(List((Regs.A, 0xF0), (Regs.H, 0xA2)), List((0x0000, 0xA4)), 0xA0, "10_1_100")
    testArithAccum(List((Regs.A, 0xF0), (Regs.L, 0xA2)), List((0x0000, 0xA5)), 0xA0, "10_1_100")
    testArithAccum(List((Regs.A, 0xF0), (Regs.HL, 0x1010)), List((0x0000, 0xA6), (0x1010, 0xA2)), 0xA0, "10_1_100")
    testArithAccum(List((Regs.A, 0xF0), (Regs.IX, 0x2020)), List((0x0000, 0xDD), (0x0001, 0xA6), (0x0002, 0x10), (0x2030, 0xA2)), 0xA0, "10_1_100", 3)
    testArithAccum(List((Regs.A, 0xF0), (Regs.IY, 0x2020)), List((0x0000, 0xFD), (0x0001, 0xA6), (0x0002, 0xF0), (0x2010, 0xA2)), 0xA0, "10_1_100", 3)
    testArithAccum(List((Regs.A, 0xF0)), List((0x0000, 0xE6), (0x0001, 0xA2)), 0xA0, "10_1_100", 2)
  }

  test("run XOR r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List((Regs.A, 0x11)), List((0x0000, 0xAF)), 0x00, "01_0_100")
    testArithAccum(List((Regs.A, 0xEF), (Regs.B, 0xFE)), List((0x0000, 0xA8)), 0x11, "00_0_100")
    testArithAccum(List((Regs.A, 0xEF), (Regs.C, 0xFF)), List((0x0000, 0xA9)), 0x10, "00_0_000")
    testArithAccum(List((Regs.A, 0xFF), (Regs.D, 0x00)), List((0x0000, 0xAA)), 0xFF, "10_0_100")
    testArithAccum(List((Regs.A, 0xFC), (Regs.E, 0xFF)), List((0x0000, 0xAB)), 0x03, "00_0_100")
    testArithAccum(List((Regs.A, 0xF0), (Regs.H, 0xCE)), List((0x0000, 0xAC)), 0x3E, "00_0_000")
    testArithAccum(List((Regs.A, 0xF0), (Regs.L, 0xCE)), List((0x0000, 0xAD)), 0x3E, "00_0_000")
    testArithAccum(List((Regs.A, 0xF0), (Regs.HL, 0x1011)), List((0x0000, 0xAE), (0x1011, 0xCE)), 0x3E, "00_0_000")
    testArithAccum(List((Regs.A, 0xF0), (Regs.IX, 0x2120)), List((0x0000, 0xDD), (0x0001, 0xAE), (0x0002, 0x10), (0x2130, 0xCE)), 0x3E, "00_0_000", 3)
    testArithAccum(List((Regs.A, 0xF0), (Regs.IY, 0x2120)), List((0x0000, 0xFD), (0x0001, 0xAE), (0x0002, 0xF0), (0x2110, 0xCE)), 0x3E, "00_0_000", 3)
    testArithAccum(List((Regs.A, 0xF0)), List((0x0000, 0xEE), (0x0001, 0xCE)), 0x3E, "00_0_000", 2)
  }

  test("run OR r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List((Regs.A, 0x00)), List((0x0000, 0xB7)), 0x00, "01_0_100")
    testArithAccum(List((Regs.A, 0xAB)), List((0x0000, 0xB7)), 0xAB, "10_0_000")
    testArithAccum(List((Regs.A, 0x30), (Regs.B, 0x02)), List((0x0000, 0xB0)), 0x32, "00_0_000")
    testArithAccum(List((Regs.A, 0x40), (Regs.C, 0x03)), List((0x0000, 0xB1)), 0x43, "00_0_000")
    testArithAccum(List((Regs.A, 0x50), (Regs.D, 0x04)), List((0x0000, 0xB2)), 0x54, "00_0_000")
    testArithAccum(List((Regs.A, 0x60), (Regs.E, 0x05)), List((0x0000, 0xB3)), 0x65, "00_0_100")
    testArithAccum(List((Regs.A, 0x70), (Regs.H, 0x06)), List((0x0000, 0xB4)), 0x76, "00_0_000")
    testArithAccum(List((Regs.A, 0x80), (Regs.L, 0x07)), List((0x0000, 0xB5)), 0x87, "10_0_100")
    testArithAccum(List((Regs.A, 0x80), (Regs.HL, 0x1010)), List((0x0000, 0xB6), (0x1010, 0x07)), 0x87, "10_0_100")
    testArithAccum(List((Regs.A, 0x80), (Regs.IX, 0x1020)), List((0x0000, 0xDD), (0x0001, 0xB6), (0x0002, 0x10), (0x1030, 0x07)), 0x87, "10_0_100", 3)
    testArithAccum(List((Regs.A, 0x80), (Regs.IY, 0x1020)), List((0x0000, 0xFD), (0x0001, 0xB6), (0x0002, 0xF0), (0x1010, 0x07)), 0x87, "10_0_100", 3)
    testArithAccum(List((Regs.A, 0x80)), List((0x0000, 0xF6), (0x0001, 0x07)), 0x87, "10_0_100", 2)
  }

  test("run CP r/(HL)/(IX+d)/(IY+d)/n") {
    // based on "real" Z80 emulator
    testArithAccum(List((Regs.A, 0x1D), (Regs.B, 0x3E)), List((0x0000, 0xB8)), 0x1D, "10_1_011")
    testArithAccum(List((Regs.A, 0xDF), (Regs.IX, 0x0302)), List((0x0000, 0xDD), (0x0001, 0xBE), (0x0002, 0x01), (0x0303, 0x3E)), 0xDF, "10_0_010", 3)
    testArithAccum(List((Regs.A, 0xA1), (Regs.IY, 0x0405)), List((0x0000, 0xFD), (0x0001, 0xBE), (0x0002, 0xFD), (0x0402, 0x3E)), 0xA1, "00_1_110", 3)
    testArithAccum(List((Regs.A, 0x63), (Regs.HL, 0x0503)), List((0x0000, 0xBE), (0x0503, 0x3E)), 0x63, "00_1_010")

    testArithAccum(List((Regs.A, 0x00)), List((0x0000, 0xFE), (0x0001, 0x40)), 0x00, "10_0_011", 2)
    testArithAccum(List((Regs.A, 0xC0), (Regs.E, 0x40)), List((0x0000, 0xBB)), 0xC0, "10_0_010")
    testArithAccum(List((Regs.A, 0x80), (Regs.B, 0x40)), List((0x0000, 0xB8)), 0x80, "00_0_110")
    testArithAccum(List((Regs.A, 0x40), (Regs.C, 0x40)), List((0x0000, 0xB9)), 0x40, "01_0_010")

    testArithAccum(List((Regs.A, 0xE0), (Regs.D, 0xF0)), List((0x0000, 0xBA)), 0xE0, "10_0_011")
    testArithAccum(List((Regs.A, 0xF0)), List((0x0000, 0xBF)), 0xF0, "01_0_010")
    testArithAccum(List((Regs.A, 0x00), (Regs.H, 0xF0)), List((0x0000, 0xBC)), 0x00, "00_0_011")
    testArithAccum(List((Regs.A, 0x10), (Regs.L, 0xF0)), List((0x0000, 0xBD)), 0x10, "00_0_011")

    testArithAccum(List((Regs.A, 0x70), (Regs.B, 0xF0)), List((0x0000, 0xB8)), 0x70, "10_0_111")
  }

  test("run INC r/(HL)/(IX+d)/(IY+d)") {
    // based on "real" Z80 emulator
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.A, 0x00)), List((0x0000, 0x3C)), Regs.A,0, 0x01, "00_0_000")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.B, 0x01)), List((0x0000, 0x04)), Regs.B,0, 0x02, "00_0_000")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.C, 0x3F)), List((0x0000, 0x0C)), Regs.C,0, 0x40, "00_1_000")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.D, 0xFE)), List((0x0000, 0x14)), Regs.D,0, 0xFF, "10_0_000")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x01), (Regs.E, 0xFE)), List((0x0000, 0x1C)), Regs.E,0, 0xFF, "10_0_001")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.H, 0xFF)), List((0x0000, 0x24)), Regs.H,0, 0x00, "01_1_000")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.L, 0x7F)), List((0x0000, 0x2C)), Regs.L,0, 0x80, "10_1_100")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.HL, 0x0102)), List((0x0000, 0x34),(0x0102,0x7F)), Regs.NONE,0x0102, 0x80, "10_1_100")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.IX, 0x0201)), List((0x0000, 0xDD),(0x0001, 0x34),(0x0002, 0x02),(0x0203,0x7F)),
      Regs.NONE,0x0203, 0x80, "10_1_100",3)
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.IY, 0x0301)), List((0x0000, 0xFD),(0x0001, 0x34),(0x0002, 0xFF),(0x0300,0x7F)),
      Regs.NONE,0x0300, 0x80, "10_1_100",3)
  }

  test("run DEC r/(HL)/(IX+d)/(IY+d)") {
    // based on "real" Z80 emulator
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0x3D)), Regs.A,0, 0x00, "01_0_010")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.B, 0x02)), List((0x0000, 0x05)), Regs.B,0, 0x01, "00_0_010")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.C, 0x40)), List((0x0000, 0x0D)), Regs.C,0, 0x3F, "00_1_010")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.D, 0xFF)), List((0x0000, 0x15)), Regs.D,0, 0xFE, "10_0_010")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x01), (Regs.E, 0xFF)), List((0x0000, 0x1D)), Regs.E,0, 0xFE, "10_0_011")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.H, 0x00)), List((0x0000, 0x25)), Regs.H,0, 0xFF, "10_1_010")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.L, 0x80)), List((0x0000, 0x2D)), Regs.L,0, 0x7F, "00_1_110")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.HL, 0x0102)), List((0x0000, 0x35),(0x0102,0x80)), Regs.NONE,0x0102, 0x7F, "00_1_110")
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.IX, 0x0201)), List((0x0000, 0xDD),(0x0001, 0x35),(0x0002, 0x02),(0x0203,0x80)),
      Regs.NONE,0x0203, 0x7F, "00_1_110",3)
    TestUtils.testRegOrAddrWithFlagsInt(List((Regs.F, 0x00), (Regs.IY, 0x0301)), List((0x0000, 0xFD),(0x0001, 0x35),(0x0002, 0xFF),(0x0300,0x80)),
      Regs.NONE,0x0300, 0x7F, "00_1_110",3)
  }

  test("run CPL") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0x2F)), Regs.A,AnyInt, IntValue(0xFE), "00_1_010")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x01)), List((0x0000, 0x2F)), Regs.A,AnyInt, IntValue(0xFE), "11_1_111")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x55)), List((0x0000, 0x2F)), Regs.A,AnyInt, IntValue(0xAA), "00_1_010")
  }

  test("run NEG") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0xED),(0x0001, 0x44)), Regs.A,AnyInt, IntValue(0xFF), "10_1_011",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x00)), List((0x0000, 0xED),(0x0001, 0x44)), Regs.A,AnyInt, IntValue(0x00), "01_0_010",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0xFF)), List((0x0000, 0xED),(0x0001, 0x44)), Regs.A,AnyInt, IntValue(0x01), "00_1_011",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x7F)), List((0x0000, 0xED),(0x0001, 0x44)), Regs.A,AnyInt, IntValue(0x81), "10_1_011",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x80)), List((0x0000, 0xED),(0x0001, 0x44)), Regs.A,AnyInt, IntValue(0x80), "10_0_111",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x40)), List((0x0000, 0xED),(0x0001, 0x44)), Regs.A,AnyInt, IntValue(0xC0), "10_0_011",2)
  }

  test("run CCF") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x00)), List((0x0000, 0x3F)), Regs.NONE,AnyInt, AnyInt, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x01), (Regs.A, 0x00)), List((0x0000, 0x3F)), Regs.NONE,AnyInt, AnyInt, "00_1_000")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x00)), List((0x0000, 0x3F)), Regs.NONE,AnyInt, AnyInt, "11_1_100")
  }

  test("run SCF") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x00)), List((0x0000, 0x37)), Regs.NONE,AnyInt, AnyInt, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x01), (Regs.A, 0x00)), List((0x0000, 0x37)), Regs.NONE,AnyInt, AnyInt, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x00)), List((0x0000, 0x37)), Regs.NONE,AnyInt, AnyInt, "11_0_101")
  }

}