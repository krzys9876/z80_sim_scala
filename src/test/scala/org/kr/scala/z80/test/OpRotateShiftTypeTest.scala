package org.kr.scala.z80.test

import org.kr.scala.z80.system.{ConsoleDebugger, Debugger, DummyDebugger, Regs}
import org.scalatest.funsuite.AnyFunSuite

class OpRotateShiftTypeTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  test("run RLCA") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0x07)), Regs.A, 0x0000, 0x02, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x01)), List((0x0000, 0x07)), Regs.A, 0x0000, 0x02, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x80)), List((0x0000, 0x07)), Regs.A, 0x0000, 0x01, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x80)), List((0x0000, 0x07)), Regs.A, 0x0000, 0x01, "11_0_101")
  }

  test("run RLC r") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0xCB), (0x0001, 0x07)), Regs.A, 0x0000, 0x02, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.B, 0x55)), List((0x0000, 0xCB), (0x0001, 0x00)), Regs.B, 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.C, 0x55)), List((0x0000, 0xCB), (0x0001, 0x01)), Regs.C, 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.D, 0xA0)), List((0x0000, 0xCB), (0x0001, 0x02)), Regs.D, 0x0000, 0x41, "00_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.E, 0x00)), List((0x0000, 0xCB), (0x0001, 0x03)), Regs.E, 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.H, 0x55)), List((0x0000, 0xCB), (0x0001, 0x04)), Regs.H, 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.L, 0x55)), List((0x0000, 0xCB), (0x0001, 0x05)), Regs.L, 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x0101)), List((0x0000, 0xCB), (0x0001, 0x06), (0x0101, 0x55)),
      Regs.NONE, 0x0101, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x06), (0x0202, 0x55)),
      Regs.NONE, 0x0202, 0xAA, "10_0_100", 4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IY, 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x06), (0x0200, 0x55)),
      Regs.NONE, 0x0200, 0xAA, "10_0_100", 4)
  }

  test("run RRCA") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x80)), List((0x0000, 0x0F)), Regs.A, 0x0000, 0x40, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x80)), List((0x0000, 0x0F)), Regs.A, 0x0000, 0x40, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0x0F)), Regs.A, 0x0000, 0x80, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x01)), List((0x0000, 0x0F)), Regs.A, 0x0000, 0x80, "11_0_101")
  }

  test("run RRC r") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x80)), List((0x0000, 0xCB), (0x0001, 0x0F)), Regs.A, 0x0000, 0x40, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.B, 0xAA)), List((0x0000, 0xCB), (0x0001, 0x08)), Regs.B, 0x0000, 0x55, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.C, 0xAA)), List((0x0000, 0xCB), (0x0001, 0x09)), Regs.C, 0x0000, 0x55, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.D, 0x03)), List((0x0000, 0xCB), (0x0001, 0x0A)), Regs.D, 0x0000, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.E, 0x03)), List((0x0000, 0xCB), (0x0001, 0x0B)), Regs.E, 0x0000, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.H, 0x00)), List((0x0000, 0xCB), (0x0001, 0x0C)), Regs.H, 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.L, 0x03)), List((0x0000, 0xCB), (0x0001, 0x0D)), Regs.L, 0x0000, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x0101)), List((0x0000, 0xCB), (0x0001, 0x0E), (0x0101, 0x03)),
      Regs.NONE, 0x0101, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x0E), (0x0202, 0x03)),
      Regs.NONE, 0x0202, 0x81, "10_0_101", 4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IY, 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x0E), (0x0200, 0x03)),
      Regs.NONE, 0x0200, 0x81, "10_0_101", 4)
  }

  test("run RLA") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0x17)), Regs.A, 0x0000, 0x02, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x01)), List((0x0000, 0x17)), Regs.A, 0x0000, 0x03, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x80)), List((0x0000, 0x17)), Regs.A, 0x0000, 0x00, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x80)), List((0x0000, 0x17)), Regs.A, 0x0000, 0x01, "11_0_101")
  }

  test("run RL r") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0xCB), (0x0001, 0x17)), Regs.A, 0x0000, 0x02, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.B, 0x55)), List((0x0000, 0xCB), (0x0001, 0x10)), Regs.B, 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.C, 0x55)), List((0x0000, 0xCB), (0x0001, 0x11)), Regs.C, 0x0000, 0xAB, "10_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.D, 0xA0)), List((0x0000, 0xCB), (0x0001, 0x12)), Regs.D, 0x0000, 0x40, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.E, 0x00)), List((0x0000, 0xCB), (0x0001, 0x13)), Regs.E, 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.H, 0x55)), List((0x0000, 0xCB), (0x0001, 0x14)), Regs.H, 0x0000, 0xAB, "10_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.L, 0x85)), List((0x0000, 0xCB), (0x0001, 0x15)), Regs.L, 0x0000, 0x0A, "00_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x0101)), List((0x0000, 0xCB), (0x0001, 0x16), (0x0101, 0x85)),
      Regs.NONE, 0x0101, 0x0A, "00_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x16), (0x0202, 0x85)),
      Regs.NONE, 0x0202, 0x0A, "00_0_101", 4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IY, 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x16), (0x0200, 0x85)),
      Regs.NONE, 0x0200, 0x0A, "00_0_101", 4)
  }

  test("run RRA") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x80)), List((0x0000, 0x1F)), Regs.A, 0x0000, 0x40, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x80)), List((0x0000, 0x1F)), Regs.A, 0x0000, 0xC0, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0x1F)), Regs.A, 0x0000, 0x00, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x01)), List((0x0000, 0x1F)), Regs.A, 0x0000, 0x80, "11_0_101")
  }

  test("run RR r") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x80)), List((0x0000, 0xCB), (0x0001, 0x1F)), Regs.A, 0x0000, 0xC0, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.B, 0x01)), List((0x0000, 0xCB), (0x0001, 0x18)), Regs.B, 0x0000, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.C, 0xAA)), List((0x0000, 0xCB), (0x0001, 0x19)), Regs.C, 0x0000, 0x55, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.D, 0xAA)), List((0x0000, 0xCB), (0x0001, 0x1A)), Regs.D, 0x0000, 0xD5, "10_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.E, 0x03)), List((0x0000, 0xCB), (0x0001, 0x1B)), Regs.E, 0x0000, 0x01, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x01), (Regs.H, 0x03)), List((0x0000, 0xCB), (0x0001, 0x1C)), Regs.H, 0x0000, 0x81, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x01), (Regs.L, 0x02)), List((0x0000, 0xCB), (0x0001, 0x1D)), Regs.L, 0x0000, 0x81, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x0101)), List((0x0000, 0xCB), (0x0001, 0x1E), (0x0101, 0x03)),
      Regs.NONE, 0x0101, 0x01, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x1E), (0x0202, 0x03)),
      Regs.NONE, 0x0202, 0x01, "00_0_001", 4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x01), (Regs.IY, 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x1E), (0x0200, 0x03)),
      Regs.NONE, 0x0200, 0x81, "10_0_101", 4)
  }

  test("run SLA r") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01)), List((0x0000, 0xCB), (0x0001, 0x27)), Regs.A, 0x0000, 0x02, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.B, 0x01)), List((0x0000, 0xCB), (0x0001, 0x20)), Regs.B, 0x0000, 0x02, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.C, 0x55)), List((0x0000, 0xCB), (0x0001, 0x21)), Regs.C, 0x0000, 0xAA, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.D, 0xAA)), List((0x0000, 0xCB), (0x0001, 0x22)), Regs.D, 0x0000, 0x54, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.E, 0x00)), List((0x0000, 0xCB), (0x0001, 0x23)), Regs.E, 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.H, 0x80)), List((0x0000, 0xCB), (0x0001, 0x24)), Regs.H, 0x0000, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.L, 0x80)), List((0x0000, 0xCB), (0x0001, 0x25)), Regs.L, 0x0000, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x0101)), List((0x0000, 0xCB), (0x0001, 0x26), (0x0101, 0x80)),
      Regs.NONE, 0x0101, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x26), (0x0202, 0x80)),
      Regs.NONE, 0x0202, 0x00, "01_0_101", 4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IY, 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x26), (0x0200, 0x80)),
      Regs.NONE, 0x0200, 0x00, "01_0_101", 4)
  }

  test("run SRA r") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x02)), List((0x0000, 0xCB), (0x0001, 0x2F)), Regs.A, 0x0000, 0x01, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.B, 0x03)), List((0x0000, 0xCB), (0x0001, 0x28)), Regs.B, 0x0000, 0x01, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.C, 0x01)), List((0x0000, 0xCB), (0x0001, 0x29)), Regs.C, 0x0000, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.D, 0x80)), List((0x0000, 0xCB), (0x0001, 0x2A)), Regs.D, 0x0000, 0xC0, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.E, 0x80)), List((0x0000, 0xCB), (0x0001, 0x2B)), Regs.E, 0x0000, 0xC0, "10_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.H, 0x81)), List((0x0000, 0xCB), (0x0001, 0x2C)), Regs.H, 0x0000, 0xC0, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.L, 0x81)), List((0x0000, 0xCB), (0x0001, 0x2D)), Regs.L, 0x0000, 0xC0, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x0101)), List((0x0000, 0xCB), (0x0001, 0x2E), (0x0101, 0x81)),
      Regs.NONE, 0x0101, 0xC0, "10_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x2E), (0x0202, 0x81)),
      Regs.NONE, 0x0202, 0xC0, "10_0_101", 4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IY, 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x2E), (0x0200, 0x81)),
      Regs.NONE, 0x0200, 0xC0, "10_0_101", 4)
  }

  test("run SRL r") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x02)), List((0x0000, 0xCB), (0x0001, 0x3F)), Regs.A, 0x0000, 0x01, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.B, 0x03)), List((0x0000, 0xCB), (0x0001, 0x38)), Regs.B, 0x0000, 0x01, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.C, 0x01)), List((0x0000, 0xCB), (0x0001, 0x39)), Regs.C, 0x0000, 0x00, "01_0_101", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.D, 0x80)), List((0x0000, 0xCB), (0x0001, 0x3A)), Regs.D, 0x0000, 0x40, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.E, 0x80)), List((0x0000, 0xCB), (0x0001, 0x3B)), Regs.E, 0x0000, 0x40, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.H, 0x81)), List((0x0000, 0xCB), (0x0001, 0x3C)), Regs.H, 0x0000, 0x40, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.L, 0x81)), List((0x0000, 0xCB), (0x0001, 0x3D)), Regs.L, 0x0000, 0x40, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x0101)), List((0x0000, 0xCB), (0x0001, 0x3E), (0x0101, 0x81)),
      Regs.NONE, 0x0101, 0x40, "00_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x0201)), List((0x0000, 0xDD), (0x0001, 0xCB), (0x0002, 0x01), (0x0003, 0x3E), (0x0202, 0x81)),
      Regs.NONE, 0x0202, 0x40, "00_0_001", 4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IY, 0x0201)), List((0x0000, 0xFD), (0x0001, 0xCB), (0x0002, 0xFF), (0x0003, 0x3E), (0x0200, 0x81)),
      Regs.NONE, 0x0200, 0x40, "00_0_001", 4)
  }

  test("run RLD") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x12),(Regs.HL,0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x34)),
      Regs.A, 0x0000, 0x13, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x12),(Regs.HL,0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x34)),
      Regs.NONE, 0x1212, 0x42, "00_0_000", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x01), (Regs.A, 0x87),(Regs.HL,0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x65)),
      Regs.A, 0x0000, 0x86, "10_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x01), (Regs.A, 0x87),(Regs.HL,0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x65)),
      Regs.NONE, 0x1212, 0x57, "10_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01),(Regs.HL,0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x02)),
      Regs.A, 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x01),(Regs.HL,0x1212)), List((0x0000, 0xED), (0x0001, 0x6F),(0x1212, 0x02)),
      Regs.NONE, 0x1212, 0x21, "01_0_100", 2)
  }

  test("run RRD") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x12), (Regs.HL, 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0x34)),
      Regs.A, 0x0000, 0x14, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x12), (Regs.HL, 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0x34)),
      Regs.NONE, 0x1313, 0x23, "00_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x01), (Regs.A, 0xAB), (Regs.HL, 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0xCD)),
      Regs.A, 0x0000, 0xAD, "10_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x01), (Regs.A, 0xAB), (Regs.HL, 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0xCD)),
      Regs.NONE, 0x1313, 0xBC, "10_0_001", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x02), (Regs.HL, 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0x30)),
      Regs.A, 0x0000, 0x00, "01_0_100", 2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x02), (Regs.HL, 0x1313)), List((0x0000, 0xED), (0x0001, 0x67), (0x1313, 0x30)),
      Regs.NONE, 0x1313, 0x23, "01_0_100", 2)
  }
}