package org.kr.scala.z80.test

import org.kr.scala.z80.opcode.OpCode
import org.kr.scala.z80.system.{ConsoleDebugger, Debugger, DummyDebugger, Regs}
import org.scalatest.funsuite.AnyFunSuite

class OpBitManipulationTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  test ("run BIT b, r/(HL)/(IX/IY+d)") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0x55)), List((0x0000, 0xCB),(0x0001, 0x47)), Regs.NONE, OpCode.ANY, 0x00, "00_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.A, 0x55)), List((0x0000, 0xCB),(0x0001, 0x47)), Regs.NONE, OpCode.ANY, 0x00, "10_1_101",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.B, 0x55)), List((0x0000, 0xCB),(0x0001, 0x48)), Regs.NONE, OpCode.ANY, 0x00, "01_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.C, 0x55)), List((0x0000, 0xCB),(0x0001, 0x51)), Regs.NONE, OpCode.ANY, 0x00, "00_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.D, 0x55)), List((0x0000, 0xCB),(0x0001, 0x5A)), Regs.NONE, OpCode.ANY, 0x00, "01_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.E, 0x55)), List((0x0000, 0xCB),(0x0001, 0x63)), Regs.NONE, OpCode.ANY, 0x00, "00_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.H, 0x55)), List((0x0000, 0xCB),(0x0001, 0x6C)), Regs.NONE, OpCode.ANY, 0x00, "01_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.L, 0xAA)), List((0x0000, 0xCB),(0x0001, 0x75)), Regs.NONE, OpCode.ANY, 0x00, "01_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x1010)), List((0x0000, 0xCB),(0x0001, 0x7E),(0x1010, 0xAA)),
      Regs.NONE, OpCode.ANY, 0x00, "00_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x1010)), List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0x7E),(0x1011,0xAA)),
      Regs.NONE, OpCode.ANY, 0x00, "00_1_000",4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IY, 0x1012)), List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFF),(0x0003,0x7E),(0x1011,0xAA)),
      Regs.NONE, OpCode.ANY, 0x00, "00_1_000",4)
  }

  test ("run RES b, r/(HL)/(IX/IY+d)") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0xFF)), List((0x0000, 0xCB),(0x0001, 0x87)), Regs.A, OpCode.ANY, 0xFE, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.B, 0xFD)), List((0x0000, 0xCB),(0x0001, 0x88)), Regs.B, OpCode.ANY, 0xFD, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.C, 0x04)), List((0x0000, 0xCB),(0x0001, 0x91)), Regs.C, OpCode.ANY, 0x00, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.D, 0x08)), List((0x0000, 0xCB),(0x0001, 0x9A)), Regs.D, OpCode.ANY, 0x00, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.E, 0x1F)), List((0x0000, 0xCB),(0x0001, 0xA3)), Regs.E, OpCode.ANY, 0x0F, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.H, 0xFF)), List((0x0000, 0xCB),(0x0001, 0xAC)), Regs.H, OpCode.ANY, 0xDF, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.L, 0xFF)), List((0x0000, 0xCB),(0x0001, 0xB5)), Regs.L, OpCode.ANY, 0xBF, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x1020)), List((0x0000, 0xCB),(0x0001, 0xBE),(0x1020, 0xFF)),
      Regs.NONE, 0x1020, 0x7F, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x2010)), List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0xB6),(0x2011,0xFF)),
      Regs.NONE, 0x2011, 0xBF, "00_0_000",4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.IY, 0x2012)), List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFE),(0x0003,0xAE),(0x2010,0xFF)),
      Regs.NONE, 0x2010, 0xDF, "11_1_111",4)
  }

  test ("run SET b, r/(HL)/(IX/IY+d)") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.A, 0xFE)), List((0x0000, 0xCB),(0x0001, 0xC7)), Regs.A, OpCode.ANY, 0xFF, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.B, 0x10)), List((0x0000, 0xCB),(0x0001, 0xC8)), Regs.B, OpCode.ANY, 0x12, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.C, 0x00)), List((0x0000, 0xCB),(0x0001, 0xD1)), Regs.C, OpCode.ANY, 0x04, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.D, 0xF0)), List((0x0000, 0xCB),(0x0001, 0xDA)), Regs.D, OpCode.ANY, 0xF8, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.E, 0x0F)), List((0x0000, 0xCB),(0x0001, 0xE3)), Regs.E, OpCode.ANY, 0x1F, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xDF), (Regs.H, 0xFF)), List((0x0000, 0xCB),(0x0001, 0xEC)), Regs.H, OpCode.ANY, 0xFF, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.L, 0xBF)), List((0x0000, 0xCB),(0x0001, 0xF5)), Regs.L, OpCode.ANY, 0xFF, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.HL, 0x1020)), List((0x0000, 0xCB),(0x0001, 0xFE),(0x1020, 0x7F)),
      Regs.NONE, 0x1020, 0xFF, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IX, 0x2010)), List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0xF6),(0x2011,0xBF)),
      Regs.NONE, 0x2011, 0xFF, "00_0_000",4)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0xFF), (Regs.IY, 0x2012)), List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFE),(0x0003,0xEE),(0x2010,0xDF)),
      Regs.NONE, 0x2010, 0xFF, "11_1_111",4)
  }
}
