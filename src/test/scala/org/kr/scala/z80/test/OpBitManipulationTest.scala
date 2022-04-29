package org.kr.scala.z80.test

import org.kr.scala.z80.opcode.OpCode
import org.kr.scala.z80.system.{Debugger, DummyDebugger}
import org.scalatest.funsuite.AnyFunSuite

class OpBitManipulationTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  test ("run BIT b, r/(HL)/(IX/IY+d)") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x55)), List((0x0000, 0xCB),(0x0001, 0x47)), "", OpCode.ANY, 0x00, "00_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x55)), List((0x0000, 0xCB),(0x0001, 0x47)), "", OpCode.ANY, 0x00, "10_1_101",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x55)), List((0x0000, 0xCB),(0x0001, 0x48)), "", OpCode.ANY, 0x00, "01_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("C", 0x55)), List((0x0000, 0xCB),(0x0001, 0x51)), "", OpCode.ANY, 0x00, "00_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("D", 0x55)), List((0x0000, 0xCB),(0x0001, 0x5A)), "", OpCode.ANY, 0x00, "01_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("E", 0x55)), List((0x0000, 0xCB),(0x0001, 0x63)), "", OpCode.ANY, 0x00, "00_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("H", 0x55)), List((0x0000, 0xCB),(0x0001, 0x6C)), "", OpCode.ANY, 0x00, "01_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("L", 0xAA)), List((0x0000, 0xCB),(0x0001, 0x75)), "", OpCode.ANY, 0x00, "01_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x1010)), List((0x0000, 0xCB),(0x0001, 0x7E),(0x1010, 0xAA)),
      "", OpCode.ANY, 0x00, "00_1_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x1010)), List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0x7E),(0x1011,0xAA)),
      "", OpCode.ANY, 0x00, "00_1_000",4)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IY", 0x1012)), List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFF),(0x0003,0x7E),(0x1011,0xAA)),
      "", OpCode.ANY, 0x00, "00_1_000",4)
  }

  test ("run RES b, r/(HL)/(IX/IY+d)") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0xFF)), List((0x0000, 0xCB),(0x0001, 0x87)), "A", OpCode.ANY, 0xFE, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0xFD)), List((0x0000, 0xCB),(0x0001, 0x88)), "B", OpCode.ANY, 0xFD, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("C", 0x04)), List((0x0000, 0xCB),(0x0001, 0x91)), "C", OpCode.ANY, 0x00, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("D", 0x08)), List((0x0000, 0xCB),(0x0001, 0x9A)), "D", OpCode.ANY, 0x00, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("E", 0x1F)), List((0x0000, 0xCB),(0x0001, 0xA3)), "E", OpCode.ANY, 0x0F, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("H", 0xFF)), List((0x0000, 0xCB),(0x0001, 0xAC)), "H", OpCode.ANY, 0xDF, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("L", 0xFF)), List((0x0000, 0xCB),(0x0001, 0xB5)), "L", OpCode.ANY, 0xBF, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x1020)), List((0x0000, 0xCB),(0x0001, 0xBE),(0x1020, 0xFF)),
      "", 0x1020, 0x7F, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x2010)), List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0xB6),(0x2011,0xFF)),
      "", 0x2011, 0xBF, "00_0_000",4)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("IY", 0x2012)), List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFE),(0x0003,0xAE),(0x2010,0xFF)),
      "", 0x2010, 0xDF, "11_1_111",4)
  }

  test ("run SET b, r/(HL)/(IX/IY+d)") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0xFE)), List((0x0000, 0xCB),(0x0001, 0xC7)), "A", OpCode.ANY, 0xFF, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x10)), List((0x0000, 0xCB),(0x0001, 0xC8)), "B", OpCode.ANY, 0x12, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("C", 0x00)), List((0x0000, 0xCB),(0x0001, 0xD1)), "C", OpCode.ANY, 0x04, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("D", 0xF0)), List((0x0000, 0xCB),(0x0001, 0xDA)), "D", OpCode.ANY, 0xF8, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("E", 0x0F)), List((0x0000, 0xCB),(0x0001, 0xE3)), "E", OpCode.ANY, 0x1F, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xDF), ("H", 0xFF)), List((0x0000, 0xCB),(0x0001, 0xEC)), "H", OpCode.ANY, 0xFF, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("L", 0xBF)), List((0x0000, 0xCB),(0x0001, 0xF5)), "L", OpCode.ANY, 0xFF, "11_1_111",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("HL", 0x1020)), List((0x0000, 0xCB),(0x0001, 0xFE),(0x1020, 0x7F)),
      "", 0x1020, 0xFF, "00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("IX", 0x2010)), List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0xF6),(0x2011,0xBF)),
      "", 0x2011, 0xFF, "00_0_000",4)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("IY", 0x2012)), List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFE),(0x0003,0xEE),(0x2010,0xDF)),
      "", 0x2010, 0xFF, "11_1_111",4)
  }
}
