package org.kr.scala.z80.test

import org.scalatest.funsuite.AnyFunSuite

class OpRotateShiftTest extends AnyFunSuite{

  test("run RLCA") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x07)), "A", 0x0000, 0x02, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x01)), List((0x0000, 0x07)), "A", 0x0000, 0x02, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x80)), List((0x0000, 0x07)), "A", 0x0000, 0x01, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0x07)), "A", 0x0000, 0x01, "11_0_101")
  }

  test("run RLC r") {
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("A",0x01)),List((0x0000,0xCB),(0x0001,0x07)),"A",0x0000,0x02,"00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("B",0x55)),List((0x0000,0xCB),(0x0001,0x00)),"B",0x0000,0xAA,"10_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("C",0x55)),List((0x0000,0xCB),(0x0001,0x01)),"C",0x0000,0xAA,"10_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("D",0xA0)),List((0x0000,0xCB),(0x0001,0x02)),"D",0x0000,0x41,"00_0_101",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("E",0x00)),List((0x0000,0xCB),(0x0001,0x03)),"E",0x0000,0x00,"01_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("H",0x55)),List((0x0000,0xCB),(0x0001,0x04)),"H",0x0000,0xAA,"10_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("L",0x55)),List((0x0000,0xCB),(0x0001,0x05)),"L",0x0000,0xAA,"10_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("HL",0x0101)),List((0x0000,0xCB),(0x0001,0x06),(0x0101,0x55)),
      "",0x0101,0xAA,"10_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IX",0x0201)),List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0x06),(0x0202,0x55)),
      "",0x0202,0xAA,"10_0_100",4)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IY",0x0201)),List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFF),(0x0003,0x06),(0x0200,0x55)),
      "",0x0200,0xAA,"10_0_100",4)
  }

  test("run RRCA") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x80)), List((0x0000, 0x0F)), "A", 0x0000, 0x40, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0x0F)), "A", 0x0000, 0x40, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x0F)), "A", 0x0000, 0x80, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x01)), List((0x0000, 0x0F)), "A", 0x0000, 0x80, "11_0_101")
  }
  test("run RRC r") {
    /*TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("A",0x80)),List((0x0000,0xCB),(0x0001,0x0F)),"A",0x0000,0x40,"11_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("B",0x01)),List((0x0000,0xCB),(0x0001,0x08)),"B",0x0000,0x80,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("C",0x03)),List((0x0000,0xCB),(0x0001,0x09)),"C",0x0000,0x81,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("D",0x03)),List((0x0000,0xCB),(0x0001,0x0A)),"D",0x0000,0x81,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("E",0x03)),List((0x0000,0xCB),(0x0001,0x0B)),"E",0x0000,0x81,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("H",0x03)),List((0x0000,0xCB),(0x0001,0x0C)),"H",0x0000,0x81,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("L",0x03)),List((0x0000,0xCB),(0x0001,0x0D)),"L",0x0000,0x81,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("HL",0x0101)),List((0x0000,0xCB),(0x0001,0x0E),(0x0101,0x03)),
      "",0x0101,0x81,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IX",0x0201)),List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0x0E),(0x0202,0x03)),
      "",0x0202,0x81,"00_0_001",4)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IY",0x0201)),List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFF),(0x0003,0x0E),(0x0200,0x03)),
      "",0x0200,0x81,"00_0_001",4)*/
  }

  test("run RLA") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x17)), "A", 0x0000, 0x02, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x01)), List((0x0000, 0x17)), "A", 0x0000, 0x03, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x80)), List((0x0000, 0x17)), "A", 0x0000, 0x00, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0x17)), "A", 0x0000, 0x01, "11_0_101")
  }

  test("run RL r") {
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("A",0x01)),List((0x0000,0xCB),(0x0001,0x17)),"A",0x0000,0x02,"00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("B",0x55)),List((0x0000,0xCB),(0x0001,0x10)),"B",0x0000,0xAA,"10_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("C",0x55)),List((0x0000,0xCB),(0x0001,0x11)),"C",0x0000,0xAB,"10_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("D",0xA0)),List((0x0000,0xCB),(0x0001,0x12)),"D",0x0000,0x40,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("E",0x00)),List((0x0000,0xCB),(0x0001,0x13)),"E",0x0000,0x00,"01_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("H",0x55)),List((0x0000,0xCB),(0x0001,0x14)),"H",0x0000,0xAB,"10_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("L",0x85)),List((0x0000,0xCB),(0x0001,0x15)),"L",0x0000,0x0A,"00_0_101",2)
    /*TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("HL",0x0101)),List((0x0000,0xCB),(0x0001,0x16),(0x0101,0x55)),
      "",0x0101,0xAA,"10_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IX",0x0201)),List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0x16),(0x0202,0x55)),
      "",0x0202,0xAA,"10_0_100",4)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IY",0x0201)),List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFF),(0x0003,0x16),(0x0200,0x55)),
      "",0x0200,0xAA,"10_0_100",4)*/
  }

  test("run RRA") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x80)), List((0x0000, 0x1F)), "A", 0x0000, 0x40, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x80)), List((0x0000, 0x1F)), "A", 0x0000, 0xC0, "11_0_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("A", 0x01)), List((0x0000, 0x1F)), "A", 0x0000, 0x00, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("A", 0x01)), List((0x0000, 0x1F)), "A", 0x0000, 0x80, "11_0_101")
  }

  test("run RR r") {
    /*TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("A",0x80)),List((0x0000,0xCB),(0x0001,0x1F)),"A",0x0000,0xC0,"11_0_100",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("B",0x01)),List((0x0000,0xCB),(0x0001,0x18)),"B",0x0000,0x00,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("C",0x03)),List((0x0000,0xCB),(0x0001,0x19)),"C",0x0000,0x01,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("D",0x03)),List((0x0000,0xCB),(0x0001,0x1A)),"D",0x0000,0x01,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("E",0x03)),List((0x0000,0xCB),(0x0001,0x1B)),"E",0x0000,0x01,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x01),("H",0x03)),List((0x0000,0xCB),(0x0001,0x1C)),"H",0x0000,0x81,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x01),("L",0x02)),List((0x0000,0xCB),(0x0001,0x1D)),"L",0x0000,0x81,"00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("HL",0x0101)),List((0x0000,0xCB),(0x0001,0x1E),(0x0101,0x03)),
      "",0x0101,0x01,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IX",0x0201)),List((0x0000,0xDD),(0x0001,0xCB),(0x0002,0x01),(0x0003,0x1E),(0x0202,0x03)),
      "",0x0202,0x01,"00_0_001",4)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IY",0x0201)),List((0x0000,0xFD),(0x0001,0xCB),(0x0002,0xFF),(0x0003,0x1E),(0x0200,0x03)),
      "",0x0200,0x01,"00_0_001",4)
  */}

}
