package org.kr.scala.z80.test

import org.kr.scala.z80.opcode.OpCode
import org.scalatest.funsuite.AnyFunSuite

class OpJumpCallReturnTest extends AnyFunSuite {
  test("run JP nn/cc,nn") {
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0xC3),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF)),List((0,0xC3),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"11_1_111",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0xDA),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0xD2),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x01)),List((0,0xDA),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_001",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x01)),List((0,0xD2),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_001",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0xCA),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0xC2),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x40)),List((0,0xCA),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"01_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x40)),List((0,0xC2),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"01_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0xEA),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0xE2),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x04)),List((0,0xEA),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_100",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x04)),List((0,0xE2),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_100",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0xFA),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0xF2),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x80)),List((0,0xFA),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"10_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x80)),List((0,0xF2),(1,0x20),(2,0x10)),"",OpCode.ANY,OpCode.ANY,"10_0_000",3)
  }

  test("run JP (HL/IX/IY") {
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("HL",0x1234)),List((0,0xE9)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("HL",0x1234)),List((0,0xE9)),"",OpCode.ANY,OpCode.ANY,"11_1_111",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IX",0x1234)),List((0,0xDD),(1,0xE9)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("IX",0x1234)),List((0,0xDD),(1,0xE9)),"",OpCode.ANY,OpCode.ANY,"11_1_111",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IY",0x1234)),List((0,0xFD),(1,0xE9)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("IY",0x1234)),List((0,0xFD),(1,0xE9)),"",OpCode.ANY,OpCode.ANY,"11_1_111",0x1234)
  }

  test("run JR f,e") {
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0x18),(1,0x03)),"",OpCode.ANY,OpCode.ANY,"00_0_000",5)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF)),List((0,0x18),(1,0x03)),"",OpCode.ANY,OpCode.ANY,"11_1_111",5)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0x38),(1,0xFD)),"",OpCode.ANY,OpCode.ANY,"00_0_000",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0x30),(1,0x80)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0xFF82)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0x30),(1,0x7F)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x0081)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0,0x30),(1,0xFD)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0xFFFF)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x01)),List((0,0x38),(1,0xFD)),"",OpCode.ANY,OpCode.ANY,"00_0_001",0xFFFF)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x01)),List((0,0x30),(1,0xFD)),"",OpCode.ANY,OpCode.ANY,"00_0_001",2)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("PC",0x1000)),List((0x1000,0x28),(0x1001,0xFD)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1002)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("PC",0x1000)),List((0x1000,0x20),(0x1001,0xFD)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x0FFF)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("PC",0x1000)),List((0x1000,0x20),(0x1001,0x80)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x0F82)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("PC",0x1000)),List((0x1000,0x20),(0x1001,0x00)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1002)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("PC",0x1000)),List((0x1000,0x20),(0x1001,0x01)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1003)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("PC",0x1000)),List((0x1000,0x20),(0x1001,0x7F)),"",OpCode.ANY,OpCode.ANY,"00_0_000",0x1081)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x40),("PC",0x1000)),List((0x1000,0x28),(0x1001,0xFD)),"",OpCode.ANY,OpCode.ANY,"01_0_000",0x0FFF)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x40),("PC",0x1000)),List((0x1000,0x20),(0x1001,0xFD)),"",OpCode.ANY,OpCode.ANY,"01_0_000",0x1002)
  }

  test("run CALL nn/cc,nn") {
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xCD), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "00_0_000", 0x1020)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0xFF), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xCD), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "11_1_111", 0x1020)

    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xDC), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", OpCode.ANY, 0x1000, "00_0_000", 0x0105)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xD4), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "00_0_000", 0x1020)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x01), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xDC), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "00_0_001", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xD4), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", OpCode.ANY, 0x1000, "00_0_001", 0x0105)

    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xCC), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", OpCode.ANY, 0x1000, "00_0_000", 0x0105)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xC4), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "00_0_000", 0x1020)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x40), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xCC), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "01_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x40), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xC4), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", OpCode.ANY, 0x1000, "01_0_000", 0x0105)

    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xEC), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", OpCode.ANY, 0x1000, "00_0_000", 0x0105)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xE4), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "00_0_000", 0x1020)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x04), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xEC), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "00_0_100", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x04), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xE4), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", OpCode.ANY, 0x1000, "00_0_100", 0x0105)

    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xFC), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", OpCode.ANY, 0x1000, "00_0_000", 0x0105)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xF4), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "00_0_000", 0x1020)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x80), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xFC), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", 0x0FFE, 0x0FFE, 0x0105, "10_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x80), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xF4), (0x0103, 0x20), (0x0104, 0x10)),
      "SP", OpCode.ANY, 0x1000, "10_0_000", 0x0105)
  }

  test("run RET cc") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xC9), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "00_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("SP", 0x0100)), List((0x0000, 0xC9), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "11_1_111", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xD8), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0100, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xD0), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "00_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("SP", 0x0100)), List((0x0000, 0xD8), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "00_0_001", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x01), ("SP", 0x0100)), List((0x0000, 0xD0), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0100, "00_0_001")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xC8), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0100, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xC0), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "00_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x40), ("SP", 0x0100)), List((0x0000, 0xC8), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "01_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x40), ("SP", 0x0100)), List((0x0000, 0xC0), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0100, "01_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xE8), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0100, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xE0), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "00_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x04), ("SP", 0x0100)), List((0x0000, 0xE8), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "00_0_100", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x04), ("SP", 0x0100)), List((0x0000, 0xE0), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0100, "00_0_100")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xF8), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0100, "00_0_000")
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xF0), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "00_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x80), ("SP", 0x0100)), List((0x0000, 0xF8), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "10_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x80), ("SP", 0x0100)), List((0x0000, 0xF0), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0100, "10_0_000")

  }

  test("run RETI") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("SP", 0x0100)), List((0x0000, 0xED),(0x0001, 0x4D), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "00_0_000", 0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0xFF), ("SP", 0x0100)), List((0x0000, 0xED),(0x0001, 0x4D), (0x0100, 0x20), (0x0101, 0x10)),
      "SP", OpCode.ANY, 0x0102, "11_1_111", 0x1020)
  }

  test("run RST") {
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xC7)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "00_0_000", 0x0000)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0xFF), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xC7)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "11_1_111", 0x0000)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xCF)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "00_0_000", 0x0008)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0xFF), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xCF)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "11_1_111", 0x0008)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xD7)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "00_0_000", 0x0010)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0xFF), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xD7)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "11_1_111", 0x0010)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xDF)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "00_0_000", 0x0018)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0xFF), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xDF)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "11_1_111", 0x0018)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xE7)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "00_0_000", 0x0020)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0xFF), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xE7)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "11_1_111", 0x0020)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xEF)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "00_0_000", 0x0028)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0xFF), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xEF)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "11_1_111", 0x0028)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xF7)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "00_0_000", 0x0030)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0xFF), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xF7)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "11_1_111", 0x0030)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0x00), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xFF)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "00_0_000", 0x0038)
    TestUtils.testRegAndAddrWordWithFlags(List(("F", 0xFF), ("SP", 0x1000), ("PC", 0x0102)), List((0x0102, 0xFF)),
      "SP", 0x0FFE, 0x0FFE, 0x0103, "11_1_111", 0x0038)
  }

  test("run DJNZ") {
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x01), ("PC", 0x0100)), List((0x0100, 0x10), (0x0101, 0xFD)),
      "B", OpCode.ANY, 0x00, "00_0_000", 0x0102)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x02), ("PC", 0x0100)), List((0x0100, 0x10), (0x0101, 0xFD)),
      "B", OpCode.ANY, 0x01, "00_0_000", 0x00FF)
    TestUtils.testRegOrAddrWithFlags(List(("F", 0x00), ("B", 0x00), ("PC", 0x0100)), List((0x0100, 0x10), (0x0101, 0xFD)),
      "B", OpCode.ANY, 0xFF, "00_0_000", 0x00FF)
  }
}
