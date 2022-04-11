package org.kr.scala.z80.test

import org.kr.scala.z80.opcode.OpCode
import org.scalatest.funsuite.AnyFunSuite

class OpJumpCallReturnTest extends AnyFunSuite{
  test("run JP nn/cc,nn") {
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0x0000,0xC3),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF)),List((0x0000,0xC3),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"11_1_111",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0x0000,0xDA),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0x0000,0xD2),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x01)),List((0x0000,0xDA),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_001",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x01)),List((0x0000,0xD2),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_001",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0x0000,0xCA),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0x0000,0xC2),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x40)),List((0x0000,0xCA),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"01_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x40)),List((0x0000,0xC2),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"01_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0x0000,0xEA),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0x0000,0xE2),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x04)),List((0x0000,0xEA),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_100",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x04)),List((0x0000,0xE2),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_100",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0x0000,0xFA),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_000",3)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00)),List((0x0000,0xF2),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"00_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x80)),List((0x0000,0xFA),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"10_0_000",0x1020)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x80)),List((0x0000,0xF2),(0x0001,0x20),(0x0002, 0x10)),"", OpCode.ANY,OpCode.ANY,"10_0_000",3)
  }

  test("run JP (HL/IX/IY") {
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("HL",0x1234)),List((0x0000,0xE9)),"", OpCode.ANY,OpCode.ANY,"00_0_000",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("HL",0x1234)),List((0x0000,0xE9)),"", OpCode.ANY,OpCode.ANY,"11_1_111",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IX",0x1234)),List((0x0000,0xDD),(0x0001,0xE9)),"", OpCode.ANY,OpCode.ANY,"00_0_000",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("IX",0x1234)),List((0x0000,0xDD),(0x0001,0xE9)),"", OpCode.ANY,OpCode.ANY,"11_1_111",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0x00),("IY",0x1234)),List((0x0000,0xFD),(0x0001,0xE9)),"", OpCode.ANY,OpCode.ANY,"00_0_000",0x1234)
    TestUtils.testRegOrAddrWithFlags(List(("F",0xFF),("IY",0x1234)),List((0x0000,0xFD),(0x0001,0xE9)),"", OpCode.ANY,OpCode.ANY,"11_1_111",0x1234)
  }
}
