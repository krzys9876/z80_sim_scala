package org.kr.scala.z80.test

import org.kr.scala.z80.system.{StateWatcher, ConsoleDebugger, ConsoleDetailedDebugger, Debugger, DummyDebugger, InputPort, InputPortConstant, InputPortSequential, RegSymbol, Regs, Z80System}
import org.scalatest.funsuite.AnyFunSuite

class OpInOutTest extends AnyFunSuite {

  implicit val debugger:Debugger=ConsoleDetailedDebugger

  test("run OUT (n),A") {
    //given
    //when
    val systemC=TestUtils.prepareTest(List((Regs.A,0x41)),List((0x0000,0xD3),(0x0001,0x01)))
    //then
    assert(systemC.get.register(Regs.PC)==2)
    assert(systemC.get.output(1,0)==0x41)
    assert(systemC.get.output(111,112)==0)
    //systemC.get.output.print(1)
  }

  test("run OUT (n),A - multiple") {
    //given
    //when
    val systemC=TestUtils.prepareTest(List(),
      List(
        (0x0000,0x3E),(0x0001,0x41), // LD A,'A'
        (0x0002,0xD3),(0x0003,0xFF), // OUT (0xFF),A
        (0x0004,0x3E),(0x0005,0x42), // LD A,'B'
        (0x0006,0xD3),(0x0007,0xFF), // OUT (0xFF),A
        (0x0008,0x3E),(0x0009,0x43), // LD A,'C'
        (0x000A,0xD3),(0x000B,0xFF)  // OUT (0xFF),A
      ),6)
    //then
    assert(systemC.get.register(Regs.PC)==0x000C)
    assert(systemC.get.output(0xFF,0)==0x41)
    assert(systemC.get.output(0xFF,1)==0x42)
    assert(systemC.get.output(0xFF,2)==0x43)
    assert(systemC.get.output(111,112)==0)

    //systemC.get.output.print(0xFF)
  }

  test("run OUT (C),r - multiple") {
    //given
    //when
    val systemC=TestUtils.prepareTest(List(),
      List(
        (0x0000,0x0E),(0x0001,0x80), // LD C,0x80
        (0x0002,0x3E),(0x0003,0x41), // LD A,'A'
        (0x0004,0xED),(0x0005,0x79), // OUT (C),A
        (0x0006,0x06),(0x0007,0x42), // LD B,'B'
        (0x0008,0xED),(0x0009,0x41), // OUT (C),B
        (0x000A,0x16),(0x000B,0x43), // LD D,'C'
        (0x000C,0xED),(0x000D,0x51), // OUT (C),D
        (0x000E,0x1E),(0x000F,0x44), // LD E,'D'
        (0x0010,0xED),(0x0011,0x59), // OUT (C),E
        (0x0012,0x26),(0x0013,0x45), // LD H,'E'
        (0x0014,0xED),(0x0015,0x61), // OUT (C),H
        (0x0016,0x2E),(0x0017,0x46), // LD L,'F'
        (0x0018,0xED),(0x0019,0x69)  // OUT (C),L
      ),13)
    //then
    assert(systemC.get.register(Regs.PC)==0x001A)
    assert(systemC.get.output(0x80,0)==0x41)
    assert(systemC.get.output(0x80,1)==0x42)
    assert(systemC.get.output(0x80,2)==0x43)
    assert(systemC.get.output(0x80,3)==0x44)
    assert(systemC.get.output(0x80,4)==0x45)
    assert(systemC.get.output(0x80,5)==0x46)

    //systemC.get.output.print(0xFF)
  }

  def prepareTestWithInput(regList: List[(RegSymbol, Int)], memList: List[(Int, Int)], port:Int, inputPort:InputPort,
                           steps:Int=1): StateWatcher[Z80System] = {
    val blank=Z80System.blank.attachPort(port,inputPort)
    TestUtils.prepareTestWith(StateWatcher(blank),regList,memList,steps)
  }

  test("run IN A,(n)") {
    //given
    //when
    val systemC=prepareTestWithInput(List(),
      List(
        (0x0000,0xDB),(0x0001,0x40), // IN A,(0x40)
      ), 0x40,new InputPortConstant(0xAB))
    //then
    assert(systemC.get.register(Regs.PC)==2)
    assert(systemC.get.register(Regs.A)==0xAB)
  }

  test("run IN A,(C)") {
    //given
    //when
    val systemC=prepareTestWithInput(List((Regs.C,0x20)),
      List(
        (0x0000,0xED),(0x0001,0x50), // IN D,(C)
        (0x0002,0xED),(0x0003,0x58), // IN E,(C)
        (0x0004,0xED),(0x0005,0x60), // IN H,(C)
        (0x0006,0xED),(0x0007,0x68), // IN L,(C)
      ),0x20, new InputPortSequential(0xF0,2,0,0x0F),
      4)
    //then
    assert(systemC.get.register(Regs.PC)==8)
    assert(systemC.get.register(Regs.D)==0xF0)
    assert(systemC.get.register(Regs.E)==0x0F)
    assert(systemC.get.register(Regs.H)==0xF0)
    assert(systemC.get.register(Regs.L)==0x0F)
  }
}
