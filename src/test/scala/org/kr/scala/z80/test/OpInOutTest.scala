package org.kr.scala.z80.test

import org.kr.scala.z80.system.{MemoryController, OutputController, RegisterController, Z80System, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class OpInOutTest extends AnyFunSuite {

  test("run OUT (n),A") {
    //given
    //when
    val systemC=TestUtils.prepareTest(List(("A",0x41)),List((0x0000,0xD3),(0x0001,0x01)))
    //then
    assert(systemC.get.registerController.get("PC")==2)
    assert(systemC.get.outputController.get(1,0)==0x41)
    assert(systemC.get.outputController.get(111,112)==0)
    //systemC.get.outputController.get.print(1)
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
    assert(systemC.get.registerController.get("PC")==0x000C)
    assert(systemC.get.outputController.get(0xFF,0)==0x41)
    assert(systemC.get.outputController.get(0xFF,1)==0x42)
    assert(systemC.get.outputController.get(0xFF,2)==0x43)
    assert(systemC.get.outputController.get(111,112)==0)

    //systemC.get.outputController.get.print(0xFF)
  }

}
