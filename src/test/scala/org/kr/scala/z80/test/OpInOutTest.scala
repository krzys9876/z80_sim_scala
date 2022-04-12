package org.kr.scala.z80.test

import org.scalatest.funsuite.AnyFunSuite

class OpInOutTest extends AnyFunSuite {
  test("always pass") {
    assert (1==1)
  }

  test("run OUT") {
    //given
    //when
    //val systemC=TestUtils.prepareTest(List(("A",0x40)),List((0x0000,0xD3),(0x0001,0x01)))
    //then
    //assert(systemC.get.registerController.get("PC")==2)
    //assert(systemC.get.outputController.get(0)==0x40)
  }
}
