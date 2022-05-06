package org.kr.scala.z80.test

import org.kr.scala.z80.system.{InputController, InputPortConstant, InputPortSequential}
import org.scalatest.funsuite.AnyFunSuite

class InputFileTest extends AnyFunSuite{
  test("input from blank input file") {
    //given
    val inC=InputController.blank
    //when
    //then
    assert(inC.get.read(0xFF)==0)
    assert(inC.get.read(0x00)==0)
  }

  test("input from constant input file") {
    //given
    val inC=InputController.blank >>= InputController.attachPort(0x10,new InputPortConstant(0xAA))
    //when
    val inCAfter=inC >>= InputController.refreshPort(0x10)
    //then
    assert(inC.get.read(0x10)==0xAA)
    assert(inCAfter.get.read(0x10)==0xAA)
  }

  test("input from sequential input file") {
    //given
    val inC=InputController.blank >>= InputController.attachPort(0x33,new InputPortSequential(0xBB,3,0,0x11))
    //when
    val inCAfter1=inC >>= InputController.refreshPort(0x33)
    val inCAfter2=inCAfter1 >>= InputController.refreshPort(0x33)
    val inCAfter3=inCAfter2 >>= InputController.refreshPort(0x33)
    //then
    assert(inC.get.read(0x33)==0xBB)
    assert(inCAfter1.get.read(0x33)==0x11)
    assert(inCAfter2.get.read(0x33)==0x11)
    assert(inCAfter3.get.read(0x33)==0xBB)
  }
}
