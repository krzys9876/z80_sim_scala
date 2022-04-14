package org.kr.scala.z80.test

import org.kr.scala.z80.system.InputController
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
}
