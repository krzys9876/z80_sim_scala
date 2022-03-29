package org.kr.scala.z80.test

import org.kr.scala.z80.RegisterController
import org.scalatest.funsuite.AnyFunSuite

class RegisterTest extends AnyFunSuite {
  test("always pass") {
    1==1
  }

  test("init blank register") {
    //given
    val registerController=RegisterController.blank
    //when
    //then
    assert(registerController.get.reg.equals(Map()))
    assert(registerController.get("A").equals(0))
    assert(registerController.get("PC").equals(0))
  }

  test("set register") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>= RegisterController.set("IX",255) >>= RegisterController.set("IY",56)
    //then
    assert(afterState.get.reg.equals(Map("IX"->255,"IY"->56)))
    assert(afterState.get("IX").equals(255))
    assert(afterState.get("IY").equals(56))
    assert(afterState.get("PC").equals(0))
  }

  test("set register direct function declaration") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>= (reg=>RegisterController(reg.set("B",36)))
    //then
    assert(afterState.get.reg.equals(Map("B"->36)))
    assert(afterState.get("B").equals(36))
    assert(afterState.get("A").equals(0))
  }

}
