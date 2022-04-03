package org.kr.scala.z80.test

import org.kr.scala.z80.system.RegisterController
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
    assert(afterState.get("B").equals(36))
    assert(afterState.get("A").equals(0))
  }

  test("set register pairs") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>=
      RegisterController.set("AF",0x0102) >>= RegisterController.set("BC",0x0304) >>=
      RegisterController.set("DE",0x0506) >>= RegisterController.set("HL",0x0708) >>=
      RegisterController.set("AF1",0xF1F2) >>= RegisterController.set("BC1",0xF3F4) >>=
      RegisterController.set("DE1",0xF5F6) >>= RegisterController.set("HL1",0xF7F8)
    //then
    assert(afterState.get("A").equals(1))
    assert(afterState.get("F").equals(2))
    assert(afterState.get("B").equals(3))
    assert(afterState.get("C").equals(4))
    assert(afterState.get("D").equals(5))
    assert(afterState.get("E").equals(6))
    assert(afterState.get("H").equals(7))
    assert(afterState.get("L").equals(8))
    assert(afterState.get("AF1").equals(0xF1F2))
    assert(afterState.get("BC1").equals(0xF3F4))
    assert(afterState.get("DE1").equals(0xF5F6))
    assert(afterState.get("HL1").equals(0xF7F8))
  }

  test("get register pairs") {
    //given
    val registerController=RegisterController.blank
    //when
    val afterState=registerController >>=
      RegisterController.set("A",1) >>= RegisterController.set("F",2) >>=
      RegisterController.set("B",3) >>= RegisterController.set("C",4) >>=
      RegisterController.set("D",5) >>= RegisterController.set("E",6) >>=
      RegisterController.set("H",7) >>= RegisterController.set("L",8)
    //then
    assert(afterState.get("AF").equals(0x0102))
    assert(afterState.get("BC").equals(0x0304))
    assert(afterState.get("DE").equals(0x0506))
    assert(afterState.get("HL").equals(0x0708))
  }
}
