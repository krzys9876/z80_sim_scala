package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, Flag, ImmutableRegister, ImmutableRegisterHandler, RegisterBase, RegisterHandler, Regs, StateWatcher}
import org.scalatest.funsuite.AnyFunSuite

class RegisterTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  testAll(new ImmutableRegisterHandler(),"immutable")

  def testAll(implicit registerHandler: RegisterHandler, prefix:String):Unit = {

    test("init blank register ($prefix)") {
      //given
      val registerController = StateWatcher[RegisterBase](registerHandler.blank)
      //when
      //then
      assert(registerController.get(Regs.A).equals(0xFF))
      assert(registerController.get(Regs.PC).equals(0))
      assert(registerController.get(Regs.F).equals(0xFF))
      assert(registerController.get(Regs.SP).equals(0xFFFF))
    }

    test("set register ($prefix)") {
      //given
      val registerController = StateWatcher[RegisterBase](registerHandler.blank)
      //when
      val afterState = registerController >>== registerHandler.set(Regs.IX, 255) >>== registerHandler.set(Regs.IY, 56)
      //then
      assert(afterState.get(Regs.IX).equals(255))
      assert(afterState.get(Regs.IY).equals(56))
      assert(afterState.get(Regs.PC).equals(0))
    }

    test("set register relative ($prefix)") {
      //given
      val registerController = StateWatcher[RegisterBase](registerHandler.blank)
      //when
      val afterState = registerController >>== registerHandler.set(Regs.SP, 128) >>== registerHandler.relative(Regs.SP, -2)
      //then
      assert(afterState.get(Regs.SP).equals(126))
    }

    test("set register direct function declaration ($prefix)") {
      //given
      val registerController = StateWatcher[RegisterBase](registerHandler.blank)
      //when
      val afterState = registerController >>= (reg => StateWatcher[RegisterBase](reg.set(Regs.B, 36)))
      //then
      assert(afterState.get(Regs.B).equals(36))
    }

    test("set register pairs ($prefix)") {
      //given
      val registerController = StateWatcher[RegisterBase](registerHandler.blank)
      //when
      val afterState = registerController >>==
        registerHandler.set(Regs.AF, 0x0102) >>== registerHandler.set(Regs.BC, 0x0304) >>==
        registerHandler.set(Regs.DE, 0x0506) >>== registerHandler.set(Regs.HL, 0x0708) >>==
        registerHandler.set(Regs.AF1, 0xF1F2) >>== registerHandler.set(Regs.BC1, 0xF3F4) >>==
        registerHandler.set(Regs.DE1, 0xF5F6) >>== registerHandler.set(Regs.HL1, 0xF7F8)
      //then
      assert(afterState.get(Regs.A).equals(1))
      assert(afterState.get(Regs.F).equals(2))
      assert(afterState.get(Regs.B).equals(3))
      assert(afterState.get(Regs.C).equals(4))
      assert(afterState.get(Regs.D).equals(5))
      assert(afterState.get(Regs.E).equals(6))
      assert(afterState.get(Regs.H).equals(7))
      assert(afterState.get(Regs.L).equals(8))
      assert(afterState.get(Regs.AF1).equals(0xF1F2))
      assert(afterState.get(Regs.BC1).equals(0xF3F4))
      assert(afterState.get(Regs.DE1).equals(0xF5F6))
      assert(afterState.get(Regs.HL1).equals(0xF7F8))
    }

    test("get register pairs ($prefix)") {
      //given
      val registerController = StateWatcher[RegisterBase](registerHandler.blank)
      //when
      val afterState = registerController >>==
        registerHandler.set(Regs.A, 1) >>== registerHandler.set(Regs.F, 2) >>==
        registerHandler.set(Regs.B, 3) >>== registerHandler.set(Regs.C, 4) >>==
        registerHandler.set(Regs.D, 5) >>== registerHandler.set(Regs.E, 6) >>==
        registerHandler.set(Regs.H, 7) >>== registerHandler.set(Regs.L, 8)
      //then
      assert(afterState.get(Regs.AF).equals(0x0102))
      assert(afterState.get(Regs.BC).equals(0x0304))
      assert(afterState.get(Regs.DE).equals(0x0506))
      assert(afterState.get(Regs.HL).equals(0x0708))
    }

    test("check flags 1 ($prefix)") {
      //given
      val registerController = StateWatcher[RegisterBase](registerHandler.blank)
      //when
      val afterState = registerController >>== registerHandler.set(Regs.F, 0xAA)
      //then
      assert(afterState.get(Regs.F) == 0xAA)
      assert(afterState.get(Flag.S))
      assert(!afterState.get(Flag.Z))
      assert(!afterState.get(Flag.H))
      assert(!afterState.get(Flag.P))
      assert(afterState.get(Flag.N))
      assert(!afterState.get(Flag.C))
    }

    test("check flags 2 ($prefix)") {
      //given
      val registerController = StateWatcher[RegisterBase](registerHandler.blank)
      //when
      val afterState = registerController >>== registerHandler.set(Regs.F, 0x55)
      //then
      assert(afterState.get(Regs.F) == 0x55)
      assert(!afterState.get(Flag.S))
      assert(afterState.get(Flag.Z))
      assert(afterState.get(Flag.H))
      assert(afterState.get(Flag.P))
      assert(!afterState.get(Flag.N))
      assert(afterState.get(Flag.C))
    }
  }
}
