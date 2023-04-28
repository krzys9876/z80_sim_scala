package org.kr.scala.z80.test

import org.kr.scala.z80.opcode.NOP
import org.kr.scala.z80.system.{Debugger, DummyDebugger, ImmutableMemoryHandler, ImmutableRegisterHandler, MemoryChangeByte, MemoryChangeWord, MemoryHandler, MutableMemoryHandler, MutableRegisterHandler, RegisterChange, RegisterChangeRelative, RegisterHandler, Regs, StateWatcher, TCycleCounterHandler, TCycleCounterHandlerImmutable, TCycleCounterHandlerMutable, Z80System}
import org.scalatest.funsuite.AnyFunSuite

class SystemTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  val memoryHandlerImmutable:MemoryHandler=new ImmutableMemoryHandler()
  val registerHandlerImmutable:RegisterHandler=new ImmutableRegisterHandler()
  val tCycleCounterHandlerImmutable:TCycleCounterHandler=new TCycleCounterHandlerImmutable()
  val memoryHandlerMutable: MemoryHandler = new MutableMemoryHandler()
  val registerHandlerMutable: RegisterHandler = new MutableRegisterHandler()
  val tCycleCounterHandlerMutable: TCycleCounterHandler = new TCycleCounterHandlerMutable()

  testAll(memoryHandlerImmutable,registerHandlerImmutable,tCycleCounterHandlerImmutable,"M:I,R:I,C:I")
  testAll(memoryHandlerImmutable,registerHandlerImmutable,tCycleCounterHandlerMutable,"M:I,R:I,C:M")
  testAll(memoryHandlerImmutable, registerHandlerMutable, tCycleCounterHandlerImmutable, "M:I,R:M,C:I")
  testAll(memoryHandlerImmutable, registerHandlerMutable, tCycleCounterHandlerMutable, "M:I,R:M,C:M")
  testAll(memoryHandlerMutable, registerHandlerImmutable, tCycleCounterHandlerImmutable, "M:M,R:I,C:I")
  testAll(memoryHandlerMutable, registerHandlerImmutable, tCycleCounterHandlerMutable, "M:M,R:I,C:M")
  testAll(memoryHandlerMutable, registerHandlerMutable, tCycleCounterHandlerImmutable, "M:M,R:M,C:I")
  testAll(memoryHandlerMutable, registerHandlerMutable, tCycleCounterHandlerMutable, "M:M,R:M,C:M")

  def testAll(implicit memoryHandler: MemoryHandler, registerHandler: RegisterHandler,
              tCycleCounterHandler: TCycleCounterHandler, prefix:String):Unit = {
    test(f"change memory (byte) $prefix") {
      //given
      val system = Z80System.blank
      //when
      val afterState = system.changeList(List(new MemoryChangeByte(0x1234, 0xFE)))
      //then
      assert(afterState.memory(0x1234) == 0xFE)
    }

    test(f"change memory (word) $prefix") {
      //given
      val system = Z80System.blank
      //when
      val afterState = system.changeList(List(new MemoryChangeWord(0x1234, 0xFEFD)))
      //then
      assert(afterState.memory(0x1234) == 0xFD)
      assert(afterState.memory(0x1235) == 0xFE)
    }

    test(f"change register $prefix") {
      //given
      val system = Z80System.blank
      //when
      val afterState = system.changeList(List(new RegisterChange(Regs.AF, 0xFEFD), new RegisterChange(Regs.SP, 0xA1B2)))
      //then
      assert(afterState.register(Regs.A) == 0xFE)
      assert(afterState.register(Regs.F) == 0xFD)
      assert(afterState.register(Regs.SP) == 0xA1B2)
    }

    test(f"change register relative $prefix") {
      //given
      val initSystem = Z80System.blank
      val system = initSystem.changeList(List(new RegisterChange(Regs.SP, 0x1002)))
      //when
      val afterState = system.changeList(List(new RegisterChangeRelative(Regs.SP, 0xF0)))
      //then
      assert(afterState.register(Regs.SP) == 0x10F2)
    }

    test(f"change list $prefix") {
      //given
      val system = Z80System.blank
      //when
      val chgList = List(
        new MemoryChangeByte(0xABCD, 0xA1),
        new MemoryChangeWord(0x3210, 0xB1C2),
        new RegisterChange(Regs.HL, 0xFEDC))
      val afterState = system.changeList(chgList)
      //then
      assert(afterState.register(Regs.HL) == 0xFEDC)
      assert(afterState.memory(0xABCD) == 0xA1)
      assert(afterState.memory(0x3210) == 0xC2)
      assert(afterState.memory(0x3211) == 0xB1)
    }

    test(f"run NOP and increase cycle counter $prefix") {
      //given
      val system = Z80System.blank
      //when
      val afterState = StateWatcher[Z80System](system) >>== Z80System.run(debugger)(10)
      //then
      assert(afterState.get.register(Regs.PC) == 10)
      assert(afterState.get.elapsedTCycles.cycles == NOP.t * 10)
    }
  }
}
