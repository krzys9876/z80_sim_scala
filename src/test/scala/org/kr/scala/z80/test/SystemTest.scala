package org.kr.scala.z80.test

import org.kr.scala.z80.opcode.NOP
import org.kr.scala.z80.system.{Debugger, DummyDebugger, ImmutableMemoryHandler, ImmutableRegisterHandler, MemoryHandler, MutableMemoryHandler, MutableRegisterHandler, RegisterHandler, Regs, StateWatcher, StateWatcherHandler, StateWatcherHandlerBase, TCycleCounterHandler, TCycleCounterHandlerImmutable, TCycleCounterHandlerMutable, Z80System}
import org.scalatest.funsuite.AnyFunSuite

class SystemTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

  val memoryHandlerImmutable:MemoryHandler=new ImmutableMemoryHandler()
  val registerHandlerImmutable:RegisterHandler=new ImmutableRegisterHandler()
  val tCycleCounterHandlerImmutable:TCycleCounterHandler=new TCycleCounterHandlerImmutable()
  val memoryHandlerMutable: MemoryHandler = new MutableMemoryHandler()
  val registerHandlerMutable: RegisterHandler = new MutableRegisterHandler()
  val tCycleCounterHandlerMutable: TCycleCounterHandler = new TCycleCounterHandlerMutable()
  val stateWatcherHandler:StateWatcherHandlerBase[Z80System] = new StateWatcherHandler[Z80System]()

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
      val afterState = system.changeMemoryByte(0x1234, 0xFE)
      //then
      assert(afterState.memory(0x1234) == 0xFE)
    }

    test(f"change memory (word) $prefix") {
      //given
      val system = Z80System.blank
      //when
      val afterState = system.changeMemoryWord(0x1234, 0xFEFD)
      //then
      assert(afterState.memory(0x1234) == 0xFD)
      assert(afterState.memory(0x1235) == 0xFE)
    }

    test(f"change register $prefix") {
      //given
      val system = Z80System.blank
      //when
      val afterState = system.changeRegister(Regs.AF, 0xFEFD).changeRegister(Regs.SP, 0xA1B2)
      //then
      assert(afterState.register(Regs.A) == 0xFE)
      assert(afterState.register(Regs.F) == 0xFD)
      assert(afterState.register(Regs.SP) == 0xA1B2)
    }

    test(f"change register relative $prefix") {
      //given
      val initSystem = Z80System.blank
      val system = initSystem.changeRegister(Regs.SP, 0x1002)
      //when
      val afterState = system.changeRegisterRelative(Regs.SP, 0xF0)
      //then
      assert(afterState.register(Regs.SP) == 0x10F2)
    }

    test(f"change list $prefix") {
      //given
      val system = Z80System.blank
      //when
      val afterState = system
        .changeMemoryByte(0xABCD, 0xA1)
        .changeMemoryWord(0x3210, 0xB1C2)
        .changeRegister(Regs.HL, 0xFEDC)
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
      val afterState = stateWatcherHandler.createNewWatcher(system) >>== Z80System.run(debugger,stateWatcherHandler)(10)
      //then
      assert(afterState.get.register(Regs.PC) == 10)
      assert(afterState.get.elapsedTCycles.cycles == NOP.t * 10)
    }
  }
}
