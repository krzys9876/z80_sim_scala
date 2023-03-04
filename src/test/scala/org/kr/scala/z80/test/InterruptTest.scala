package org.kr.scala.z80.test

import org.kr.scala.z80.system.{CyclicInterrupt, Debugger, DummyDebugger, ImmutableMemory, InputFile, MemoryHandler, OutputFile, Register, Regs, StateWatcher, Z80System}
import org.kr.scala.z80.utils.{AnyInt, IntValue}
import org.scalatest.funsuite.AnyFunSuite

class InterruptTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger
  implicit val memoryHandler:MemoryHandler=ImmutableMemory

  test("run EI / DI") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IFF, 0x00), (Regs.PC, 0x0100)), List((0x0100, 0xFB)),
      Regs.IFF, AnyInt, IntValue(0x01), "00_0_000", 0x0101)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IFF, 0x01), (Regs.PC, 0x0100)), List((0x0100, 0xFB)),
      Regs.IFF, AnyInt, IntValue(0x01), "00_0_000", 0x0101)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IFF, 0x01), (Regs.PC, 0x0100)), List((0x0100, 0xF3)),
      Regs.IFF, AnyInt, IntValue(0x00), "00_0_000", 0x0101)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IFF, 0x00), (Regs.PC, 0x0100)), List((0x0100, 0xF3)),
      Regs.IFF, AnyInt, IntValue(0x00), "00_0_000", 0x0101)
  }

  test("run IM 0/1/2") {
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IM, 0x01), (Regs.PC, 0x0100)), List((0x0100, 0xED),(0x0101, 0x46)),
      Regs.IM, AnyInt, IntValue(0x00), "00_0_000", 0x0102)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IM, 0x00), (Regs.PC, 0x0100)), List((0x0100, 0xED), (0x0101, 0x46)),
      Regs.IM, AnyInt, IntValue(0x00), "00_0_000", 0x0102)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IM, 0x00), (Regs.PC, 0x0100)), List((0x0100, 0xED), (0x0101, 0x56)),
      Regs.IM, AnyInt, IntValue(0x01), "00_0_000", 0x0102)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IM, 0x01), (Regs.PC, 0x0100)), List((0x0100, 0xED), (0x0101, 0x56)),
      Regs.IM, AnyInt, IntValue(0x01), "00_0_000", 0x0102)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IM, 0x00), (Regs.PC, 0x0100)), List((0x0100, 0xED), (0x0101, 0x5E)),
      Regs.IM, AnyInt, IntValue(0x02), "00_0_000", 0x0102)
    TestUtils.testRegOrAddrWithFlags(List((Regs.F, 0x00), (Regs.IM, 0x02), (Regs.PC, 0x0100)), List((0x0100, 0xED), (0x0101, 0x5E)),
      Regs.IM, AnyInt, IntValue(0x02), "00_0_000", 0x0102)
  }

  private def interruptTestProgram:List[(Int,Int)] = List(
    (0x0000, 0x00), (0x0001, 0x00), (0x0002, 0x00), (0x0003, 0x00), (0x0004, 0x00), // NOP 5 x 4T
    (0x0005, 0x00), (0x0006, 0x00), (0x0007, 0x00), (0x0008, 0x00), (0x0009, 0x00), // NOP 5 x 4T
    (0x000A, 0xC3), (0x000B, 0x00), (0x000C, 0x00), //JP 0x0000 10T
    // interrupt vector for IM 1, interrupt takes 11T+2T (RST is 11T plus two extra wait cycles)
    (0x0038, 0x3C), //INC A 4T
    (0x0039, 0xED), (0x003A, 0x4D)) //RETI 14T

  test("trigger interrupt (IM 1)") {
    //given
    val sysBlank=new Z80System(ImmutableMemory.blank(0x0100),Register.blank,OutputFile.blank,InputFile.blank,
      0,Z80System.use8BitIOPorts,CyclicInterrupt(50))
    val memList=interruptTestProgram
    val regList=List((Regs.IFF,1),(Regs.IM,1),(Regs.SP,0x00FF),(Regs.A,0x00))
    // run to interrupt routine, before RETI
    // full loop: 10x4T + 10T + 2 x 4T, but probing value is set before instruction (it originates from the previous run)
    // so we get below 0 when executing the second NOP of the second loop, not the first
    val run1=TestUtils.prepareTestWith(StateWatcher(sysBlank),regList,memList,14)
    // then
    assert(run1.state.getRegValue(Regs.PC)==0x0039) //inside interrupt routine, after INC A
    assert(run1.state.getRegValue(Regs.A)==0x01) // accumulator increased inside interrupt routine
    // when
    // run to interrupt routine, than return from it (after RETI)
    val run2 = TestUtils.prepareTestWith(StateWatcher(sysBlank), regList, memList, 15)
    // then
    assert(run2.state.getRegValue(Regs.PC) == 0x0002) //after interrupt routine, before third NOP of the second loop
    // when
    // run again to interrupt routine, before RETI
    val run3 = TestUtils.prepareTestWith(StateWatcher(sysBlank), regList, memList, 20)
    // then
    assert(run3.state.getRegValue(Regs.PC) == 0x0039) //inside interrupt routine, after INC A
    assert(run3.state.getRegValue(Regs.A) == 0x02) // accumulator increased inside interrupt routine
  }
  test("trigger interrupt (IM 0) - unsupported") {
    //given
    val sysBlank = new Z80System(ImmutableMemory.blank(0x0100), Register.blank, OutputFile.blank, InputFile.blank,
      0, Z80System.use8BitIOPorts, CyclicInterrupt(50))
    val memList = interruptTestProgram
    val regList = List((Regs.IFF, 1), (Regs.IM, 0), (Regs.SP, 0x00FF), (Regs.A, 0x00))
    // when
    // then
    assertThrows[AssertionError](TestUtils.prepareTestWith(StateWatcher(sysBlank), regList, memList, 14))
  }
  test("do not trigger interrupt when IFF is 0") {
    //given
    val sysBlank = new Z80System(ImmutableMemory.blank(0x0100), Register.blank, OutputFile.blank, InputFile.blank,
      0, Z80System.use8BitIOPorts, CyclicInterrupt(50))
    val memList = interruptTestProgram
    val regList = List((Regs.IFF, 0), (Regs.IM, 1), (Regs.SP, 0x00FF), (Regs.A, 0x00))
    // when
    // when interrupts are disabled the program will not enter the interrupt routine
    // and accumulator will always be 0 (it is increased in the interrupt routine)
    val run = TestUtils.prepareTestWith(StateWatcher(sysBlank), regList, memList, 100)
    // then
    assert(run.state.getRegValue(Regs.A)==0)
  }
  test("run HALT w/o interrupts") {
    //given
    val sysBlank = new Z80System(ImmutableMemory.blank(0x0100), Register.blank, OutputFile.blank, InputFile.blank,
      0, Z80System.use8BitIOPorts, CyclicInterrupt(50))
    val memList = List((0x0000,0x76)) // HALT
    val regList = List((Regs.IFF, 0), (Regs.IM, 0), (Regs.SP, 0x00FF), (Regs.A, 0x00))
    // when
    // when interrupts are disabled the program will not enter the interrupt routine
    // and stay in HALT instruction indefinitely
    val run = TestUtils.prepareTestWith(StateWatcher(sysBlank), regList, memList, 100)
    // then
    assert(run.state.getRegValue(Regs.PC) == 0x0000)
  }
  test("run HALT with interrupts") {
    //given
    val sysBlank = new Z80System(ImmutableMemory.blank(0x0100), Register.blank, OutputFile.blank, InputFile.blank,
      0, Z80System.use8BitIOPorts, CyclicInterrupt(40))
    val memList = List((0x0000, 0x76),(0x0038, 0xED), (0x0039, 0x4D)) // HALT + RETI (the whole interrupt routine is RETI)
    val regList = List((Regs.IFF, 1), (Regs.IM, 1), (Regs.SP, 0x00FF), (Regs.A, 0x00))
    // when
    // when interrupts are enabled the program will escape from HALT when interrupt is triggered
    val run1 = TestUtils.prepareTestWith(StateWatcher(sysBlank), regList, memList, 12)
    // then
    assert(run1.state.getRegValue(Regs.PC) == 0x0038)
    // when
    // after interrupt routine is finished the program returns after HALT
    val run2 = TestUtils.prepareTestWith(StateWatcher(sysBlank), regList, memList, 13)
    // then
    assert(run2.state.getRegValue(Regs.PC) == 0x0001)
  }
}
