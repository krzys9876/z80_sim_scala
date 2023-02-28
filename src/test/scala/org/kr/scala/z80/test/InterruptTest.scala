package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, Regs}
import org.kr.scala.z80.utils.{AnyInt, IntValue}
import org.scalatest.funsuite.AnyFunSuite

class InterruptTest extends AnyFunSuite {

  implicit val debugger:Debugger=DummyDebugger

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
}
