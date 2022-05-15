package org.kr.scala.z80.test

import org.kr.scala.z80.system.{StateWatcher, Debugger, DummyDebugger, InputFile, InputPortConstant, InputPortMultiple, InputPortSequential, InputPortSingle}
import org.scalatest.funsuite.AnyFunSuite

class InputFileTest extends AnyFunSuite{

  implicit val debugger:Debugger=DummyDebugger

  test("input from blank input file") {
    //given
    val inC=StateWatcher[InputFile](InputFile.blank)
    //when
    //then
    assert(inC.get.read(0xFF)==0)
    assert(inC.get.read(0x00)==0)
  }

  test("input from constant input file") {
    //given
    val inC=StateWatcher[InputFile](InputFile.blank) >>== InputFile.attachPort(0x10,new InputPortConstant(0xAA))
    //when
    val inCAfter=inC >>== InputFile.refreshPort(0x10)
    //then
    assert(inC.get.read(0x10)==0xAA)
    assert(inCAfter.get.read(0x10)==0xAA)
  }

  test("input from sequential input file") {
    //given
    val inC=StateWatcher[InputFile](InputFile.blank) >>== InputFile.attachPort(0x33,new InputPortSequential(0xBB,3,0,0x11))
    //when
    val inCAfter1=inC >>== InputFile.refreshPort(0x33)
    val inCAfter2=inCAfter1 >>== InputFile.refreshPort(0x33)
    val inCAfter3=inCAfter2 >>== InputFile.refreshPort(0x33)
    val inCAfter4=inCAfter3 >>== InputFile.refreshPort(0x33)
    //then
    assert(inC.get.read(0x33)==0xBB)
    assert(inCAfter1.get.read(0x33)==0x11)
    assert(inCAfter2.get.read(0x33)==0x11)
    assert(inCAfter3.get.read(0x33)==0xBB)
    assert(inCAfter4.get.read(0x33)==0x11)
  }

  test("input from single-value input file") {
    //given
    val inC=StateWatcher[InputFile](InputFile.blank) >>== InputFile.attachPort(0x41,new InputPortSingle(0xCC,0x22))
    //when
    val inCAfter1=inC >>== InputFile.refreshPort(0x41)
    val inCAfter2=inCAfter1 >>== InputFile.refreshPort(0x41)
    //then
    assert(inC.get.read(0x41)==0xCC)
    assert(inCAfter1.get.read(0x41)==0x22)
    assert(inCAfter2.get.read(0x41)==0x22)
  }

  test("input from multi-value input file") {
    //given
    val inC=StateWatcher[InputFile](InputFile.blank) >>== InputFile.attachPort(0x52,new InputPortMultiple(List(0x01,0x02,0x03),0xFF))
    //when
    val inCAfter1=inC >>== InputFile.refreshPort(0x52)
    val inCAfter2=inCAfter1 >>== InputFile.refreshPort(0x52)
    val inCAfter3=inCAfter2 >>== InputFile.refreshPort(0x52)
    //then
    assert(inC.get.read(0x52)==0x01)
    assert(inCAfter1.get.read(0x52)==0x02)
    assert(inCAfter2.get.read(0x52)==0x03)
    assert(inCAfter3.get.read(0x52)==0xFF)
  }
}
