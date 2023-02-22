package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, OutputFile, PortID}
import org.scalatest.funsuite.AnyFunSuite

class OutputFileTest extends AnyFunSuite{
  test("check empty output file") {
    //given
    val outFile=OutputFile.blank
    //when
    //then
    assert(outFile(PortID(0),0)==0)
    assert(outFile(PortID(1),0)==0)
    assert(outFile(PortID(100),5)==0)
    assert(outFile(PortID(300),20)==0)
  }

  test("add elements to output file") {
    //given
    val outFile=OutputFile.blank
    implicit val debugger:Debugger=DummyDebugger
    //when
    val outFileTest=outFile
      .write(PortID(10),0x41)
      .write(PortID(10),0x42)
      .write(PortID(10),0x43)
      .write(PortID(20),0x44)
    //then
    assert(outFileTest(PortID(10),0)==0x41)
    assert(outFileTest(PortID(10),1)==0x42)
    assert(outFileTest(PortID(10),2)==0x43)
    assert(outFileTest(PortID(10),3)==0)
    assert(outFileTest(PortID(20),0)==0x44)
    assert(outFileTest(PortID(20),1)==0)

    //outFileTest.print(10)
  }
}
