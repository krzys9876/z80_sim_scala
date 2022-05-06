package org.kr.scala.z80.test

import org.kr.scala.z80.system.{Debugger, DummyDebugger, OutputFile}
import org.scalatest.funsuite.AnyFunSuite

class OutputFileTest extends AnyFunSuite{
  test("check empty output file") {
    //given
    val outFile=OutputFile.blank
    //when
    //then
    assert(outFile(0,0)==0)
    assert(outFile(1,0)==0)
    assert(outFile(100,5)==0)
    assert(outFile(300,20)==0)
  }

  test("add elements to output file") {
    //given
    val outFile=OutputFile.blank
    implicit val debugger:Debugger=DummyDebugger
    //when
    val outFileTest=outFile
      .put(10,0x41)
      .put(10,0x42)
      .put(10,0x43)
      .put(20,0x44)
    //then
    assert(outFileTest(10,0)==0x41)
    assert(outFileTest(10,1)==0x42)
    assert(outFileTest(10,2)==0x43)
    assert(outFileTest(10,3)==0)
    assert(outFileTest(20,0)==0x44)
    assert(outFileTest(20,1)==0)

    //outFileTest.print(10)
  }
}
