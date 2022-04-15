package org.kr.scala.z80.test

import org.kr.scala.z80.system.{InputController, MemoryController, OutputController, RegisterController, Z80System, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class OpLoad8BitTypeTest extends AnyFunSuite {
  // TEST LOAD 8-BIT

  test("run LD B,0xFE") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(),List((10,0x06),(11,0xFE)),11) //LD H,0xFE
    //then
    assert(sysTest.get.registerController.get("PC")==12)
    assert(sysTest.get.registerController.get("B")==0xFE)
  }

  test("run LD H,0xFF | LD A,H") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(),
      List((0,0x26),(1,0xFF), //LD H,0xFE
        (2,0x7C)),2) // LD A, H
    //then
    assert(sysTest.get.registerController.get("PC")==3)
    assert(sysTest.get.registerController.get("A")==0xFF)
    assert(sysTest.get.registerController.get("H")==0xFF)
  }

  test("run LD C,(HL)") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("H",0x01),("L",0x02)),
      List((0,0x4E), //LD C,(HL)
        (0x0102,0xFE)))
    //then
    assert(sysTest.get.registerController.get("PC")==1)
    assert(sysTest.get.registerController.get("C")==0xFE)
  }

  test("run LD (HL),E") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("H",0x01),("L",0x02),("E",0xFF)),
      List((0,0x73) //LD (HL),E
        ))
    //then
    assert(sysTest.get.registerController.get("PC")==1)
    assert(sysTest.get.memoryController.get(0x0102)==0xFF)
  }

  test("run LD r,(IX+d) | LD r,(IY+d)") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("IX",0x0101),("IY",0x0109)),
      List((0,0xDD),(1,0x56),(2,0x05), //LD D,(IX+5)
        (3,0xFD),(4,0x5E),(5,0xFE), //LD E,(IY-2)
        (0x0106,0xFF),(0x0107,0xFE)),2)
    //then
    assert(sysTest.get.registerController.get("PC")==6)
    assert(sysTest.get.registerController.get("D")==0xFF)
    assert(sysTest.get.registerController.get("E")==0xFE)
  }

  test("run LD (IX+d),r | LD (IY+d),r") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("IX",0x0100),("IY",0x0109),("A",0x01),("B",0x02)),
      List((0,0xDD),(1,0x77),(2,0x03), //LD D,(IX+5)
        (3,0xFD),(4,0x70),(5,0xFC), //LD E,(IY-2)
        (0x0103,0x01),(0x0105,0x02)),2)
    //then
    assert(sysTest.get.registerController.get("PC")==6)
    assert(sysTest.get.memoryController.get(0x0103)==0x01)
    assert(sysTest.get.memoryController.get(0x0105)==0x02)
  }

  test("run LD (HL),n") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("H",0x01),("L",0x02)),
      List((0,0x36),(1,0xFF),//LD (HL),0xFF
        ))
    //then
    assert(sysTest.get.registerController.get("PC")==2)
    assert(sysTest.get.registerController.get("H")==1)
    assert(sysTest.get.registerController.get("L")==2)
    assert(sysTest.get.memoryController.get(0x0102)==0xFF)
  }

  test("run LD (IX+d),n | LD (IY+d),n") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("IX",0x0104),("IY",0x0101)),
      List((0,0xDD),(1,0x36),(2,0xFE),(3,0xFF), //LD (IX-2),0xFF
        (4,0xFD),(5,0x36),(6,0x03),(7,0xFE), //LD (IY+3),0xFE
        ),2)
    //then
    assert(sysTest.get.registerController.get("PC")==8)
    assert(sysTest.get.memoryController.get(0x0102)==0xFF)
    assert(sysTest.get.memoryController.get(0x0104)==0xFE)
  }

  test("run LD A,(BC)") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("B", 0x01),("C", 0x02)),
      List((0, 0x0A),//LD A,(BC)
        (0x0102,0xFE)
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("A") == 0xFE)
  }

  test("run LD A,(DE)") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("D", 0x01),("E", 0x03)),
      List((0, 0x1A), //LD A,(DE)
        (0x0103,0xFD)
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.registerController.get("A") == 0xFD)
  }

  test("run LD (BC),A") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("B", 0x01),("C", 0x02),("A", 0xFF)),
      List((0, 0x02) //LD (BC),A
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.memoryController.get(0x0102) == 0xFF)
  }

  test("run LD (DE),A") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("D", 0x02),("E", 0x03),("A", 0xFE)),
      List((0, 0x12) //LD (DE),A
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 1)
    assert(sysTest.get.memoryController.get(0x0203) == 0xFE)
  }

  test("run LD A,(nn)") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(),
      List((0,0x3A),(1,0x02),(2,0x01), //LD A,(nn)
        (0x0102, 0xFE)
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 3)
    assert(sysTest.get.registerController.get("A") == 0xFE)
  }

  test("run LD (nn),A") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("A", 0xFF)),
      List((0, 0x32),(1,0x02),(2,0x01) //LD (nn),A
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 3)
    assert(sysTest.get.memoryController.get(0x0102) == 0xFF)
  }

  test("run LD A,I") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("I", 0xFF)),
      List((0,0xED),(1,0x57) //LD A,I
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("A") == 0xFF)
  }

  test("run LD A,R") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("R", 0xFE)),
      List((0,0xED),(1,0x5F) //LD A,R
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("A") == 0xFE)
  }

  test("run LD I,A") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("A", 0xFD)),
      List((0,0xED),(1,0x47) //LD I,A
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("I") == 0xFD)
  }

  test("run LD R,A") {
    //given
    //when
    val sysTest=TestUtils.prepareTest(List(("A", 0xFC)),
      List((0,0xED),(1,0x4F) //LD R,A
      ))
    //then
    assert(sysTest.get.registerController.get("PC") == 2)
    assert(sysTest.get.registerController.get("R") == 0xFC)
  }
}
