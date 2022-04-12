package org.kr.scala.z80.test

import org.kr.scala.z80.system.{MemoryController, OutputController, RegisterController, Z80System, Z80SystemController}
import org.scalatest.funsuite.AnyFunSuite

class OpInOutTest extends AnyFunSuite {

  def prepareTestIO(regList: List[(String, Int)], memList: List[(Int, Int)], portList:List[Int]): Z80SystemController = {
    //given
    val sysBlank = Z80SystemController.blank
    val reg = regList.foldLeft(sysBlank.get.registerController)(
      (regC, entry) => regC >>= RegisterController.set(entry._1, entry._2)
    )

    val mem = memList.foldLeft(sysBlank.get.memoryController)(
      (memC, entry) => memC >>= MemoryController.poke(entry._1, entry._2)
    )

    //when
    val sysInit = Z80SystemController(new Z80System(mem, reg,OutputController.blank))
    val sysTest = sysInit >>= Z80SystemController.run(1)
    Z80SystemController(sysTest.get)
  }

  test("run OUT") {
    //given
    //when
    val systemC=TestUtils.prepareTest(List(("A",0x41)),List((0x0000,0xD3),(0x0001,0x01)))
    //then
    assert(systemC.get.registerController.get("PC")==2)
    assert(systemC.get.outputController.get(1,0)==0x41)
    assert(systemC.get.outputController.get(111,112)==0)

    //systemC.get.outputController.get.print(1)
  }
}
