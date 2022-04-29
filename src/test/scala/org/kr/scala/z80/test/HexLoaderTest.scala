package org.kr.scala.z80.test

import org.kr.scala.z80.system.MemoryController
import org.scalatest.funsuite.AnyFunSuite

class HexLoaderTest extends AnyFunSuite{
  test("load single line") {
    //given
    val mem=MemoryController.blank(0x100)
    //when
    val memLoaded=mem >>= MemoryController.loadHexLine(":10004000282DDB81F53A4320FE3F2003F118202ABA")
    //then
    assert(memLoaded.get(0x0040)==0x28)
    assert(memLoaded.get(0x0041)==0x2D)
    assert(memLoaded.get(0x0042)==0xDB)
    assert(memLoaded.get(0x004F)==0x2A)
  }

  test("load multiple lines") {
    //given
    val mem=MemoryController.blank(0x100)
    //when
    val memLoaded=mem >>= MemoryController.loadHexLines(
      List(
        ":10000000F3C3B80000000000C39F00000000000020",
        ":10001000C374000000000000C3AA0000000000003C",
        ":1000300000000000000000001800F5E5DB80E6018C",
        ":10004000282DDB81F53A4320FE3F2003F118202ABA")
    )
    //then
    assert(memLoaded.get(0x0000)==0xF3)
    assert(memLoaded.get(0x0001)==0xC3)
    assert(memLoaded.get(0x0010)==0xC3)
    assert(memLoaded.get(0x0011)==0x74)
    assert(memLoaded.get(0x003E)==0xE6)
    assert(memLoaded.get(0x003F)==0x01)
    assert(memLoaded.get(0x003E)==0xE6)
    assert(memLoaded.get(0x003F)==0x01)
    assert(memLoaded.get(0x0040)==0x28)
    assert(memLoaded.get(0x0041)==0x2D)
    assert(memLoaded.get(0x004F)==0x2A)
  }

  test("load ending line") {
    //given
    val mem=MemoryController.blank(0x100)
    //when
    val memLoaded=mem >>= MemoryController.loadHexLine(":00000001FF")
    //then
    assert(memLoaded.get(0x0000)==0x00)
    assert(memLoaded.get(0x0001)==0x00)
    assert(memLoaded.get(0x0002)==0x00)
  }
}
