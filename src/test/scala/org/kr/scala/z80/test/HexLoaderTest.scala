package org.kr.scala.z80.test

import org.kr.scala.z80.system.MemoryController
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import java.io.File
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.ListHasAsScala

class HexLoaderTest extends AnyFunSuite with BeforeAndAfterAll {

  val tmpDir:String="./tmp-files"

  override def beforeAll(): Unit = {
    if(!Files.exists(Path.of(tmpDir))) Files.createDirectory(Path.of(tmpDir))
  }

  override def afterAll(): Unit = {}


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

  private def getTmpFilePath(workingDir:String,file:String):Path={
    val tmpDir=new File(workingDir).getCanonicalPath.replace("\\", "/")
    Path.of(tmpDir,file)
  }

  private def saveToFile(filePath:Path,lines:List[String]):Unit={
    val bytes=lines.foldLeft("")((str,line)=>str+(if(!str.isBlank) "\n" else "")+line).getBytes
    Files.write(filePath,bytes)
  }

  test("read hex file") {
    //given
    val lines=List(
      ":10000000F3C3B80000000000C39F00000000000020",":10001000C374000000000000C3AA0000000000003C",
      ":1000300000000000000000001800F5E5DB80E6018C",":10004000282DDB81F53A4320FE3F2003F118202ABA",
      ":100050003F20237DFE3F2003210020223F20F17717",":100060003A43203C324320FE3038043ED6D380E170",
      ":00000001FF"
    )
    val tmpFilePath=getTmpFilePath("./tmp-files","tmp1.hex")
    if(Files.exists(tmpFilePath)) Files.delete(tmpFilePath)

    saveToFile(tmpFilePath,lines)
    val mem=MemoryController.blank(0x100)
    //when
    val linesFromFile=Files.readAllLines(tmpFilePath).asScala.toList
    val memLoaded=mem >>= MemoryController.loadHexLines(linesFromFile)
    //then
    assert(memLoaded.get(0x0000)==0xF3)
    assert(memLoaded.get(0x0001)==0xC3)
    assert(memLoaded.get(0x0010)==0xC3)
    assert(memLoaded.get(0x0011)==0x74)
    assert(memLoaded.get(0x0050)==0x3F)
    assert(memLoaded.get(0x005F)==0x77)
    assert(memLoaded.get(0x0060)==0x3A)
    assert(memLoaded.get(0x006F)==0xE1)
  }
}
