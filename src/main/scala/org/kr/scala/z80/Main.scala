package org.kr.scala.z80

import org.kr.scala.z80.opcode.handler.Load16Bit
import org.kr.scala.z80.opcode.{ADD_A_reg, Label, Location, OpCode, OpCodes}
import org.kr.scala.z80.system.{ConsoleDebugger, ConsoleDetailedDebugger, Debugger, InputController, InputPortSingle, MemoryController, OutputController, RegisterController, Z80System, Z80SystemController}
import org.kr.scala.z80.utils.Z80Utils

import scala.jdk.CollectionConverters.ListHasAsScala
import java.nio.file.{Files, Path}

object Main extends App {

  val CONTROL_PORT=0xB1
  val DATA_PORT=0xB0
  val CR=0x0D
  val hexFile=Path.of("C:\\data\\data","basicall_KR_simpleIO_01.hex")
  val hexLines=Files.readAllLines(hexFile).asScala.toList
  // memory
  val memory=MemoryController.blank(0x10000) >>= MemoryController.loadHexLines(hexLines) >>= MemoryController.lockTo(0x4000)
  val input=InputController.blank >>= InputController.attachPort(CONTROL_PORT,new InputPortSingle(1)) >>= InputController.attachPort(DATA_PORT,new InputPortSingle(CR))
  val initSystem=Z80SystemController(new Z80System(memory,RegisterController.blank,OutputController.blank,input))

  implicit val debugger:Debugger=ConsoleDetailedDebugger
  val after=initSystem >>= Z80SystemController.run(debugger)(2000)

  println(after.get.outputController.get.print(DATA_PORT))

  //println(OpCodes.operation8bMap.keys)
  //println(OpCodes.operation8bMap.keys.flatten.toList)
  //println(OpCodes.operation8bMap.keys.flatten.toList.contains(ADD_A_H))

  //println(BitManipulation.bit.find(OpCode(0xC8,0x9E)))

  /*println(Z80Utils.rawByteTo2Compl(0))
  println(Z80Utils.rawByteTo2Compl(1))
  println(Z80Utils.rawByteTo2Compl(2))
  println(Z80Utils.rawByteTo2Compl(126))
  println(Z80Utils.rawByteTo2Compl(127))
  println(Z80Utils.rawByteTo2Compl(128))
  println(Z80Utils.rawByteTo2Compl(129))
  println(Z80Utils.rawByteTo2Compl(130))
  println(Z80Utils.rawByteTo2Compl(254))
  println(Z80Utils.rawByteTo2Compl(255))

  println(Z80Utils.rawByteTo2Compl(254)+Z80Utils.rawByteTo2Compl(1))
  println(Z80Utils.rawByteTo2Compl(Z80Utils.rawByteTo2Compl(254)+Z80Utils.rawByteTo2Compl(1)))
  println(Z80Utils.add8bit(254,1))
  println(Z80Utils.rawByteTo2Compl(Z80Utils.add8bit(254,1)))
  println(Z80Utils.rawByteTo2Compl(254)+Z80Utils.rawByteTo2Compl(3))
  println(Z80Utils.rawByteTo2Compl(Z80Utils.rawByteTo2Compl(254)+Z80Utils.rawByteTo2Compl(3)))
  println(Z80Utils.add8bit(254,3))
  println(Z80Utils.rawByteTo2Compl(Z80Utils.add8bit(254,3)))
*/
  /*println(LoadLocation("A",OpCode.ANY,OpCode.ANY,"",OpCode.ANY,OpCode.ANY))
  println(LoadLocation("",0xFC,OpCode.ANY,"",OpCode.ANY,OpCode.ANY))
  println(LoadLocation("",OpCode.ANY,0x03,"",OpCode.ANY,OpCode.ANY))
  println(LoadLocation("",OpCode.ANY,OpCode.ANY,"HL",OpCode.ANY,OpCode.ANY))
  println(LoadLocation("",OpCode.ANY,OpCode.ANY,"PC",0x0A,OpCode.ANY))
  println(LoadLocation("",OpCode.ANY,OpCode.ANY,"IX",OpCode.ANY,0x0A))

  println(OpCode(0x47,OpCode.ANY).isLoad8Bit)*/

  //println(Load8Bit.destRegListMap)
  //println(Load8Bit.destReg)

  //println(Load8Bit.getDestReg(OpCode(124,0)))


  /*val s=MemoryController.blank(10)
  val s1 = s >>= MemoryController.poke(1,2) >>= MemoryController.poke(3,123) >>= (mem=>MemoryController(mem.replaceAt(7,77)))
  print(s1.state.mem)*/

  /*val z=Z80SystemController.blank
  val initMemory=z.get.memoryController >>=
    MemoryController.poke(0,0x3E) >>=
    MemoryController.poke(1,0xFF) >>=
    MemoryController.poke(2,0x06) >>=
    MemoryController.poke(3,0xFE) >>=
    MemoryController.poke(4,0x0E) >>=
    MemoryController.poke(5,0xFD) >>=
    MemoryController.poke(6,0x16) >>=
    MemoryController.poke(7,0xFC) >>=
    MemoryController.poke(8,0x67) >>=
    MemoryController.poke(9,0x69)
  //0x3E,0xFF,0x06,0xFE,0x0E,0xFD,0x16,0xFC
  //LD A,0xFF | LD B,0xFE | LD C,0xFD | LD E,0xFC | LD H,A | LD L,C
  println(initMemory.get.mem.slice(0,20))
  println(z.state.registerController.get("PC"))
  val zInit=Z80SystemController(new Z80System(MemoryController(initMemory.get),RegisterController.blank))
  val zAfter1=zInit >>= Z80SystemController.run(1)
  println(zAfter1.state.registerController.get("PC"))
  val zAfter2=zAfter1 >>= Z80SystemController.run(65536)
  println(zAfter2.state.registerController.get("PC"))
  println(zAfter2.state.registerController.get.reg)*/
}