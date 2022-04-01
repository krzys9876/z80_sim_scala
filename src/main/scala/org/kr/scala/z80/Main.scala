package org.kr.scala.z80

object Main extends App {

  println(LocationSpec8Bit("A",OpCode.ANY,OpCode.ANY,"",OpCode.ANY,OpCode.ANY))
  println(LocationSpec8Bit("",0xFC,OpCode.ANY,"",OpCode.ANY,OpCode.ANY))
  println(LocationSpec8Bit("",OpCode.ANY,0x03,"",OpCode.ANY,OpCode.ANY))
  println(LocationSpec8Bit("",OpCode.ANY,OpCode.ANY,"HL",OpCode.ANY,OpCode.ANY))
  println(LocationSpec8Bit("",OpCode.ANY,OpCode.ANY,"PC",0x0A,OpCode.ANY))
  println(LocationSpec8Bit("",OpCode.ANY,OpCode.ANY,"IX",OpCode.ANY,0x0A))

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