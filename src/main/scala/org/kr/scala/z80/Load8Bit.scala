package org.kr.scala.z80

object Load8Bit {
  // Z80 manual page 42
  val sourceLocListMap:Map[List[OpCode],LocationSpec8Bit]=Map(
    // registers
    List(OpCode(0xED,0x57)) -> LocationSpec8Bit.register("I"),
    List(OpCode(0xED,0x5F)) -> LocationSpec8Bit.register("R"),
    List(OpCode(0x7F,OpCode.ANY),OpCode(0x4F,OpCode.ANY),OpCode(0x47,OpCode.ANY),OpCode(0x57,OpCode.ANY),
      OpCode(0x5F,OpCode.ANY),OpCode(0x67,OpCode.ANY),OpCode(0x6F,OpCode.ANY),OpCode(0x77,OpCode.ANY),
      OpCode(0x02,OpCode.ANY),OpCode(0x12,OpCode.ANY),OpCode(0xDD,0x77),OpCode(0xFD,0x77),
      OpCode(0x32,OpCode.ANY),OpCode(0xED,0x47),OpCode(0xED,0x4F)) -> LocationSpec8Bit.register("A"),
    List(OpCode(0x78,OpCode.ANY),OpCode(0x40,OpCode.ANY),OpCode(0x48,OpCode.ANY),OpCode(0x50,OpCode.ANY),
      OpCode(0x58,OpCode.ANY),OpCode(0x60,OpCode.ANY),OpCode(0x68,OpCode.ANY),OpCode(0x70,OpCode.ANY),
      OpCode(0xDD,0x70),OpCode(0xFD,0x70)) -> LocationSpec8Bit.register("B"),
    List(OpCode(0x79,OpCode.ANY),OpCode(0x41,OpCode.ANY),OpCode(0x49,OpCode.ANY),OpCode(0x51,OpCode.ANY),
      OpCode(0x59,OpCode.ANY),OpCode(0x61,OpCode.ANY),OpCode(0x69,OpCode.ANY),OpCode(0x71,OpCode.ANY),
      OpCode(0xDD,0x71),OpCode(0xFD,0x71)) -> LocationSpec8Bit.register("C"),
    List(OpCode(0x7A,OpCode.ANY),OpCode(0x42,OpCode.ANY),OpCode(0x4A,OpCode.ANY),OpCode(0x52,OpCode.ANY),
      OpCode(0x5A,OpCode.ANY),OpCode(0x62,OpCode.ANY),OpCode(0x6A,OpCode.ANY),OpCode(0x72,OpCode.ANY),
      OpCode(0xDD,0x72),OpCode(0xFD,0x72)) -> LocationSpec8Bit.register("D"),
    List(OpCode(0x7B,OpCode.ANY),OpCode(0x43,OpCode.ANY),OpCode(0x4B,OpCode.ANY),OpCode(0x53,OpCode.ANY),
      OpCode(0x5B,OpCode.ANY),OpCode(0x63,OpCode.ANY),OpCode(0x6B,OpCode.ANY),OpCode(0x73,OpCode.ANY),
      OpCode(0xDD,0x73),OpCode(0xFD,0x73)) -> LocationSpec8Bit.register("E"),
    List(OpCode(0x7C,OpCode.ANY),OpCode(0x44,OpCode.ANY),OpCode(0x4C,OpCode.ANY),OpCode(0x54,OpCode.ANY),
      OpCode(0x5C,OpCode.ANY),OpCode(0x64,OpCode.ANY),OpCode(0x6C,OpCode.ANY),OpCode(0x74,OpCode.ANY),
      OpCode(0xDD,0x74),OpCode(0xFD,0x74)) -> LocationSpec8Bit.register("H"),
    List(OpCode(0x7D,OpCode.ANY),OpCode(0x45,OpCode.ANY),OpCode(0x4D,OpCode.ANY),OpCode(0x55,OpCode.ANY),
      OpCode(0x5D,OpCode.ANY),OpCode(0x65,OpCode.ANY),OpCode(0x6D,OpCode.ANY),OpCode(0x75,OpCode.ANY),
      OpCode(0xDD,0x75),OpCode(0xFD,0x75)) -> LocationSpec8Bit.register("L"),
    // indirect registers
    List(OpCode(0x7E,OpCode.ANY),OpCode(0x46,OpCode.ANY),OpCode(0x4E,OpCode.ANY),OpCode(0x56,OpCode.ANY),
      OpCode(0x5E,OpCode.ANY),OpCode(0x66,OpCode.ANY),OpCode(0x6E,OpCode.ANY)) -> LocationSpec8Bit.registerAddr("HL"),
    List(OpCode(0x0A,OpCode.ANY)) -> LocationSpec8Bit.registerAddr("BC"),
    List(OpCode(0x1A,OpCode.ANY)) -> LocationSpec8Bit.registerAddr("DE"),
    // indirect registers with offset
    List(OpCode(0xDD,0x7E),OpCode(0xDD,0x46),OpCode(0xDD,0x4E),OpCode(0xDD,0x56),OpCode(0xDD,0x5E),
      OpCode(0xDD,66),OpCode(0xDD,0x6E)) -> LocationSpec8Bit.registerAddrIndirOffset("IX",2),
    List(OpCode(0xFD,0x7E),OpCode(0xFD,0x46),OpCode(0xFD,0x4E),OpCode(0xFD,0x56),OpCode(0xFD,0x5E),
      OpCode(0xFD,66),OpCode(0xFD,0x6E)) -> LocationSpec8Bit.registerAddrIndirOffset("IY",2),
    // immediate address
    List(OpCode(0x3A,OpCode.ANY)) -> LocationSpec8Bit.indirAddress(1),
    // immediate
    List(OpCode(0x3E,OpCode.ANY),OpCode(0x06,OpCode.ANY),OpCode(0x0E,OpCode.ANY),OpCode(0x16,OpCode.ANY),
      OpCode(0x1E,OpCode.ANY),OpCode(0x26,OpCode.ANY),OpCode(0x2E,OpCode.ANY),
      OpCode(0x36,OpCode.ANY)) -> LocationSpec8Bit.registerAddrDirOffset("PC",1),
    List(OpCode(0xDD,0x36),OpCode(0xFD,0x36)) -> LocationSpec8Bit.registerAddrDirOffset("PC",3)
  )

  val sourceLoc:Map[OpCode,LocationSpec8Bit]=Z80Utils.flattenMapOfLists(sourceLocListMap)
  def getSourceLoc(opcode:OpCode):LocationSpec8Bit = sourceLoc.getOrElse(opcode,sourceLoc(OpCode(opcode.main,OpCode.ANY)))

  val destLocListMap:Map[List[OpCode],LocationSpec8Bit]=Map(
    // registers
    List(OpCode(0xED,0x57),OpCode(0xED,0x5F),OpCode(0x7F,OpCode.ANY),OpCode(0x78,OpCode.ANY),OpCode(0x79,OpCode.ANY),
      OpCode(0x7A,OpCode.ANY),OpCode(0x7B,OpCode.ANY),OpCode(0x7C,OpCode.ANY),OpCode(0x7D,OpCode.ANY),
      OpCode(0x7E,OpCode.ANY),OpCode(0x7F,OpCode.ANY),OpCode(0x0A,OpCode.ANY),OpCode(0x1A,OpCode.ANY),OpCode(0xDD,0x7E),
      OpCode(0xFD,0x7E),OpCode(0x3A,OpCode.ANY),OpCode(0x3E,OpCode.ANY)) -> LocationSpec8Bit.register("A"),
    List(OpCode(0x47,OpCode.ANY),OpCode(0x40,OpCode.ANY),OpCode(0x41,OpCode.ANY),OpCode(0x42,OpCode.ANY),
      OpCode(0x43,OpCode.ANY),OpCode(0x44,OpCode.ANY),OpCode(0x45,OpCode.ANY),
      OpCode(0x46,OpCode.ANY),OpCode(0xDD,0x46),OpCode(0xFD,0x46),OpCode(0x06,OpCode.ANY)) ->LocationSpec8Bit.register("B"),
    List(OpCode(0x4F,OpCode.ANY),OpCode(0x48,OpCode.ANY),OpCode(0x49,OpCode.ANY),OpCode(0x4A,OpCode.ANY),
      OpCode(0x4B,OpCode.ANY),OpCode(0x4C,OpCode.ANY),OpCode(0x4D,OpCode.ANY),
      OpCode(0x4E,OpCode.ANY),OpCode(0xDD,0x4E),OpCode(0xFD,0x4E),OpCode(0x0E,OpCode.ANY)) ->LocationSpec8Bit.register("C"),
    List(OpCode(0x57,OpCode.ANY),OpCode(0x50,OpCode.ANY),OpCode(0x51,OpCode.ANY),OpCode(0x52,OpCode.ANY),
      OpCode(0x53,OpCode.ANY),OpCode(0x54,OpCode.ANY),OpCode(0x55,OpCode.ANY),
      OpCode(0x56,OpCode.ANY),OpCode(0xDD,0x56),OpCode(0xFD,0x56),OpCode(0x16,OpCode.ANY)) ->LocationSpec8Bit.register("D"),
    List(OpCode(0x5F,OpCode.ANY),OpCode(0x58,OpCode.ANY),OpCode(0x59,OpCode.ANY),OpCode(0x5A,OpCode.ANY),
      OpCode(0x5B,OpCode.ANY),OpCode(0x5C,OpCode.ANY),OpCode(0x5D,OpCode.ANY),
      OpCode(0x5E,OpCode.ANY),OpCode(0xDD,0x5E),OpCode(0xFD,0x5E),OpCode(0x1E,OpCode.ANY)) ->LocationSpec8Bit.register("E"),
    List(OpCode(0x67,OpCode.ANY),OpCode(0x60,OpCode.ANY),OpCode(0x61,OpCode.ANY),OpCode(0x62,OpCode.ANY),
      OpCode(0x63,OpCode.ANY),OpCode(0x64,OpCode.ANY),OpCode(0x65,OpCode.ANY),
      OpCode(0x66,OpCode.ANY),OpCode(0xDD,0x66),OpCode(0xFD,0x66),OpCode(0x26,OpCode.ANY)) ->LocationSpec8Bit.register("H"),
    List(OpCode(0x6F,OpCode.ANY),OpCode(0x68,OpCode.ANY),OpCode(0x69,OpCode.ANY),OpCode(0x6A,OpCode.ANY),
      OpCode(0x6B,OpCode.ANY),OpCode(0x6C,OpCode.ANY),OpCode(0x6D,OpCode.ANY),
      OpCode(0x6E,OpCode.ANY),OpCode(0xDD,0x6E),OpCode(0xFD,0x6E),OpCode(0x2E,OpCode.ANY)) -> LocationSpec8Bit.register("L"),
    List(OpCode(0xED,0x47))->LocationSpec8Bit.register("I"),
    List(OpCode(0xED,0x4F))->LocationSpec8Bit.register("R"),
    // indirect registers
    List(OpCode(0x77,OpCode.ANY),OpCode(0x70,OpCode.ANY),OpCode(0x71,OpCode.ANY),OpCode(0x72,OpCode.ANY),
      OpCode(0x73,OpCode.ANY),OpCode(0x74,OpCode.ANY),OpCode(0x75,OpCode.ANY),OpCode(0x36,OpCode.ANY))
      -> LocationSpec8Bit.registerAddr("HL"),
    List(OpCode(0x02,OpCode.ANY)) -> LocationSpec8Bit.registerAddr("BC"),
    List(OpCode(0x12,OpCode.ANY)) -> LocationSpec8Bit.registerAddr("DE"),
    // indirect registers with offset
    List(OpCode(0xDD,0x77),OpCode(0xDD,0x70),OpCode(0xDD,0x71),OpCode(0xDD,0x72),OpCode(0xDD,0x73),
      OpCode(0xDD,74),OpCode(0xDD,0x75),OpCode(0xDD,0x36)) -> LocationSpec8Bit.registerAddrIndirOffset("IX",2),
    List(OpCode(0xFD,0x77),OpCode(0xFD,0x70),OpCode(0xFD,0x71),OpCode(0xFD,0x72),OpCode(0xFD,0x73),
      OpCode(0xFD,74),OpCode(0xFD,0x75),OpCode(0xFD,0x36)) -> LocationSpec8Bit.registerAddrIndirOffset("IY",2),
    // immediate address
    List(OpCode(0x32,OpCode.ANY)) -> LocationSpec8Bit.indirAddress(1)
  )

  val destLoc:Map[OpCode,LocationSpec8Bit]=Z80Utils.flattenMapOfLists(destLocListMap)
  def getDestLoc(opcode:OpCode):LocationSpec8Bit = destLoc.getOrElse(opcode,destLoc(OpCode(opcode.main,OpCode.ANY)))


  val instructionSizeListMap:Map[List[OpCode],Int]=Map(
    List(OpCode(0x7F,OpCode.ANY),OpCode(0x4F,OpCode.ANY),OpCode(0x47,OpCode.ANY),OpCode(0x57,OpCode.ANY),
      OpCode(0x5F,OpCode.ANY),OpCode(0x67,OpCode.ANY),OpCode(0x6F,OpCode.ANY),OpCode(0x77,OpCode.ANY),
      OpCode(0x02,OpCode.ANY),OpCode(0x12,OpCode.ANY),
      OpCode(0x78,OpCode.ANY),OpCode(0x40,OpCode.ANY),OpCode(0x48,OpCode.ANY),OpCode(0x50,OpCode.ANY),
      OpCode(0x58,OpCode.ANY),OpCode(0x60,OpCode.ANY),OpCode(0x68,OpCode.ANY),OpCode(0x70,OpCode.ANY),
      OpCode(0x79,OpCode.ANY),OpCode(0x41,OpCode.ANY),OpCode(0x49,OpCode.ANY),OpCode(0x51,OpCode.ANY),
      OpCode(0x59,OpCode.ANY),OpCode(0x61,OpCode.ANY),OpCode(0x69,OpCode.ANY),OpCode(0x71,OpCode.ANY),
      OpCode(0x7A,OpCode.ANY),OpCode(0x42,OpCode.ANY),OpCode(0x4A,OpCode.ANY),OpCode(0x52,OpCode.ANY),
      OpCode(0x5A,OpCode.ANY),OpCode(0x62,OpCode.ANY),OpCode(0x6A,OpCode.ANY),OpCode(0x72,OpCode.ANY),
      OpCode(0x7B,OpCode.ANY),OpCode(0x43,OpCode.ANY),OpCode(0x4B,OpCode.ANY),OpCode(0x53,OpCode.ANY),
      OpCode(0x5B,OpCode.ANY),OpCode(0x63,OpCode.ANY),OpCode(0x6B,OpCode.ANY),OpCode(0x73,OpCode.ANY),
      OpCode(0x7C,OpCode.ANY),OpCode(0x44,OpCode.ANY),OpCode(0x4C,OpCode.ANY),OpCode(0x54,OpCode.ANY),
      OpCode(0x5C,OpCode.ANY),OpCode(0x64,OpCode.ANY),OpCode(0x6C,OpCode.ANY),OpCode(0x74,OpCode.ANY),
      OpCode(0x7D,OpCode.ANY),OpCode(0x45,OpCode.ANY),OpCode(0x4D,OpCode.ANY),OpCode(0x55,OpCode.ANY),
      OpCode(0x5D,OpCode.ANY),OpCode(0x65,OpCode.ANY),OpCode(0x6D,OpCode.ANY),OpCode(0x75,OpCode.ANY),
      OpCode(0x0A,OpCode.ANY),OpCode(0x1A,OpCode.ANY),
      OpCode(0x7E,OpCode.ANY),OpCode(0x46,OpCode.ANY),OpCode(0x4E,OpCode.ANY),OpCode(0x56,OpCode.ANY),
      OpCode(0x5E,OpCode.ANY),OpCode(0x66,OpCode.ANY),OpCode(0x6E,OpCode.ANY),
      OpCode(0x1A,OpCode.ANY)) -> 1,
    List(OpCode(0xED,0x57),OpCode(0xED,0x5F),OpCode(0xED,0x47),OpCode(0xED,0x4F),
      OpCode(0x3E,OpCode.ANY),OpCode(0x06,OpCode.ANY),OpCode(0x0E,OpCode.ANY),OpCode(0x16,OpCode.ANY),
      OpCode(0x1E,OpCode.ANY),OpCode(0x26,OpCode.ANY),OpCode(0x2E,OpCode.ANY), OpCode(0x36,OpCode.ANY)) -> 2,
    List(OpCode(0xDD,0x77),OpCode(0xFD,0x77),OpCode(0x32,OpCode.ANY),OpCode(0xDD,0x70),OpCode(0xFD,0x70),
      OpCode(0xDD,0x71),OpCode(0xFD,0x71),OpCode(0xDD,0x72),OpCode(0xFD,0x72),
      OpCode(0xDD,0x73),OpCode(0xFD,0x73),OpCode(0xDD,0x74),OpCode(0xFD,0x74),
      OpCode(0xDD,0x75),OpCode(0xFD,0x75),
      OpCode(0xDD,0x7E),OpCode(0xDD,0x46),OpCode(0xDD,0x4E),OpCode(0xDD,0x56),OpCode(0xDD,0x5E),
      OpCode(0xDD,66),OpCode(0xDD,0x6E),
      OpCode(0xFD,0x7E),OpCode(0xFD,0x46),OpCode(0xFD,0x4E),OpCode(0xFD,0x56),OpCode(0xFD,0x5E),
      OpCode(0xFD,66),OpCode(0xFD,0x6E),OpCode(0x3A,OpCode.ANY)) ->3,
    List(OpCode(0xDD,0x36),OpCode(0xFD,0x36)) -> 4
  )

  val instructionSize:Map[OpCode,Int]=Z80Utils.flattenMapOfLists(instructionSizeListMap)
  val getInstructionSize: OpCode=>Int = opcode => instructionSize.getOrElse(opcode,instructionSize(OpCode(opcode.main,OpCode.ANY)))

  val isLoad8Bit: OpCode=>Boolean = opcode =>
    instructionSize.contains(opcode) || instructionSize.contains(OpCode(opcode.main,OpCode.ANY))
}

case class LocationSpec8Bit(reg:String, immediate:Int, offsetPC:Int, addressReg:String, directOffset:Int, indirectOffset:Int) {
  override def toString: String =
    this match {
      case LocationSpec8Bit(r,_,_,_,_,_) if r!="" => f"$r"
      case LocationSpec8Bit(_,i,_,_,_,_) if i!=OpCode.ANY => f"0x$i%02X"
      case LocationSpec8Bit(_,_,pco,_,_,_) if pco!=OpCode.ANY => f"(PC+0x${pco+1}%02X,0x$pco%02X)"
      case LocationSpec8Bit(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO, indirO) match {
          case (OpCode.ANY,OpCode.ANY) => f"($r)"
          case (o,OpCode.ANY) => f"($r+0x$o%02X)"
          case (OpCode.ANY,o) => f"($r+(PC+0x$o%02X))"
        }
    }
}

object LocationSpec8Bit {
  def register(r:String):LocationSpec8Bit=LocationSpec8Bit(r,OpCode.ANY,OpCode.ANY,"",OpCode.ANY,OpCode.ANY)
  def immediate(i:Int):LocationSpec8Bit=LocationSpec8Bit("",i,OpCode.ANY,"",OpCode.ANY,OpCode.ANY)
  def indirAddress(a:Int):LocationSpec8Bit=LocationSpec8Bit("",OpCode.ANY,a,"",OpCode.ANY,OpCode.ANY)
  def registerAddr(r:String):LocationSpec8Bit=LocationSpec8Bit("",OpCode.ANY,OpCode.ANY,r,OpCode.ANY,OpCode.ANY)
  def registerAddrDirOffset(r:String,o:Int):LocationSpec8Bit=LocationSpec8Bit("",OpCode.ANY,OpCode.ANY,r,o,OpCode.ANY)
  def registerAddrIndirOffset(r:String,o:Int):LocationSpec8Bit=LocationSpec8Bit("",OpCode.ANY,OpCode.ANY,r,OpCode.ANY,o)
}
