package org.kr.scala.z80

object Load8Bit {
  // Z80 manual page 42
  val sourceLocListMap:Map[List[OpCode],LoadLocation]=Map(
    // registers
    List(OpCode(0xED,0x57)) -> LoadLocation.register("I"),
    List(OpCode(0xED,0x5F)) -> LoadLocation.register("R"),
    List(OpCode(0x7F,OpCode.ANY),OpCode(0x4F,OpCode.ANY),OpCode(0x47,OpCode.ANY),OpCode(0x57,OpCode.ANY),
      OpCode(0x5F,OpCode.ANY),OpCode(0x67,OpCode.ANY),OpCode(0x6F,OpCode.ANY),OpCode(0x77,OpCode.ANY),
      OpCode(0x02,OpCode.ANY),OpCode(0x12,OpCode.ANY),OpCode(0xDD,0x77),OpCode(0xFD,0x77),
      OpCode(0x32,OpCode.ANY),OpCode(0xED,0x47),OpCode(0xED,0x4F)) -> LoadLocation.register("A"),
    List(OpCode(0x78,OpCode.ANY),OpCode(0x40,OpCode.ANY),OpCode(0x48,OpCode.ANY),OpCode(0x50,OpCode.ANY),
      OpCode(0x58,OpCode.ANY),OpCode(0x60,OpCode.ANY),OpCode(0x68,OpCode.ANY),OpCode(0x70,OpCode.ANY),
      OpCode(0xDD,0x70),OpCode(0xFD,0x70)) -> LoadLocation.register("B"),
    List(OpCode(0x79,OpCode.ANY),OpCode(0x41,OpCode.ANY),OpCode(0x49,OpCode.ANY),OpCode(0x51,OpCode.ANY),
      OpCode(0x59,OpCode.ANY),OpCode(0x61,OpCode.ANY),OpCode(0x69,OpCode.ANY),OpCode(0x71,OpCode.ANY),
      OpCode(0xDD,0x71),OpCode(0xFD,0x71)) -> LoadLocation.register("C"),
    List(OpCode(0x7A,OpCode.ANY),OpCode(0x42,OpCode.ANY),OpCode(0x4A,OpCode.ANY),OpCode(0x52,OpCode.ANY),
      OpCode(0x5A,OpCode.ANY),OpCode(0x62,OpCode.ANY),OpCode(0x6A,OpCode.ANY),OpCode(0x72,OpCode.ANY),
      OpCode(0xDD,0x72),OpCode(0xFD,0x72)) -> LoadLocation.register("D"),
    List(OpCode(0x7B,OpCode.ANY),OpCode(0x43,OpCode.ANY),OpCode(0x4B,OpCode.ANY),OpCode(0x53,OpCode.ANY),
      OpCode(0x5B,OpCode.ANY),OpCode(0x63,OpCode.ANY),OpCode(0x6B,OpCode.ANY),OpCode(0x73,OpCode.ANY),
      OpCode(0xDD,0x73),OpCode(0xFD,0x73)) -> LoadLocation.register("E"),
    List(OpCode(0x7C,OpCode.ANY),OpCode(0x44,OpCode.ANY),OpCode(0x4C,OpCode.ANY),OpCode(0x54,OpCode.ANY),
      OpCode(0x5C,OpCode.ANY),OpCode(0x64,OpCode.ANY),OpCode(0x6C,OpCode.ANY),OpCode(0x74,OpCode.ANY),
      OpCode(0xDD,0x74),OpCode(0xFD,0x74)) -> LoadLocation.register("H"),
    List(OpCode(0x7D,OpCode.ANY),OpCode(0x45,OpCode.ANY),OpCode(0x4D,OpCode.ANY),OpCode(0x55,OpCode.ANY),
      OpCode(0x5D,OpCode.ANY),OpCode(0x65,OpCode.ANY),OpCode(0x6D,OpCode.ANY),OpCode(0x75,OpCode.ANY),
      OpCode(0xDD,0x75),OpCode(0xFD,0x75)) -> LoadLocation.register("L"),
    // indirect registers
    List(OpCode(0x7E,OpCode.ANY),OpCode(0x46,OpCode.ANY),OpCode(0x4E,OpCode.ANY),OpCode(0x56,OpCode.ANY),
      OpCode(0x5E,OpCode.ANY),OpCode(0x66,OpCode.ANY),OpCode(0x6E,OpCode.ANY)) -> LoadLocation.registerAddr("HL"),
    List(OpCode(0x0A,OpCode.ANY)) -> LoadLocation.registerAddr("BC"),
    List(OpCode(0x1A,OpCode.ANY)) -> LoadLocation.registerAddr("DE"),
    // indirect registers with offset
    List(OpCode(0xDD,0x7E),OpCode(0xDD,0x46),OpCode(0xDD,0x4E),OpCode(0xDD,0x56),OpCode(0xDD,0x5E),
      OpCode(0xDD,66),OpCode(0xDD,0x6E)) -> LoadLocation.registerAddrIndirOffset("IX",2),
    List(OpCode(0xFD,0x7E),OpCode(0xFD,0x46),OpCode(0xFD,0x4E),OpCode(0xFD,0x56),OpCode(0xFD,0x5E),
      OpCode(0xFD,66),OpCode(0xFD,0x6E)) -> LoadLocation.registerAddrIndirOffset("IY",2),
    // immediate address
    List(OpCode(0x3A,OpCode.ANY)) -> LoadLocation.indirAddress(1),
    // immediate
    List(OpCode(0x3E,OpCode.ANY),OpCode(0x06,OpCode.ANY),OpCode(0x0E,OpCode.ANY),OpCode(0x16,OpCode.ANY),
      OpCode(0x1E,OpCode.ANY),OpCode(0x26,OpCode.ANY),OpCode(0x2E,OpCode.ANY),
      OpCode(0x36,OpCode.ANY)) -> LoadLocation.registerAddrDirOffset("PC",1),
    List(OpCode(0xDD,0x36),OpCode(0xFD,0x36)) -> LoadLocation.registerAddrDirOffset("PC",3)
  )

  val sourceLoc:Map[OpCode,LoadLocation]=Z80Utils.flattenMapOfLists(sourceLocListMap)
  def getSourceLoc(opcode:OpCode):LoadLocation = sourceLoc.getOrElse(opcode,sourceLoc(OpCode(opcode.main,OpCode.ANY)))

  val destLocListMap:Map[List[OpCode],LoadLocation]=Map(
    // registers
    List(OpCode(0xED,0x57),OpCode(0xED,0x5F),OpCode(0x7F,OpCode.ANY),OpCode(0x78,OpCode.ANY),OpCode(0x79,OpCode.ANY),
      OpCode(0x7A,OpCode.ANY),OpCode(0x7B,OpCode.ANY),OpCode(0x7C,OpCode.ANY),OpCode(0x7D,OpCode.ANY),
      OpCode(0x7E,OpCode.ANY),OpCode(0x7F,OpCode.ANY),OpCode(0x0A,OpCode.ANY),OpCode(0x1A,OpCode.ANY),OpCode(0xDD,0x7E),
      OpCode(0xFD,0x7E),OpCode(0x3A,OpCode.ANY),OpCode(0x3E,OpCode.ANY)) -> LoadLocation.register("A"),
    List(OpCode(0x47,OpCode.ANY),OpCode(0x40,OpCode.ANY),OpCode(0x41,OpCode.ANY),OpCode(0x42,OpCode.ANY),
      OpCode(0x43,OpCode.ANY),OpCode(0x44,OpCode.ANY),OpCode(0x45,OpCode.ANY),
      OpCode(0x46,OpCode.ANY),OpCode(0xDD,0x46),OpCode(0xFD,0x46),OpCode(0x06,OpCode.ANY)) ->LoadLocation.register("B"),
    List(OpCode(0x4F,OpCode.ANY),OpCode(0x48,OpCode.ANY),OpCode(0x49,OpCode.ANY),OpCode(0x4A,OpCode.ANY),
      OpCode(0x4B,OpCode.ANY),OpCode(0x4C,OpCode.ANY),OpCode(0x4D,OpCode.ANY),
      OpCode(0x4E,OpCode.ANY),OpCode(0xDD,0x4E),OpCode(0xFD,0x4E),OpCode(0x0E,OpCode.ANY)) ->LoadLocation.register("C"),
    List(OpCode(0x57,OpCode.ANY),OpCode(0x50,OpCode.ANY),OpCode(0x51,OpCode.ANY),OpCode(0x52,OpCode.ANY),
      OpCode(0x53,OpCode.ANY),OpCode(0x54,OpCode.ANY),OpCode(0x55,OpCode.ANY),
      OpCode(0x56,OpCode.ANY),OpCode(0xDD,0x56),OpCode(0xFD,0x56),OpCode(0x16,OpCode.ANY)) ->LoadLocation.register("D"),
    List(OpCode(0x5F,OpCode.ANY),OpCode(0x58,OpCode.ANY),OpCode(0x59,OpCode.ANY),OpCode(0x5A,OpCode.ANY),
      OpCode(0x5B,OpCode.ANY),OpCode(0x5C,OpCode.ANY),OpCode(0x5D,OpCode.ANY),
      OpCode(0x5E,OpCode.ANY),OpCode(0xDD,0x5E),OpCode(0xFD,0x5E),OpCode(0x1E,OpCode.ANY)) ->LoadLocation.register("E"),
    List(OpCode(0x67,OpCode.ANY),OpCode(0x60,OpCode.ANY),OpCode(0x61,OpCode.ANY),OpCode(0x62,OpCode.ANY),
      OpCode(0x63,OpCode.ANY),OpCode(0x64,OpCode.ANY),OpCode(0x65,OpCode.ANY),
      OpCode(0x66,OpCode.ANY),OpCode(0xDD,0x66),OpCode(0xFD,0x66),OpCode(0x26,OpCode.ANY)) ->LoadLocation.register("H"),
    List(OpCode(0x6F,OpCode.ANY),OpCode(0x68,OpCode.ANY),OpCode(0x69,OpCode.ANY),OpCode(0x6A,OpCode.ANY),
      OpCode(0x6B,OpCode.ANY),OpCode(0x6C,OpCode.ANY),OpCode(0x6D,OpCode.ANY),
      OpCode(0x6E,OpCode.ANY),OpCode(0xDD,0x6E),OpCode(0xFD,0x6E),OpCode(0x2E,OpCode.ANY)) -> LoadLocation.register("L"),
    List(OpCode(0xED,0x47))->LoadLocation.register("I"),
    List(OpCode(0xED,0x4F))->LoadLocation.register("R"),
    // indirect registers
    List(OpCode(0x77,OpCode.ANY),OpCode(0x70,OpCode.ANY),OpCode(0x71,OpCode.ANY),OpCode(0x72,OpCode.ANY),
      OpCode(0x73,OpCode.ANY),OpCode(0x74,OpCode.ANY),OpCode(0x75,OpCode.ANY),OpCode(0x36,OpCode.ANY))
      -> LoadLocation.registerAddr("HL"),
    List(OpCode(0x02,OpCode.ANY)) -> LoadLocation.registerAddr("BC"),
    List(OpCode(0x12,OpCode.ANY)) -> LoadLocation.registerAddr("DE"),
    // indirect registers with offset
    List(OpCode(0xDD,0x77),OpCode(0xDD,0x70),OpCode(0xDD,0x71),OpCode(0xDD,0x72),OpCode(0xDD,0x73),
      OpCode(0xDD,74),OpCode(0xDD,0x75),OpCode(0xDD,0x36)) -> LoadLocation.registerAddrIndirOffset("IX",2),
    List(OpCode(0xFD,0x77),OpCode(0xFD,0x70),OpCode(0xFD,0x71),OpCode(0xFD,0x72),OpCode(0xFD,0x73),
      OpCode(0xFD,74),OpCode(0xFD,0x75),OpCode(0xFD,0x36)) -> LoadLocation.registerAddrIndirOffset("IY",2),
    // immediate address
    List(OpCode(0x32,OpCode.ANY)) -> LoadLocation.indirAddress(1)
  )

  val destLoc:Map[OpCode,LoadLocation]=Z80Utils.flattenMapOfLists(destLocListMap)
  def getDestLoc(opcode:OpCode):LoadLocation = destLoc.getOrElse(opcode,destLoc(OpCode(opcode.main,OpCode.ANY)))


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
