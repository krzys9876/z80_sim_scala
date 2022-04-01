package org.kr.scala.z80

class Z80System(val memoryController: MemoryController, val registerController: RegisterController) {
  def step:Z80System= {
    val PC = registerController.get("PC")
    val oper = memoryController.get(PC)
    val oper1 = memoryController.get(PC,1)
    (oper,oper1) match {
      //NOP
      case (0,_) => returnNewNOP
      // LD r,r1 : 01xxxyyy, yyy->xxx - register code
      case (x,_) if (x & 0xC0)==0x40 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) && List(7,0,1,2,3,4,5).contains(x & 7) =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewReg(newRegister(Load8Bit.getDestReg(OpCode(oper, oper1)),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD r,n : 00xxx110, xxx - register code
      case (x,_) if (x & 0xC7)==0x06 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewReg(newRegister(Load8Bit.getDestReg(OpCode(oper, oper1)),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD r,(HL) : 01xxx110, xxx - register code
      case (x,_) if (x & 0xC7)==0x46 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewReg(newRegister(Load8Bit.getDestReg(OpCode(oper, oper1)),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD (HL),r
      case (x,_) if (x & 0xF0)==0x70 && List(7,0,1,2,3,4,5).contains(x & 7) =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        val memTo:Int=>Int=getAddressFromReg("HL",_)
        returnNewMem(newMemory(memTo(0),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
     // LD (HL),n
      case (x,_) if x==0x36 =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        val memTo:Int=>Int=getAddressFromReg("HL",_)
        returnNewMem(newMemory(memTo(0),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD r,(IX+d) | LD r,(IY+d)
      case (y,x) if (y==0xDD || y==0xFD) && (x & 0xC7)==0x46 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        println(sourceLoc)
        returnNewReg(newRegister(Load8Bit.getDestReg(OpCode(oper, oper1)),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD (IX+d),r | LD (IY+d),r
      case (y,x) if (y==0xDD || y==0xFD) && (x & 0xF8)==0x70 && List(7,0,1,2,3,4,5).contains(x & 7) =>
        val memTo:Int=>Int=getAddressFromReg(if(y==0xDD) "IX" else "IY",_)
        val offsetD=getMemFromPC(2)
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewMem(newMemory(memTo(offsetD),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD (IX+d),n | LD (IY+d),n
      case (y,x) if (y==0xDD || y==0xFD) && (x == 0x36) =>
        val memTo:Int=>Int=getAddressFromReg(if(y==0xDD) "IX" else "IY",_)
        val offsetD=getMemFromPC(2)
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewMem(newMemory(memTo(offsetD),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD A,(BC) | LD A,(DE)
      case (x,_) if x == 0x0A || x==0x1A =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewReg(newRegister(Load8Bit.getDestReg(OpCode(oper, oper1)),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD (BC),A | LD (DE),A
      case (x,_) if x == 0x02 || x==0x12 =>
        val memTo:Int=>Int=getAddressFromReg(if(x==0x02) "BC" else "DE",_)
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewMem(newMemory(memTo(0),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD A,(nn)
      case (x,_) if x == 0x3A =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewReg(newRegister(Load8Bit.getDestReg(OpCode(oper, oper1)),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD (nn),A
      case (x,_) if x == 0x32 =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewMem(newMemory(makeWord(getMemFromPC(2),getMemFromPC(1)),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))
      // LD A<->I/R
      case (y,x) if y==0xED && (x ==0x57 || x ==0x5F || x ==0x47 || x ==0x4F) =>
        val sourceLoc=Load8Bit.getSourceLoc(OpCode(oper, oper1))
        returnNewReg(newRegister(Load8Bit.getDestReg(OpCode(oper, oper1)),getValueFromLocation(sourceLoc)),Load8Bit.getInstructionSize(OpCode(oper,oper1)))

      // operations not implemented or invalid
      case (_, _) => throw new UnknownOperationException(f"Unknown operation $oper at $PC")
    }
  }

  def getRegSymbolBit02(x:Int):String = Register.getRegCode(x & 0x07)
  def getRegSymbolBit35(x:Int):String = Register.getRegCode((x >> 3) & 0x07)
  def getRegValue(opcode:Int,opcodeToSymbol:Int=>String):Int = getRegValue(opcodeToSymbol(opcode))
  def getRegValue(symbol:String):Int=registerController.get(symbol)
  def getMemFromPC(offset:Int):Int = getMemFromReg("PC",offset)
  def getAddressFromReg(symbol:String,offset:Int):Int=
    offset+(symbol match {
      case "HL" => makeWord(registerController.get("H"), registerController.get("L"))
      case "BC" => makeWord(registerController.get("B"), registerController.get("C"))
      case "DE" => makeWord(registerController.get("D"), registerController.get("E"))
      case otherSymbol => registerController.get(otherSymbol)
    })
  def getMemFromReg(symbol:String,offset:Int):Int = getMem(getAddressFromReg(symbol,offset))
  def getMem(address:Int):Int = memoryController.get(address)
  def makeWord(valH:Int,valL:Int):Int=valH*0x100+valL

  private def newRegister(symbol:String,value:Int):RegisterController=
    RegisterController((registerController >>= RegisterController.set(symbol,value)).get)
  private def newMemory(address:Int,value:Int):MemoryController=
    MemoryController((memoryController >>= MemoryController.poke(address,value)).get)

  private def returnNewNOP:Z80System = returnNewReg(registerController,1)
  private def returnNewReg(newRegister:BaseStateMonad[Register], forwardPC:Int):Z80System =
    new Z80System(memoryController,RegisterController(newRegister.get.movePC(forwardPC)))
  private def returnNewMem(newMemory:BaseStateMonad[Memory], forwardPC:Int):Z80System =
    new Z80System(MemoryController(newMemory.get),RegisterController(registerController.get.movePC(forwardPC)))

  private def getValueFromLocation(loc:LocationSpec8Bit):Int = {
    loc match {
      case LocationSpec8Bit(r,_,_,_,_,_) if r!="" => getRegValue(r)
      case LocationSpec8Bit(_,i,_,_,_,_) if i!=OpCode.ANY => i
      case LocationSpec8Bit(_,_,pco,_,_,_) if pco!=OpCode.ANY => getMem(makeWord(getMemFromPC(pco+1),getMemFromPC(pco)))
      case LocationSpec8Bit(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO,indirO) match {
          case (OpCode.ANY,OpCode.ANY) => getMemFromReg(r,0)
          case (o,OpCode.ANY) => getMemFromReg(r,o)
          case (OpCode.ANY,o) => getMemFromReg(r,getMemFromPC(o))
        }
    }
  }
/*
  private def saveValueToLocation(loc:LocationSpec8Bit,value:Int):Z80System = {
    loc match {
      case LocationSpec8Bit(r,_,_,_,_,_) if r!="" => returnNewReg(r)
      case LocationSpec8Bit(_,i,_,_,_,_) if i!=OpCode.ANY => i
      case LocationSpec8Bit(_,_,pco,_,_,_) if pco!=OpCode.ANY => getMem(makeWord(getMemFromPC(pco+1),getMemFromPC(pco)))
      case LocationSpec8Bit(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO,indirO) match {
          case (OpCode.ANY,OpCode.ANY) => getMemFromReg(r,0)
          case (o,OpCode.ANY) => getMemFromReg(r,o)
          case (OpCode.ANY,o) => getMemFromReg(r,getMemFromPC(o))
        }
    }
  }
*/
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
  def apply(source:Z80System):Z80System=new Z80System(source.memoryController,source.registerController)
}

class UnknownOperationException(message : String) extends Exception(message) {}

case class OpCode(main:Int,supp:Int) {
  override def toString: String = f"OpCode($main,${if(supp==OpCode.ANY) "ANY" else supp})"
}

object OpCode {
  val ANY:Int = -1
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

object Load8Bit {
  // Z80 manual page 42
  val destRegListMap:Map[List[OpCode],String]=Map(
    List(OpCode(0xED,0x57),OpCode(0xED,0x5F),OpCode(0x7F,OpCode.ANY),OpCode(0x78,OpCode.ANY),OpCode(0x79,OpCode.ANY),
      OpCode(0x7A,OpCode.ANY),OpCode(0x7B,OpCode.ANY),OpCode(0x7C,OpCode.ANY),OpCode(0x7D,OpCode.ANY),
      OpCode(0x7E,OpCode.ANY),OpCode(0x7F,OpCode.ANY),OpCode(0x0A,OpCode.ANY),OpCode(0x1A,OpCode.ANY),OpCode(0xDD,0x7E),
      OpCode(0xFD,0x7E),OpCode(0x3A,OpCode.ANY),OpCode(0x3E,OpCode.ANY)) -> "A",
    List(OpCode(0x47,OpCode.ANY),OpCode(0x40,OpCode.ANY),OpCode(0x41,OpCode.ANY),OpCode(0x42,OpCode.ANY),
      OpCode(0x43,OpCode.ANY),OpCode(0x44,OpCode.ANY),OpCode(0x45,OpCode.ANY),
      OpCode(0x46,OpCode.ANY),OpCode(0xDD,0x46),OpCode(0xFD,0x46),OpCode(0x06,OpCode.ANY)) ->"B",
    List(OpCode(0x4F,OpCode.ANY),OpCode(0x48,OpCode.ANY),OpCode(0x49,OpCode.ANY),OpCode(0x4A,OpCode.ANY),
      OpCode(0x4B,OpCode.ANY),OpCode(0x4C,OpCode.ANY),OpCode(0x4D,OpCode.ANY),
      OpCode(0x4E,OpCode.ANY),OpCode(0xDD,0x4E),OpCode(0xFD,0x4E),OpCode(0x0E,OpCode.ANY)) ->"C",
    List(OpCode(0x57,OpCode.ANY),OpCode(0x50,OpCode.ANY),OpCode(0x51,OpCode.ANY),OpCode(0x52,OpCode.ANY),
      OpCode(0x53,OpCode.ANY),OpCode(0x54,OpCode.ANY),OpCode(0x55,OpCode.ANY),
      OpCode(0x56,OpCode.ANY),OpCode(0xDD,0x56),OpCode(0xFD,0x56),OpCode(0x16,OpCode.ANY)) ->"D",
    List(OpCode(0x5F,OpCode.ANY),OpCode(0x58,OpCode.ANY),OpCode(0x59,OpCode.ANY),OpCode(0x5A,OpCode.ANY),
      OpCode(0x5B,OpCode.ANY),OpCode(0x5C,OpCode.ANY),OpCode(0x5D,OpCode.ANY),
      OpCode(0x5E,OpCode.ANY),OpCode(0xDD,0x5E),OpCode(0xFD,0x5E),OpCode(0x1E,OpCode.ANY)) ->"E",
    List(OpCode(0x67,OpCode.ANY),OpCode(0x60,OpCode.ANY),OpCode(0x61,OpCode.ANY),OpCode(0x62,OpCode.ANY),
      OpCode(0x63,OpCode.ANY),OpCode(0x64,OpCode.ANY),OpCode(0x65,OpCode.ANY),
      OpCode(0x66,OpCode.ANY),OpCode(0xDD,0x66),OpCode(0xFD,0x66),OpCode(0x26,OpCode.ANY)) ->"H",
    List(OpCode(0x6F,OpCode.ANY),OpCode(0x68,OpCode.ANY),OpCode(0x69,OpCode.ANY),OpCode(0x6A,OpCode.ANY),
      OpCode(0x6B,OpCode.ANY),OpCode(0x6C,OpCode.ANY),OpCode(0x6D,OpCode.ANY),
      OpCode(0x6E,OpCode.ANY),OpCode(0xDD,0x6E),OpCode(0xFD,0x6E),OpCode(0x2E,OpCode.ANY)) ->"L",
    List(OpCode(0xED,0x47))->"I",
    List(OpCode(0xED,0x4F))->"R"
  )
  val destReg:Map[OpCode,String]=destRegListMap.map(entry=>entry._1.flatMap(opcode=>Map(opcode->entry._2))).flatten.toMap
  def getDestReg(opcode:OpCode):String = destReg.getOrElse(opcode,destReg(OpCode(opcode.main,OpCode.ANY)))

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

  val sourceLoc:Map[OpCode,LocationSpec8Bit]=sourceLocListMap.map(entry=>entry._1.flatMap(opcode=>Map(opcode->entry._2))).flatten.toMap
  def getSourceLoc(opcode:OpCode):LocationSpec8Bit = sourceLoc.getOrElse(opcode,sourceLoc(OpCode(opcode.main,OpCode.ANY)))

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

  val instructionSize:Map[OpCode,Int]=instructionSizeListMap.map(entry=>entry._1.flatMap(opcode=>Map(opcode->entry._2))).flatten.toMap
  def getInstructionSize(opcode:OpCode):Int = instructionSize.getOrElse(opcode,instructionSize(OpCode(opcode.main,OpCode.ANY)))

}