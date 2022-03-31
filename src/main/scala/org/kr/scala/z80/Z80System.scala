package org.kr.scala.z80

class Z80System(val memoryController: MemoryController, val registerController: RegisterController) {
  def step:Z80System= {
    val PC = registerController.get("PC")
    val oper = memoryController.get(PC)
    val oper1 = memoryController.get(PC,1)
    (oper,oper1) match {
      //NOP
      case (0,_) =>
        returnNewNOP
      // LD r,r1 : 01xxxyyy, yyy->xxx - register code
      case (x,_) if (x & 0xC0)==0x40 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) && List(7,0,1,2,3,4,5).contains(x & 7) =>
        val regTo:Int=>String=getRegSymbolBit35
        val valueFrom:Int=>Int=getRegValue(_,getRegSymbolBit02)
        returnNewReg(newRegister(regTo(x),valueFrom(x)),1)
      // LD r,n : 00xxx110, xxx - register code
      case (x,_) if (x & 0xC7)==0x06 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) =>
        val regTo:Int=>String=getRegSymbolBit35
        val valueFrom:Int=>Int=getMemFromPC
        returnNewReg(newRegister(regTo(x),valueFrom(1)),2)
      // LD r,(HL) : 01xxx110, xxx - register code
      case (x,_) if (x & 0xC7)==0x46 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) =>
        val regTo:Int=>String=getRegSymbolBit35
        val valueFrom:Int=>Int=getMemFromReg("HL",_)
        returnNewReg(newRegister(regTo(x),valueFrom(0)),1)
      // LD (HL),r
      case (x,_) if (x & 0xF0)==0x70 && List(7,0,1,2,3,4,5).contains(x & 7) =>
        val memTo:Int=>Int=getAddressFromReg("HL",_)
        val valueFrom:Int=>Int=getRegValue(_,getRegSymbolBit02)
        returnNewMem(newMemory(memTo(0),valueFrom(x)),1)
     // LD (HL),n
      case (x,_) if x==0x36 =>
        val memTo:Int=>Int=getAddressFromReg("HL",_)
        val valueFrom:Int=>Int=getMemFromReg("PC",_)
        returnNewMem(newMemory(memTo(0),valueFrom(1)),2)
      // LD r,(IX+d) | LD r,(IY+d)
      case (y,x) if (y==0xDD || y==0xFD) && (x & 0xC7)==0x46 && List(7,0,1,2,3,4,5).contains((x>>3) & 7) =>
        val regTo:Int=>String=getRegSymbolBit35
        val valueFrom:Int=>Int=getMemFromReg(if(y==0xDD) "IX" else "IY",_)
        val offsetD=getMemFromPC(2)
        returnNewReg(newRegister(regTo(x),valueFrom(offsetD)),3)
      // LD (IX+d),r | LD (IY+d),r
      case (y,x) if (y==0xDD || y==0xFD) && (x & 0xF8)==0x70 && List(7,0,1,2,3,4,5).contains(x & 7) =>
        val memTo:Int=>Int=getAddressFromReg(if(y==0xDD) "IX" else "IY",_)
        val offsetD=getMemFromPC(2)
        val valueFrom:Int=>Int=getRegValue(_,getRegSymbolBit02)
        returnNewMem(newMemory(memTo(offsetD),valueFrom(x)),3)

      // operations not implemented or invalid
      case (_, _) => throw new UnknownOperationException(f"Unknown operation $oper at $PC")
    }
  }

  def getRegSymbolBit02(x:Int):String = Register.getRegCode(x & 0x07)
  def getRegSymbolBit35(x:Int):String = Register.getRegCode((x >> 3) & 0x07)
  def getRegValue(opcode:Int,opcodeToSymbol:Int=>String):Int = registerController.get(opcodeToSymbol(opcode))
  def getMemFromPC(offset:Int):Int = getMemFromReg("PC",offset)
  def getAddressFromReg(symbol:String,offset:Int):Int=
    offset+(symbol match {
      case "HL" => makeWord(registerController.get("H"), registerController.get("L"))
      case otherSymbol => registerController.get(otherSymbol)
    })
  def getMemFromReg(symbol:String,offset:Int):Int =
    memoryController.get(getAddressFromReg(symbol,offset))
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
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
  def apply(source:Z80System):Z80System=new Z80System(source.memoryController,source.registerController)
}

class UnknownOperationException(message : String) extends Exception(message) {}
