package org.kr.scala.z80

class Z80System(val memoryController: MemoryController, val registerController: RegisterController) {
  def step:Z80System= {
    val PC = registerController.get("PC")
    val oper = memoryController.get(PC)
    val oper1 = memoryController.get(PC,1)
    val opcode = OpCode(oper,oper1)

    opcode.opType match {
      case OpType.Nop => handleNop
      case OpType.Load8Bit => handleLoad8Bit(opcode)
      case OpType.Load16Bit => handleLoad16Bit(opcode)
      case OpType.Unknown => throw new UnknownOperationException(f"Unknown operation $oper at $PC")
    }
  }

  private def getRegValue(symbol:String):Int={
    symbol match {
      case "AF" | "BC" | "DE" | "HL" =>
        makeWord(registerController.get(symbol.substring(0,1)), registerController.get(symbol.substring(1,2)))
      case _ => registerController.get(symbol)
    }
  }
  private def getMemFromPC(offset:Int):Int = getMemFromReg("PC",offset)
  private def getAddressFromReg(symbol:String,offset:Int):Int= getRegValue(symbol)+offset
  private def getMemFromReg(symbol:String,offset:Int):Int = getMem(getAddressFromReg(symbol,offset))
  private def getMem(address:Int):Int = memoryController.get(address)
  private def makeWord(valH:Int,valL:Int):Int=valH*0x100+valL

  private def newRegister(symbol:String,value:Int):RegisterController= {
    val newReg=symbol match {
      case "AF" | "BC" | "DE" | "HL" =>
        registerController >>=
          RegisterController.set(symbol.substring(0,1),Z80Utils.getH(value)) >>=
            RegisterController.set(symbol.substring(1,2),Z80Utils.getL(value))
      case _ => registerController >>= RegisterController.set(symbol,value)
    }
    RegisterController(newReg.get)
  }

  private def newMemory(address:Int,value:Int):MemoryController=
    MemoryController((memoryController >>= MemoryController.poke(address,value)).get)

  private def returnNew(newMemory:BaseStateMonad[Memory],newRegister:BaseStateMonad[Register], forwardPC:Int):Z80System =
    new Z80System(MemoryController(newMemory.get),RegisterController(newRegister.get.movePC(forwardPC)))
  private def returnNewReg(newRegister:BaseStateMonad[Register], forwardPC:Int):Z80System =
    new Z80System(memoryController,RegisterController(newRegister.get.movePC(forwardPC)))
  private def returnNewMem(newMemory:BaseStateMonad[Memory], forwardPC:Int):Z80System =
    new Z80System(MemoryController(newMemory.get),RegisterController(registerController.get.movePC(forwardPC)))

  private def handleLoad8Bit(opcode:OpCode):Z80System = {
    val value=getValueFromLocation(Load8Bit.sourceLoc.find(opcode))
    val destLoc=Load8Bit.destLoc.find(opcode)
    val instrSize=Load8Bit.instSize.find(opcode)
    handleLoad8Bit(destLoc,value,instrSize)
  }

  private def handleNop:Z80System = returnNewReg(registerController,1)

  private def handleLoad8Bit(dest:LoadLocation, value:Int, forwardPC:Int):Z80System= {
    dest match {
      case LoadLocation(r,_,_,_,_,_) if r!="" =>
        returnNewReg(newRegister(dest.reg,value),forwardPC)
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY =>
        returnNewMem(newMemory(makeWord(getMemFromPC(pco+1),getMemFromPC(pco)),value),forwardPC)
      case LoadLocation(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO,indirO) match {
          case (OpCode.ANY,OpCode.ANY) => getAddressFromReg(r,0)
            returnNewMem(newMemory(getAddressFromReg(r,0),value),forwardPC)
          case (OpCode.ANY,indir) =>
            returnNewMem(newMemory(getAddressFromReg(r,getMemFromPC(indir)),value),forwardPC)
        }
    }
  }

  private def handleLoad16Bit(opcode:OpCode):Z80System = {
    val sourceLoc=Load16Bit.sourceLoc.find(opcode)
    val (valueH,valueL)=sourceLoc match {
      case LoadLocation(r,_,_,_,_,_) if r!="" =>
        val value=getRegValue(r)
        (Z80Utils.getH(value),Z80Utils.getL(value))
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY =>
        (getMem(makeWord(getMemFromPC(pco+1),getMemFromPC(pco))+1),getMem(makeWord(getMemFromPC(pco+1),getMemFromPC(pco))))
      case LoadLocation(_,_,_,r,dirO,_) if r!="" =>
        dirO match {
          case OpCode.ANY => (getMemFromReg(r,1),getMemFromReg(r,0))
          case _ => (getMemFromReg(r,dirO+1),getMemFromReg(r,dirO))
        }
    }
    val destLoc=Load16Bit.destLoc.find(opcode)
    val instrSize=Load16Bit.instSize.find(opcode)
    val stackChange=Load16Bit.getstackChange(opcode)
    handleLoad16Bit(destLoc,valueH,valueL,instrSize,stackChange)
  }

  private def handleLoad16Bit(dest:LoadLocation, valueH:Int, valueL:Int, forwardPC:Int,stackChange:Int):Z80System= {
    dest match {
      case LoadLocation(r,_,_,_,_,_) if r!="" =>
        val newReg=newRegister(r,makeWord(valueH,valueL)) >>= RegisterController.setRelative("SP",stackChange)
        returnNewReg(newReg,forwardPC)
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY =>
        val address=makeWord(getMemFromPC(pco+1),getMemFromPC(pco))
        val newMem=newMemory(address+1,valueH) >>= MemoryController.poke(address,valueL)
        returnNewMem(newMem,forwardPC)
      case LoadLocation(_,_,_,r,dirO,_) if r!="" && dirO!=OpCode.ANY =>
        val newMem=newMemory(getAddressFromReg(r,dirO+1),valueH) >>= MemoryController.poke(getAddressFromReg(r,dirO),valueL)
        val newReg=registerController >>= RegisterController.setRelative("SP",stackChange)
        returnNew(newMem,newReg,forwardPC)
    }
  }


  private def getValueFromLocation(loc:LoadLocation):Int =
    loc match {
      case LoadLocation(r,_,_,_,_,_) if r!="" => getRegValue(r)
      case LoadLocation(_,i,_,_,_,_) if i!=OpCode.ANY => i
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY => getMem(makeWord(getMemFromPC(pco+1),getMemFromPC(pco)))
      case LoadLocation(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO,indirO) match {
          case (OpCode.ANY,OpCode.ANY) => getMemFromReg(r,0)
          case (o,OpCode.ANY) => getMemFromReg(r,o)
          case (OpCode.ANY,o) => getMemFromReg(r,getMemFromPC(o))
        }
    }
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
  def apply(source:Z80System):Z80System=new Z80System(source.memoryController,source.registerController)
}
