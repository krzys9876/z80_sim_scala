package org.kr.scala.z80.system

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.utils.Z80Utils

class Z80System(val memoryController: MemoryController, val registerController: RegisterController) {
  def step:Z80System= {
    val PC = registerController.get("PC")
    val opcode = OpCode(
      memoryController.get(PC),
      memoryController.get(PC,1),
      memoryController.get(PC,3))

    val opTypeSpec=specsHandlerMap.keys.find(_.isOper(opcode)).getOrElse(Unknown)
    val opCodeHandler=specsHandlerMap.getOrElse(opTypeSpec, handleUnknown(_))
    opCodeHandler(opcode)
  }

  private val specsHandlerMap:Map[OperationSpec,OpCode=>Z80System]=Map(
    Load8Bit->handleLoad8Bit,
    Load16Bit->handleLoad16Bit,
    Exchange->handleExchange,
    Arithmetic8Bit->handleArithmetic8Bit,
    Arithmetic16Bit->handleArithmetic16Bit,
    RotateShift->handleRotateShift,
    RotateDigit->handleRotateDigit,
    BitManipulation->handleBitManipulation,
    JumpCallReturn->handleJumpCallReturn,
    Nop->handleNop,
    Unknown->handleUnknown
  )

  def getRegValue(symbol:String):Int=registerController.get(symbol)
  def getFlag(flag:FlagSymbol):Boolean=registerController.get(flag)
  def getFlagValue(flag:FlagSymbol):Int=if(getFlag(flag)) 1 else 0
  private def getByteFromMemoryAtPC(offset:Int):Int = getByteFromMemoryAtReg("PC",offset)
  private def getWordFromMemoryAtPC(offset:Int):Int = getWordFromMemoryAtReg("PC",offset)
  private def getAddressFromReg(symbol:String,offset:Int):Int= getRegValue(symbol)+offset
  private def getByteFromMemoryAtReg(symbol:String,offset:Int):Int = getByte(getAddressFromReg(symbol,offset))
  private def getWordFromMemoryAtReg(symbol:String,offset:Int):Int =
    Z80Utils.makeWord(getByte(getAddressFromReg(symbol,offset)+1),getByte(getAddressFromReg(symbol,offset)))
  private def getByte(address:Int):Int = memoryController.get(address)
  private def getWord(address:Int):Int = Z80Utils.makeWord(memoryController.get(address+1),memoryController.get(address))

  private def returnAfterChange(chgList:List[SystemChangeBase],forwardPC:Int=0):Z80System = {
    val chgListAfterPC=chgList ++ (if(forwardPC!=0) List(new RegisterChangeRelative("PC",forwardPC)) else List())
    (Z80SystemController(this) >>= Z80SystemController.changeList(chgListAfterPC)).get
  }

  private def returnAfterOneChange(chg:SystemChangeBase,forwardPC:Int):Z80System = returnAfterChange(List(chg),forwardPC)

  private def handleUnknown(code: OpCode):Z80System = {
    throw new UnknownOperationException(f"Unknown operation $code at ${getRegValue("PC")}")
    this
  }

  private def handleNop(code:OpCode):Z80System = {
    val instrSize=Nop.instSize.find(code)
    returnAfterChange(List[SystemChangeBase](),instrSize)
  }

  private def handleLoad8Bit(opcode:OpCode):Z80System = {
    val value=getValueFromLocation(Load8Bit.sourceLoc.find(opcode))
    val destLoc=Load8Bit.destLoc.find(opcode)
    val instrSize=Load8Bit.instSize.find(opcode)
    returnAfterOneChange(putValueToLocation(destLoc,value),instrSize)
  }

  private def handleLoad16Bit(opcode:OpCode):Z80System = {
    val sourceLoc=Load16Bit.sourceLoc.find(opcode)
    val value=getValueFromLocation(sourceLoc)
    val destLoc=Load16Bit.destLoc.find(opcode)
    val instrSize=Load16Bit.instSize.find(opcode)
    val stackChange=Load16Bit.stackChange.find(opcode)

    val chgList= List(putValueToLocation(destLoc,value,isWord = true))
    val stackChgList=destLoc match {
      case LoadLocation(r,_,_,rd,dirO,_,_) if r!="" || (rd!="" && dirO!=OpCode.ANY) =>
        List(new RegisterChangeRelative("SP",stackChange))
      case _ => List()
    }
    returnAfterChange(chgList++stackChgList,instrSize)
  }

  def getValueFromLocation(loc:LoadLocation):Int =
    loc match {
      case LoadLocation(r,_,_,_,_,_,_) if r!="" => getRegValue(r)
      case LoadLocation(_,i,_,_,_,_,_) if i!=OpCode.ANY => i
      case LoadLocation(_,_,pco,_,_,_,isWord) if pco!=OpCode.ANY =>
        if(isWord) getWord(getWordFromMemoryAtPC(pco)) else getByte(getWordFromMemoryAtPC(pco))
      case LoadLocation(_,_,_,r,dirO,indirO,isWord) if r!="" =>
        (dirO,indirO,isWord) match {
          case (OpCode.ANY,OpCode.ANY,_) => if(isWord) getWordFromMemoryAtReg(r,0) else getByteFromMemoryAtReg(r,0)
          case (o,OpCode.ANY,isWord) => if(isWord) getWordFromMemoryAtReg(r,o) else getByteFromMemoryAtReg(r,o)
          case (OpCode.ANY,off2Compl,_) => getByteFromMemoryAtReg(r,Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(off2Compl)))
        }
    }

  private def putValueToMemory(address:Int, value:Int, isWord:Boolean):SystemChangeBase =
    if(isWord) new MemoryChangeWord(address,value)
    else new MemoryChangeByte(address,value)

  def putValueToLocation(location:LoadLocation,value:Int,isWord:Boolean=false):SystemChangeBase =
    location match {
      case LoadLocation(r,_,_,_,_,_,_) if r!="" => new RegisterChange(r,value)
      case LoadLocation(_,_,pco,_,_,_,_) if pco!=OpCode.ANY => putValueToMemory(getWordFromMemoryAtPC(pco),value,isWord)
      case LoadLocation(_,_,_,r,dirO,indirO,_) if r!="" =>
        (dirO,indirO) match {
          case (dirO,OpCode.ANY) if dirO!=OpCode.ANY => putValueToMemory(getAddressFromReg(r,dirO),value,isWord)
          case (OpCode.ANY,OpCode.ANY) => putValueToMemory(getAddressFromReg(r,0),value,isWord)
          case (OpCode.ANY,indirOff2Compl) =>
            putValueToMemory(getAddressFromReg(r,Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(indirOff2Compl))),value,isWord)
        }
    }

  private def handleExchange(opcode:OpCode):Z80System = {
    val exchangeLocList=Exchange.exchangeLoc.find(opcode)
    val instrSize=Exchange.instSize.find(opcode)

    val chgList=exchangeLocList.flatMap(entry=>{
      entry match {
        case loc : ExchangeLocation =>
          List(new RegisterChange(loc.reg1,getRegValue(entry.reg2)),
            new RegisterChange(loc.reg2,getRegValue(entry.reg1)))
        case loc : ExchangeLocationIndirect =>
          List(new MemoryChangeWord(getAddressFromReg(loc.reg1,0),getRegValue(loc.reg2)),
            new RegisterChange(loc.reg2,getWordFromMemoryAtReg(loc.reg1,0)))
      }
    })
    returnAfterChange(chgList,instrSize)
  }

  private def handleArithmetic8Bit(code: OpCode):Z80System = {
    implicit val system:Z80System=this
    val (change,forwardPC)=Arithmetic8Bit.handle(code)
    returnAfterChange(change,forwardPC)
  }

  private def handleArithmetic16Bit(code: OpCode):Z80System = {
    implicit val system:Z80System=this
    val (change,forwardPC)=Arithmetic16Bit.handle(code)
    returnAfterChange(change,forwardPC)
  }

  private def handleRotateShift(code: OpCode):Z80System = {
    implicit val system:Z80System=this
    val (change,forwardPC)=RotateShift.handle(code)
    returnAfterChange(change,forwardPC)
  }

  private def handleRotateDigit(code: OpCode):Z80System = {
    implicit val system:Z80System=this
    val (change,forwardPC)=RotateDigit.handle(code)
    returnAfterChange(change,forwardPC)
  }

  private def handleBitManipulation(code: OpCode):Z80System = {
    implicit val system:Z80System=this
    val (change,forwardPC)=BitManipulation.handle(code)
    returnAfterChange(change,forwardPC)
  }

  private def handleJumpCallReturn(code: OpCode):Z80System = {
    implicit val system:Z80System=this
    val (change,forwardPC)=JumpCallReturn.handle(code)
    returnAfterChange(change,forwardPC)
  }
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
}
