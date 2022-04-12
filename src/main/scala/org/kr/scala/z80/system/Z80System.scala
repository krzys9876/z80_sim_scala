package org.kr.scala.z80.system

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.utils.Z80Utils

class Z80System(val memoryController: MemoryController, val registerController: RegisterController) {
  def step:Z80System= {
    val PC = registerController.get("PC")
    handle(
      OpCode(
        memoryController.get(PC),
        memoryController.get(PC,1),
        memoryController.get(PC,3)))
  }

  private def handle(opcode:OpCode):Z80System={
    val handler=opcodeHandlers.find(_.isOper(opcode)).getOrElse(Unknown)
    implicit val system:Z80System=this
    val (change,forwardPC)=handler.handle(opcode)
    returnAfterChange(change,forwardPC)
  }

  private val opcodeHandlers:List[OperationSpec with OpCodeHandler]=
    List(Load8Bit,Load16Bit,Exchange,Arithmetic8Bit,Arithmetic16Bit,
      RotateShift,RotateShift,RotateDigit,BitManipulation,JumpCallReturn,Nop,Unknown)

  def getRegValue(symbol:String):Int=registerController.get(symbol)
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
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
}