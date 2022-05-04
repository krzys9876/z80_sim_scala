package org.kr.scala.z80.system

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.opcode.handler.{Arithmetic16Bit, Arithmetic8Bit, BitManipulation, Exchange, Load8Bit}
import org.kr.scala.z80.utils.Z80Utils

class Z80System(val memoryController: MemoryController, val registerController: RegisterController,
                val outputController: OutputController,
                val inputController: InputController) {
  def step(implicit debugger:Debugger):Z80System= {
    val pc = registerController.get("PC")
    val opCode=OpCode(
      memoryController.get(pc),
      memoryController.get(pc,1),
      memoryController.get(pc,3))
    debugger.debug(pc,OpCode.getOpCodeObject(opCode))
    handle(opCode)
  }

  private def handle(opcode:OpCode):Z80System={
    val handler=opcodeHandlers.find(_.isOper(opcode)).getOrElse(Unknown)
    implicit val system:Z80System=this
    val (change,forwardPC)=handler.handle(opcode)
    returnAfterChange(change,forwardPC)
  }

  private val opcodeHandlers:List[OperationSpec with OpCodeHandler]=
    List(Load8Bit,Load16Bit,Exchange,Arithmetic8Bit,Arithmetic16Bit,
      RotateShift,RotateShift,RotateDigit,BitManipulation,JumpCallReturn,
      InputOutput,Nop,Unknown)

  def getRegValue(symbol:String):Int=registerController.get(symbol)
  def getFlags:Flag=new Flag(registerController.get("F"))

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

  def readPort(port:Int):Int=inputController.read(port)

  def getValueFromLocation(loc:Location):Int =
    loc match {
      case l if l==Location.empty => OpCode.ANY
      case Location(r,_,_,_,_,_,_) if r!="" => getRegValue(r)
      case Location(_,i,_,_,_,_,_) if i!=OpCode.ANY => i
      case Location(_,_,pco,_,_,_,isWord) if pco!=OpCode.ANY =>
        if(isWord) getWord(getWordFromMemoryAtPC(pco)) else getByte(getWordFromMemoryAtPC(pco))
      case Location(_,_,_,r,dirO,indirO,isWord) if r!="" =>
        (dirO,indirO,isWord) match {
          case (OpCode.ANY,OpCode.ANY,_) => if(isWord) getWordFromMemoryAtReg(r,0) else getByteFromMemoryAtReg(r,0)
          case (o,OpCode.ANY,isWord) => if(isWord) getWordFromMemoryAtReg(r,o) else getByteFromMemoryAtReg(r,o)
          case (OpCode.ANY,off2Compl,_) => getByteFromMemoryAtReg(r,Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(off2Compl)))
        }
    }

  private def putValueToMemory(address:Int, value:Int, isWord:Boolean):SystemChangeBase =
    if(isWord) new MemoryChangeWord(address,value)
    else new MemoryChangeByte(address,value)

  def putValueToLocation(location:Location, value:Int, isWord:Boolean=false):SystemChangeBase =
    location match {
      case Location(r,_,_,_,_,_,_) if r!="" => new RegisterChange(r,value)
      case Location(_,_,pco,_,_,_,_) if pco!=OpCode.ANY => putValueToMemory(getWordFromMemoryAtPC(pco),value,isWord)
      case Location(_,_,_,r,dirO,indirO,_) if r!="" =>
        (dirO,indirO) match {
          case (dirO,OpCode.ANY) if dirO!=OpCode.ANY => putValueToMemory(getAddressFromReg(r,dirO),value,isWord)
          case (OpCode.ANY,OpCode.ANY) => putValueToMemory(getAddressFromReg(r,0),value,isWord)
          case (OpCode.ANY,indirOff2Compl) =>
            putValueToMemory(getAddressFromReg(r,Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(indirOff2Compl))),value,isWord)
        }
      case l if l==Location.empty => new DummyChange
    }
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank,
    OutputController.blank, InputController.blank)
}