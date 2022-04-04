package org.kr.scala.z80.system

import org.kr.scala.z80.opcode._
import org.kr.scala.z80.utils.Z80Utils

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
      case OpType.Exchange => handleExchange(opcode)
      case OpType.Arithmetic8Bit => handleArithmetic8Bit(opcode)
      case OpType.Unknown => throw new UnknownOperationException(f"Unknown operation $oper at $PC")
    }
  }

  private def getRegValue(symbol:String):Int=registerController.get(symbol)
  private def getByteFromPC(offset:Int):Int = getByteFromReg("PC",offset)
  private def getWordFromPC(offset:Int):Int = getWordFromReg("PC",offset)
  private def getAddressFromReg(symbol:String,offset:Int):Int= getRegValue(symbol)+offset
  private def getByteFromReg(symbol:String,offset:Int):Int = getByte(getAddressFromReg(symbol,offset))
  private def getWordFromReg(symbol:String,offset:Int):Int =
    Z80Utils.makeWord(getByte(getAddressFromReg(symbol,offset)+1),getByte(getAddressFromReg(symbol,offset)))
  private def getByte(address:Int):Int = memoryController.get(address)
  private def getWord(address:Int):Int = Z80Utils.makeWord(memoryController.get(address+1),memoryController.get(address))

  private def returnAfterChange(chgList:List[SystemChangeBase],forwardPC:Int):Z80System = {
    val chgListAfterPC=chgList++List(new RegisterChangeRelative("PC",forwardPC))
    (Z80SystemController(this) >>= Z80SystemController.changeList(chgListAfterPC)).get
  }

  private def returnAfterOneChange(chg:SystemChangeBase,forwardPC:Int):Z80System = returnAfterChange(List(chg),forwardPC)

  private def handleLoad8Bit(opcode:OpCode):Z80System = {
    val value=getValueFromLocation(Load8Bit.sourceLoc.find(opcode))
    val destLoc=Load8Bit.destLoc.find(opcode)
    val instrSize=Load8Bit.instSize.find(opcode)
    handleLoad8Bit(destLoc,value,instrSize)
  }

  private def handleNop:Z80System = returnAfterChange(List[SystemChangeBase](),1)

  private def handleLoad8Bit(dest:LoadLocation, value:Int, forwardPC:Int):Z80System= {
    val change= dest match {
      case LoadLocation(r,_,_,_,_,_) if r!="" => new RegisterChange(dest.reg,value)
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY =>
        new MemoryChangeByte(getWordFromPC(pco),value)
      case LoadLocation(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO,indirO) match {
          case (OpCode.ANY,OpCode.ANY) => getAddressFromReg(r,0)
            new MemoryChangeByte(getAddressFromReg(r,0),value)
          case (OpCode.ANY,indir) =>
            new MemoryChangeByte(getAddressFromReg(r,getByteFromPC(indir)),value)
        }
    }
    returnAfterOneChange(change,forwardPC)
  }

  private def handleLoad16Bit(opcode:OpCode):Z80System = {
    val sourceLoc=Load16Bit.sourceLoc.find(opcode)
    val value=sourceLoc match {
      case LoadLocation(r,_,_,_,_,_) if r!="" => getRegValue(r)
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY => getWord(getWordFromPC(pco))
      case LoadLocation(_,_,_,r,dirO,_) if r!="" =>
        dirO match {
          case OpCode.ANY => getWordFromReg(r,0)
          case _ => getWordFromReg(r,dirO)
        }
    }
    val destLoc=Load16Bit.destLoc.find(opcode)
    val instrSize=Load16Bit.instSize.find(opcode)
    val stackChange=Load16Bit.stackChange.find(opcode)
    handleLoad16Bit(destLoc,value,instrSize,stackChange)
  }

  private def handleLoad16Bit(dest:LoadLocation, value:Int, forwardPC:Int,stackChange:Int):Z80System= {
    val chgList= dest match {
      case LoadLocation(r,_,_,_,_,_) if r!="" =>
        List(new RegisterChange(r,value),new RegisterChangeRelative("SP",stackChange))
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY =>
        val address=getWordFromPC(pco)
        List(new MemoryChangeWord(address,value))
      case LoadLocation(_,_,_,r,dirO,_) if r!="" && dirO!=OpCode.ANY =>
        List(new MemoryChangeWord(getAddressFromReg(r,dirO),value),
          new RegisterChangeRelative("SP",stackChange))
    }
    returnAfterChange(chgList,forwardPC)
  }

  private def getValueFromLocation(loc:LoadLocation):Int =
    loc match {
      case LoadLocation(r,_,_,_,_,_) if r!="" => getRegValue(r)
      case LoadLocation(_,i,_,_,_,_) if i!=OpCode.ANY => i
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY => getByte(getWordFromPC(pco))
      case LoadLocation(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO,indirO) match {
          case (OpCode.ANY,OpCode.ANY) => getByteFromReg(r,0)
          case (o,OpCode.ANY) => getByteFromReg(r,o)
          case (OpCode.ANY,o) => getByteFromReg(r,getByteFromPC(o))
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
            new RegisterChange(loc.reg2,getWordFromReg(loc.reg1,0)))
      }
    })
    returnAfterChange(chgList,instrSize)
  }

  private def handleArithmetic8Bit(code: OpCode):Z80System = {
    //TODO: change hardcode to multi operand implementation
    val prevA=getRegValue("A")
    val operand=getRegValue("B")
    val prevA2C=Z80Utils.rawByteTo2Compl(prevA)
    val operand2C=Z80Utils.rawByteTo2Compl(operand)
    val value2C=prevA2C+operand2C
    val valueRaw=prevA+operand
    val valueByte=valueRaw & 0xFF
    val flagS=Z80Utils.rawByteTo2Compl(valueByte)<0
    val flagZ=valueByte==0
    val flagH=(prevA & 0x0F)+(operand & 0x0F)>0x0F
    val flagP=(value2C > 0x7F) || (value2C < -0x80)
    val flagN=false
    val flagC=valueRaw>valueByte
    val newF=Flag.set(flagS,flagZ,flagH,flagP,flagN,flagC)
    val chgList=List(new RegisterChange("A",valueByte),new RegisterChange("F",newF))
    returnAfterChange(chgList,Arithmetic8Bit.instSize.find(code))
  }
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
  def apply(source:Z80System):Z80System=new Z80System(source.memoryController,source.registerController)
}
