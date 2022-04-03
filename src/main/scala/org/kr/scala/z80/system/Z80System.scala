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
      case OpType.Unknown => throw new UnknownOperationException(f"Unknown operation $oper at $PC")
    }
  }

  private def getRegValue(symbol:String):Int=registerController.get(symbol)
  private def getMemFromPC(offset:Int):Int = getMemFromReg("PC",offset)
  private def getAddressFromReg(symbol:String,offset:Int):Int= getRegValue(symbol)+offset
  private def getMemFromReg(symbol:String,offset:Int):Int = getMem(getAddressFromReg(symbol,offset))
  private def getMem(address:Int):Int = memoryController.get(address)

  private def newRegister(regList:List[(String,Int)]):RegisterController= {
    val newReg=regList.foldLeft(registerController.get)(
      (register, regValuePair)=>(RegisterController(register) >>= RegisterController.set(regValuePair._1,regValuePair._2)).get)
    RegisterController(newReg)
  }

  private def returnNew(newMemory:BaseStateMonad[Memory],newRegister:BaseStateMonad[Register], forwardPC:Int):Z80System =
    new Z80System(MemoryController(newMemory.get),RegisterController(newRegister.get.movePC(forwardPC)))
  private def returnNewReg(newRegister:BaseStateMonad[Register], forwardPC:Int):Z80System = {
    new Z80System(memoryController,RegisterController(newRegister.get.movePC(forwardPC)))
  }

  private def returnAfterChange(chgList:List[SystemChangeBase],forwardPC:Int):Z80System = {
    val chgListAfterPC=chgList++List(new RegisterChangeRelative("PC",forwardPC))
    (Z80SystemController(this) >>= Z80SystemController.changeList(chgListAfterPC)).get
  }

  private def returnAfterOneChange(chg:SystemChangeBase,forwardPC:Int):Z80System = returnAfterChange(List(chg),forwardPC)


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
    val change= dest match {
      case LoadLocation(r,_,_,_,_,_) if r!="" => new RegisterChange(dest.reg,value)
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY =>
        new MemoryChangeByte(Z80Utils.makeWord(getMemFromPC(pco+1),getMemFromPC(pco)),value)
      case LoadLocation(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO,indirO) match {
          case (OpCode.ANY,OpCode.ANY) => getAddressFromReg(r,0)
            new MemoryChangeByte(getAddressFromReg(r,0),value)
          case (OpCode.ANY,indir) =>
            new MemoryChangeByte(getAddressFromReg(r,getMemFromPC(indir)),value)
        }
    }
    returnAfterOneChange(change,forwardPC)
  }

  private def handleLoad16Bit(opcode:OpCode):Z80System = {
    val sourceLoc=Load16Bit.sourceLoc.find(opcode)
    val (valueH,valueL)=sourceLoc match {
      case LoadLocation(r,_,_,_,_,_) if r!="" =>
        val value=getRegValue(r)
        (Z80Utils.getH(value),Z80Utils.getL(value))
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY =>
        (getMem(Z80Utils.makeWord(getMemFromPC(pco+1),getMemFromPC(pco))+1),getMem(Z80Utils.makeWord(getMemFromPC(pco+1),getMemFromPC(pco))))
      case LoadLocation(_,_,_,r,dirO,_) if r!="" =>
        dirO match {
          case OpCode.ANY => (getMemFromReg(r,1),getMemFromReg(r,0))
          case _ => (getMemFromReg(r,dirO+1),getMemFromReg(r,dirO))
        }
    }
    val destLoc=Load16Bit.destLoc.find(opcode)
    val instrSize=Load16Bit.instSize.find(opcode)
    val stackChange=Load16Bit.stackChange.find(opcode)
    handleLoad16Bit(destLoc,Z80Utils.makeWord(valueH,valueL),instrSize,stackChange)
  }

  private def handleLoad16Bit(dest:LoadLocation, value:Int, forwardPC:Int,stackChange:Int):Z80System= {
    val chgList= dest match {
      case LoadLocation(r,_,_,_,_,_) if r!="" =>
        List(new RegisterChange(r,value),new RegisterChangeRelative("SP",stackChange))
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY =>
        val address=Z80Utils.makeWord(getMemFromPC(pco+1),getMemFromPC(pco))
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
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY => getMem(Z80Utils.makeWord(getMemFromPC(pco+1),getMemFromPC(pco)))
      case LoadLocation(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO,indirO) match {
          case (OpCode.ANY,OpCode.ANY) => getMemFromReg(r,0)
          case (o,OpCode.ANY) => getMemFromReg(r,o)
          case (OpCode.ANY,o) => getMemFromReg(r,getMemFromPC(o))
        }
    }

  private def handleExchange(opcode:OpCode):Z80System = {
    val exchangeLocList=Exchange.exchangeLoc.find(opcode)
    val instrSize=Exchange.instSize.find(opcode)
    val newReg=handleExchangeWithRegister(exchangeLocList.filter(loc => !loc.reg1Indirect))
    //val newMem=handleExchangeWithMemory(exchangeLocList.filter(loc => loc.reg1Indirect))
    returnNew(memoryController,newReg,instrSize)
  }

  private def handleExchangeWithRegister(locList:List[ExchangeLocation]):RegisterController = {
    val regModifyList:List[(String,Int)]=locList.foldLeft(List[(String,Int)]())(
      (list,entry)=>list++List((entry.reg1,getRegValue(entry.reg2)),(entry.reg2,getRegValue(entry.reg1))))
    newRegister(regModifyList)
  }

  // do poprawki!!! - rozkodować zapisywanie pamięci na dwa bajty (teraz zapisuje się w jednej komórce całe słowo zamiast 2 bajtów w kokejnych komórkach)
  private def handleExchangeWithMemory(locList:List[ExchangeLocation]):Z80System = {
    val newSystem=locList.foldLeft(this)(
      (system,entry)=> {
        system
      }/*list++List[(Int,Int,String,Int)](
        (getRegValue(entry.reg1),getRegValue(entry.reg2),
          entry.reg2,Z80Utils.makeWord(getMemFromReg(entry.reg1,1),getMemFromReg(entry.reg1,0))*/
          )
    newSystem
    //newMemory(regMemModifyList.map(entry=>(entry._1,entry._2)))
  }

}

//getRegValue(entry.reg1),getRegValue(entry.reg2),
//entry.reg2,Z80Utils.makeWord(getMemFromReg(entry.reg1,1),getMemFromReg(entry.reg1,0)


object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
  def apply(source:Z80System):Z80System=new Z80System(source.memoryController,source.registerController)
}
