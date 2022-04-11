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

  private def getRegValue(symbol:String):Int=registerController.get(symbol)
  private def getFlag(flag:FlagSymbol):Boolean=registerController.get(flag)
  private def getFlagValue(flag:FlagSymbol):Int=if(getFlag(flag)) 1 else 0
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
    handleLoad16Bit(destLoc,value,instrSize,stackChange)
  }

  private def handleLoad16Bit(dest:LoadLocation, value:Int, forwardPC:Int,stackChange:Int):Z80System= {
    val chgList= List(putValueToLocation(dest,value,isWord = true))
    val stackChgList=dest match {
      case LoadLocation(r,_,rd,dirO,_,_) if r!="" || (rd!="" && dirO!=OpCode.ANY) =>
        List(new RegisterChangeRelative("SP",stackChange))
      case _ => List()
    }
    returnAfterChange(chgList++stackChgList,forwardPC)
  }

  private def getValueFromLocation(loc:LoadLocation):Int =
    loc match {
      case LoadLocation(r,_,_,_,_,_) if r!="" => getRegValue(r)
      case LoadLocation(_,pco,_,_,_,isWord) if pco!=OpCode.ANY =>
        if(isWord) getWord(getWordFromMemoryAtPC(pco)) else getByte(getWordFromMemoryAtPC(pco))
      case LoadLocation(_,_,r,dirO,indirO,isWord) if r!="" =>
        (dirO,indirO,isWord) match {
          case (OpCode.ANY,OpCode.ANY,_) => if(isWord) getWordFromMemoryAtReg(r,0) else getByteFromMemoryAtReg(r,0)
          case (o,OpCode.ANY,isWord) => if(isWord) getWordFromMemoryAtReg(r,o) else getByteFromMemoryAtReg(r,o)
          case (OpCode.ANY,off2Compl,_) => getByteFromMemoryAtReg(r,Z80Utils.rawByteTo2Compl(getByteFromMemoryAtPC(off2Compl)))
        }
    }

  private def putValueToMemory(address:Int, value:Int, isWord:Boolean):SystemChangeBase =
    if(isWord) new MemoryChangeWord(address,value)
    else new MemoryChangeByte(address,value)

  private def putValueToLocation(location:LoadLocation,value:Int,isWord:Boolean=false):SystemChangeBase =
    location match {
      case LoadLocation(r,_,_,_,_,_) if r!="" => new RegisterChange(r,value)
      case LoadLocation(_,pco,_,_,_,_) if pco!=OpCode.ANY => putValueToMemory(getWordFromMemoryAtPC(pco),value,isWord)
      case LoadLocation(_,_,r,dirO,indirO,_) if r!="" =>
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
    val oper = Arithmetic8Bit.operation.find(code)
    val instrSize = Arithmetic8Bit.instSize.find(code)
    val operandLoc=Arithmetic8Bit.operand.find(code)
    val operand=getValueFromLocation(operandLoc)

    val (value,destLocation,flags) = oper match {
      case o: ArithmeticOpLocationAccum =>
        val (value, flags) = handleArithmetic8Bit(o.operation, getRegValue("A"),operand)
        (value,LoadLocation.register("A"),flags)
      case o: ArithmeticOpLocationFlags =>
        val (value, flags) = handleArithmetic8Bit(o.operation, getRegValue("A"),operand)
        (value,LoadLocation.empty,flags)
      case o: ArithmeticOpVariableLocation =>
        val (value, flags) = handleArithmetic8Bit(o.operation, 0, operand, changeCarry = false)
        (value,operandLoc,flags)
    }
    val chgList=destLocation match {
      case loc if loc==LoadLocation.empty => List()
      case _ => List(putValueToLocation(destLocation,value))
    }
    returnAfterChange(chgList++List(new RegisterChange("F", flags)), instrSize)
  }

  private def handleArithmetic8Bit(operation:ArithmeticOperation,prevValueIn:Int,operandIn:Int,changeCarry:Boolean=true):(Int,Int)={
    //http://www.z80.info/z80sflag.htm
    val (prevValue,operand)=operation match {
      case ArithmeticOpType.Inc | ArithmeticOpType.Dec => (operandIn,1)
      case _ => (prevValueIn,operandIn)
    }
    val carry=getFlagValue(Flag.C)
    val (valueUnsigned,valueSigned)=operation match {
      case ArithmeticOpType.Add | ArithmeticOpType.Inc => (prevValue+operand,Z80Utils.rawByteTo2Compl(prevValue)+Z80Utils.rawByteTo2Compl(operand))
      case ArithmeticOpType.AddC => (prevValue+operand+carry,Z80Utils.rawByteTo2Compl(prevValue)+Z80Utils.rawByteTo2Compl(operand)+carry)
      case ArithmeticOpType.Sub | ArithmeticOpType.Comp | ArithmeticOpType.Dec => (prevValue-operand,Z80Utils.rawByteTo2Compl(prevValue)-Z80Utils.rawByteTo2Compl(operand))
      case ArithmeticOpType.SubC => (prevValue-operand-carry,Z80Utils.rawByteTo2Compl(prevValue)-Z80Utils.rawByteTo2Compl(operand)-carry)
      case ArithmeticOpType.And => (prevValue & operand,prevValue & operand)
      case ArithmeticOpType.Xor => (prevValue ^ operand,prevValue ^ operand)
      case ArithmeticOpType.Or => (prevValue | operand,prevValue | operand)
    }
    val valueOut=valueUnsigned & 0xFF
    val flagS=Z80Utils.rawByteTo2Compl(valueOut)<0
    val flagP=operation match {
      //parity
      case ArithmeticOpType.And | ArithmeticOpType.Xor | ArithmeticOpType.Or => Z80Utils.isEvenBits(valueUnsigned)
      // overflow
      case _ =>
        (valueSigned > 0x7F) || (valueSigned < -0x80)
    }
    val flagZ=valueOut==0
    val (flagH,flagN,flagC)=operation match {
      case ArithmeticOpType.Add | ArithmeticOpType.Inc => ((prevValue & 0x0F)+(operand & 0x0F)>0x0F,false,valueUnsigned>valueOut)
      case ArithmeticOpType.AddC => ((prevValue & 0x0F)+(operand & 0x0F)+carry>0x0F,false,valueUnsigned>valueOut)
      case ArithmeticOpType.Sub | ArithmeticOpType.Comp | ArithmeticOpType.Dec => ((prevValue & 0x0F)-(operand & 0x0F)<0x00,true,valueUnsigned<valueOut)
      case ArithmeticOpType.SubC => ((prevValue & 0x0F)-(operand & 0x0F)-carry<0x00,true,valueUnsigned<valueOut)
      case ArithmeticOpType.And => (true,false,false)
      case ArithmeticOpType.Xor | ArithmeticOpType.Or => (false,false,false)
    }

    val newF=Flag.set(flagS,flagZ,flagH,flagP,flagN,if(changeCarry) flagC else carry==1)
    (valueOut,newF)
  }

  private def handleArithmetic16Bit(code: OpCode):Z80System = {
    val oper = Arithmetic16Bit.operation.find(code)
    val instrSize = Arithmetic16Bit.instSize.find(code)
    val sourceLoc=Arithmetic16Bit.source.find(code)
    val destLoc=Arithmetic16Bit.destination.find(code)
    val operand=getValueFromLocation(sourceLoc)
    val prevValue=getValueFromLocation(destLoc)
    val prevFlags=getRegValue("F")

    val chgList=oper match {
      case o : ArithmeticOpVariableLocation =>
        val (value, flags) = handleArithmetic16Bit(o.operation, prevValue, prevFlags, operand)
        List(putValueToLocation(destLoc,value), new RegisterChange("F", flags))
    }
    returnAfterChange(chgList, instrSize)
  }

  private def handleArithmetic16Bit(operation:ArithmeticOperation,prevValueIn:Int,prevFlags:Int,operandIn:Int):(Int,Int)={
    //http://www.z80.info/z80sflag.htm
    val (prevValue,operand)=operation match {
      case ArithmeticOpType.Inc | ArithmeticOpType.Dec => (operandIn,1)
      case _ => (prevValueIn,operandIn)
    }
    val carry=getFlagValue(Flag.C)
    val (valueUnsigned,valueSigned)=operation match {
      case ArithmeticOpType.Add | ArithmeticOpType.Inc => (prevValue+operand,Z80Utils.rawWordTo2Compl(prevValue)+Z80Utils.rawWordTo2Compl(operand))
      case ArithmeticOpType.Dec => (prevValue-operand,Z80Utils.rawWordTo2Compl(prevValue)-Z80Utils.rawWordTo2Compl(operand))
      case ArithmeticOpType.AddC => (prevValue+operand+carry,Z80Utils.rawWordTo2Compl(prevValue)+Z80Utils.rawWordTo2Compl(operand)+carry)
      case ArithmeticOpType.SubC => (prevValue-operand-carry,Z80Utils.rawWordTo2Compl(prevValue)-Z80Utils.rawWordTo2Compl(operand)-carry)
    }
    val valueOut=valueUnsigned & 0xFFFF

    val newF=operation match {
      case ArithmeticOpType.Add =>
        new Flag(prevFlags)
          .set(Flag.H,(prevValue & 0x0FFF) + (operand & 0x0FFF) > 0x0FFF)
          .reset(Flag.N)
          .set(Flag.C,valueUnsigned>valueOut)()
      case ArithmeticOpType.AddC => Flag.set(
        Z80Utils.rawWordTo2Compl(valueOut)<0, valueOut==0,
        (prevValue & 0x0FFF) + (operand & 0x0FFF) > 0x0FFF,
        (valueSigned > 0x7FFF) || (valueSigned < -0x8000),n=false,valueUnsigned>valueOut
      )
      case ArithmeticOpType.SubC => Flag.set(
        Z80Utils.rawWordTo2Compl(valueOut)<0,valueOut==0,
        (prevValue & 0x0FFF) - (operand & 0x0FFF) < 0x0000,
        (valueSigned > 0x7FFF) || (valueSigned < -0x8000), n=true,valueUnsigned<valueOut
      )
      case ArithmeticOpType.Inc | ArithmeticOpType.Dec => prevFlags
    }
    (valueOut,newF)
  }


  private def handleRotateShift(code: OpCode):Z80System = {
    val oper = RotateShift.operation.find(code)
    val instrSize = RotateShift.instSize.find(code)
    val location=RotateShift.location.find(code)
    val prevValue=getValueFromLocation(location)
    val prevFlags=getRegValue("F")

    val (value, flags) = handleRotateShift(oper, prevValue, prevFlags)
    val change=putValueToLocation(location,value)

    returnAfterChange(List(change,new RegisterChange("F", flags)),instrSize)
  }

  private def handleRotateShift(operation:ArithmeticOperation,prevValueIn:Int,prevFlags:Int):(Int,Int)={
    //http://www.z80.info/z80sflag.htm
    val prevCarry=getFlagValue(Flag.C)
    val bit7=Z80Utils.getBit(prevValueIn,7)
    val bit0=Z80Utils.getBit(prevValueIn,0)
    val valueOut=operation match {
      case ArithmeticOpType.Rlc | ArithmeticOpType.Rlca => ((prevValueIn << 1) & 0xFF) + (if(bit7) 1 else 0)
      case ArithmeticOpType.Rrc | ArithmeticOpType.Rrca => ((prevValueIn >> 1) & 0xFF) + (if(bit0) 0x80 else 0)
      case ArithmeticOpType.Rl | ArithmeticOpType.Rla => ((prevValueIn << 1) & 0xFF) + prevCarry
      case ArithmeticOpType.Rr | ArithmeticOpType.Rra => ((prevValueIn >> 1) & 0xFF) + (prevCarry << 7)
      case ArithmeticOpType.Sla => (prevValueIn << 1) & 0xFF
      case ArithmeticOpType.Sra => ((prevValueIn >> 1) & 0xFF) + (if(bit7) 0x80 else 0)
    }

    val newCarry=operation match {
      case ArithmeticOpType.Rlc | ArithmeticOpType.Rlca | ArithmeticOpType.Sla => bit7
      case ArithmeticOpType.Rrc | ArithmeticOpType.Rrca | ArithmeticOpType.Sra => bit0
      case ArithmeticOpType.Rl | ArithmeticOpType.Rla => bit7
      case ArithmeticOpType.Rr | ArithmeticOpType.Rra => bit0
    }
    val valueSigned=Z80Utils.rawByteTo2Compl(valueOut)
    val flagS=valueSigned<0
    val flagZ=valueOut==0
    val flagP=Z80Utils.isEvenBits(valueOut)

    val newF=operation match {
      case ArithmeticOpType.Rlca | ArithmeticOpType.Rrca | ArithmeticOpType.Rla |  ArithmeticOpType.Rra =>
        new Flag(prevFlags).reset(Flag.H).reset(Flag.N).set(Flag.C,newCarry)()
      case _ => Flag.set(flagS,flagZ,h = false,flagP,n = false,newCarry)
    }

    (valueOut,newF)
  }

  private def handleRotateDigit(code: OpCode):Z80System = {
    val oper = RotateDigit.operation.find(code)
    val instrSize = RotateDigit.instSize.find(code)
    val location=RotateDigit.location.find(code)
    val prevValueR=getValueFromLocation(location)
    val prevValueA=getRegValue("A")

    val (valueR, valueA, flags) = handleRotateDigit(oper, prevValueR, prevValueA)
    val change=List(putValueToLocation(location,valueR),new RegisterChange("A",valueA))

    returnAfterChange(change++List(new RegisterChange("F", flags)),instrSize)
  }

  private def handleRotateDigit(operation:ArithmeticOperation,prevValueR:Int,prevValueA:Int):(Int,Int,Int)={
    //http://www.z80.info/z80sflag.htm
    val prevCarry=getFlag(Flag.C)
    val digit1A=(prevValueA & 0xF0) >> 4
    val digit2A=prevValueA & 0x0F
    val digit1R=(prevValueR & 0xF0) >> 4
    val digit2R=prevValueR & 0x0F
    val (valueOutR, valueOutA)=operation match {
      case ArithmeticOpType.Rld => ((digit2R << 4) + digit2A,(digit1A << 4) + digit1R)
      case ArithmeticOpType.Rrd => ((digit2A << 4) + digit1R,(digit1A << 4) + digit2R)
    }

    val valueSigned=Z80Utils.rawByteTo2Compl(valueOutA)
    val flagS=valueSigned<0
    val flagZ=valueOutA==0
    val flagP=Z80Utils.isEvenBits(valueOutA)

    val newF=Flag.set(flagS,flagZ,h = false,flagP,n = false,prevCarry)
    (valueOutR,valueOutA,newF)
  }

  private def handleBitManipulation(code: OpCode):Z80System = {
    val oper = BitManipulation.operation.find(code)
    val bit = BitManipulation.bit.find(code)
    val instrSize = BitManipulation.instSize.find(code)
    val location=BitManipulation.location.find(code)
    val prevValue=getValueFromLocation(location)
    val prevFlags=getRegValue("F")

    val (value, flags) = handleBitManipulation(oper,bit,prevValue,prevFlags)
    val change=
      (if(value!=prevValue) List(putValueToLocation(location,value)) else List())++
        (if(flags!=prevFlags) List(new RegisterChange("F", flags)) else List())

    returnAfterChange(change,instrSize)
  }

  private def handleBitManipulation(oper: BitOperation, bit: Int, prevValue: Int, prevFlags:Int):(Int,Int)={
    oper match {
      case BitOpType.Test =>
        val newZ= !Z80Utils.getBit(prevValue,bit)
        val newF=new Flag(prevFlags).set(Flag.Z,newZ).set(Flag.H).reset(Flag.N)()
        (prevValue,newF)
      case BitOpType.Reset => (Z80Utils.resetBit(prevValue,bit),prevFlags)
      case BitOpType.Set => (Z80Utils.setBit(prevValue,bit),prevFlags)
    }
  }

  private def handleJumpCallReturn(code: OpCode):Z80System = {
    val oper = JumpCallReturn.operation.find(code)
    val condition = JumpCallReturn.condition.find(code)
    val instrSize = JumpCallReturn.instSize.find(code)
    val location=JumpCallReturn.location.find(code)
    val prevPC=getRegValue("PC")
    val prevFlags=getRegValue("F")

    val address=oper match {
      case JumpType.JumpR => calcRelativeAddress(prevPC,getValueFromLocation(location))
      case JumpType.Return => getValueFromLocation(location)
      case _ => getValueFromLocation(location)
    }

    val (chgList,shouldJump)=oper match {
      case JumpType.Jump | JumpType.JumpR | JumpType.Call | JumpType.Return =>
        val (newPC,shouldJump)=handleJump(prevPC,address,prevFlags,condition)
        val newPCToChange=newPC+(if(!shouldJump) instrSize else 0)
        (List(new RegisterChange("PC",newPCToChange)),shouldJump)
    }

    val stackChange=(shouldJump,oper) match {
      case (true,JumpType.Call) => List(
        new MemoryChangeWord(getRegValue("SP")-2,getRegValue("PC")+instrSize),
        new RegisterChangeRelative("SP",-2)
      )
      case (true,JumpType.Return) => List(
        new RegisterChangeRelative("SP",2)
      )
      case _ => List()
    }

    returnAfterChange(chgList++stackChange)
  }

  // Jump relative - relative operand is 2's complement and must be incremented by 2
  private def calcRelativeAddress(pc:Int,relative:Int):Int=Z80Utils.word2ComplToRaw(pc+2+Z80Utils.rawByteTo2Compl(relative))

  private def handleJump(prevPC:Int,address:Int, prevFlags:Int,condition:JumpCondition):(Int,Boolean)={
    condition match {
      case JumpCondition(Flag.None,_) => (address,true)
      case condition =>
        if(new Flag(prevFlags)(condition.flag)==condition.value) (address,true) else (prevPC,false)
    }
  }
}

object Z80System {
  val blank:Z80System=new Z80System(MemoryController.blank(0x10000),RegisterController.blank)
}
