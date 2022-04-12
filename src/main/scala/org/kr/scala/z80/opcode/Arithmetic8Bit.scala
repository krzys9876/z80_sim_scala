package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object Arithmetic8Bit extends OperationSpec with OpCodeHandler {
  // Z80 manual page 50 (NOTE: ADD A,(HL) is 0x86, not 0x88!
  val operationListMap: Map[List[OpCode],AritheticOpLocationBase] = Map(
    List(OpCode(0x87),OpCode(0x80),OpCode(0x81),OpCode(0x82),OpCode(0x83),OpCode(0x84),OpCode(0x85),
      OpCode(0x86),OpCode(0xDD, 0x86),OpCode(0xFD, 0x86),OpCode(0xC6)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Add),
    List(OpCode(0x8F),OpCode(0x88),OpCode(0x89),OpCode(0x8A),OpCode(0x8B),OpCode(0x8C),OpCode(0x8D),
      OpCode(0x8E),OpCode(0xDD, 0x8E),OpCode(0xFD, 0x8E),OpCode(0xCE)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.AddC),
    List(OpCode(0x97),OpCode(0x90),OpCode(0x91),OpCode(0x92),OpCode(0x93),OpCode(0x94),OpCode(0x95),
      OpCode(0x96),OpCode(0xDD, 0x96),OpCode(0xFD, 0x96),OpCode(0xD6)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Sub),
    List(OpCode(0x9F),OpCode(0x98),OpCode(0x99),OpCode(0x9A),OpCode(0x9B),OpCode(0x9C),OpCode(0x9D),
      OpCode(0x9E),OpCode(0xDD, 0x9E),OpCode(0xFD, 0x9E),OpCode(0xDE)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.SubC),
    List(OpCode(0xA7),OpCode(0xA0),OpCode(0xA1),OpCode(0xA2),OpCode(0xA3),OpCode(0xA4),OpCode(0xA5),
      OpCode(0xA6),OpCode(0xDD, 0xA6),OpCode(0xFD, 0xA6),OpCode(0xE6)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.And),
    List(OpCode(0xAF),OpCode(0xA8),OpCode(0xA9),OpCode(0xAA),OpCode(0xAB),OpCode(0xAC),OpCode(0xAD),
      OpCode(0xAE),OpCode(0xDD, 0xAE),OpCode(0xFD, 0xAE),OpCode(0xEE)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Xor),
    List(OpCode(0xB7),OpCode(0xB0),OpCode(0xB1),OpCode(0xB2),OpCode(0xB3),OpCode(0xB4),OpCode(0xB5),
      OpCode(0xB6),OpCode(0xDD, 0xB6),OpCode(0xFD, 0xB6),OpCode(0xF6)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Or),
    List(OpCode(0xBF),OpCode(0xB8),OpCode(0xB9),OpCode(0xBA),OpCode(0xBB),OpCode(0xBC),OpCode(0xBD),
      OpCode(0xBE),OpCode(0xDD, 0xBE),OpCode(0xFD, 0xBE),OpCode(0xFE)) -> new ArithmeticOpLocationFlags(ArithmeticOpType.Comp),
    List(OpCode(0x3C),OpCode(0x04),OpCode(0x0C),OpCode(0x14),OpCode(0x1C),OpCode(0x24),OpCode(0x2C),
      OpCode(0x34),OpCode(0xDD, 0x34),OpCode(0xFD, 0x34)) -> new ArithmeticOpVariableLocation(ArithmeticOpType.Inc),
    List(OpCode(0x3D),OpCode(0x05),OpCode(0x0D),OpCode(0x15),OpCode(0x1D),OpCode(0x25),OpCode(0x2D),
      OpCode(0x35),OpCode(0xDD, 0x35),OpCode(0xFD, 0x35)) -> new ArithmeticOpVariableLocation(ArithmeticOpType.Dec),
    List(OpCode(0x2F)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Cpl),
    List(OpCode(0xED,0x44)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Neg)
  )

  val operation: OpCodeMap[AritheticOpLocationBase] = new OpCodeMap(operationListMap, AritheticOpLocationBase.empty)

  val operandListMap: Map[List[OpCode],LoadLocation] = Map(
    // accumulator
    List(OpCode(0x2F),OpCode(0xED,0x44))->LoadLocation.register("A"),
    //indirect register
    List(OpCode(0x86),OpCode(0x8E),OpCode(0x96),OpCode(0x9E),
      OpCode(0xA6),OpCode(0xAE),OpCode(0xB6),OpCode(0xBE),
      OpCode(0x34),OpCode(0x35)
    ) -> LoadLocation.registerAddr("HL"),
    //indirect registers with offset
    List(OpCode(0xDD, 0x86),OpCode(0xDD, 0x8E),OpCode(0xDD, 0x96),OpCode(0xDD, 0x9E),
      OpCode(0xDD, 0xA6),OpCode(0xDD, 0xAE),OpCode(0xDD, 0xB6),OpCode(0xDD, 0xBE),
      OpCode(0xDD, 0x34),OpCode(0xDD, 0x35)
    ) -> LoadLocation.registerAddrIndirOffset("IX",2),
    List(OpCode(0xFD, 0x86),OpCode(0xFD, 0x8E),OpCode(0xFD, 0x96),OpCode(0xFD, 0x9E),
      OpCode(0xFD, 0xA6),OpCode(0xFD, 0xAE),OpCode(0xFD, 0xB6),OpCode(0xFD, 0xBE),
      OpCode(0xFD, 0x34),OpCode(0xFD, 0x35)
    ) -> LoadLocation.registerAddrIndirOffset("IY",2),
    // immediate
    List(OpCode(0xC6), OpCode(0xCE), OpCode(0xD6), OpCode(0xDE),
      OpCode(0xE6), OpCode(0xEE), OpCode(0xF6),
      OpCode(0xFE)) -> LoadLocation.registerAddrDirOffset("PC", 1)
  )++
    //register
    OpCode.generateMapByReg(OpCode(0x80),1,0)++
    OpCode.generateMapByReg(OpCode(0x88),1,0)++
    OpCode.generateMapByReg(OpCode(0x90),1,0)++
    OpCode.generateMapByReg(OpCode(0x98),1,0)++
    OpCode.generateMapByReg(OpCode(0xA0),1,0)++
    OpCode.generateMapByReg(OpCode(0xA8),1,0)++
    OpCode.generateMapByReg(OpCode(0xB0),1,0)++
    OpCode.generateMapByReg(OpCode(0xB8),1,0)++
    OpCode.generateMapByReg(OpCode(0x04),1,3)++
    OpCode.generateMapByReg(OpCode(0x05),1,3)

  val operand: OpCodeMap[LoadLocation] = new OpCodeMap(operandListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x86),OpCode(0x8E),OpCode(0x96),OpCode(0x9E),OpCode(0xA6),OpCode(0xAE),OpCode(0xB6),OpCode(0xBE),
      OpCode(0x34),OpCode(0x35),
      OpCode(0x2F)) -> 1,
    List(OpCode(0xC6),OpCode(0xCE),OpCode(0xD6),OpCode(0xDE),OpCode(0xE6),OpCode(0xEE),OpCode(0xF6),OpCode(0xFE),
      OpCode(0xCE),OpCode(0xDE),OpCode(0xE6),OpCode(0xEE),OpCode(0xF6),OpCode(0xFE),
      OpCode(0xED,0x44)) -> 2,
    List(OpCode(0xDD, 0x86),OpCode(0xDD, 0x8E),OpCode(0xDD, 0x96),OpCode(0xDD, 0x9E),OpCode(0xDD, 0xA6),
      OpCode(0xDD, 0xAE),OpCode(0xDD, 0xB6),OpCode(0xDD, 0xBE),OpCode(0xDD, 0x34),OpCode(0xDD, 0x35),
      OpCode(0xFD, 0x86),OpCode(0xFD, 0x8E),OpCode(0xFD, 0x96),OpCode(0xFD, 0x9E),
      OpCode(0xFD, 0xA6),OpCode(0xFD, 0xAE),OpCode(0xFD, 0xB6),OpCode(0xFD, 0xBE),
      OpCode(0xFD, 0x34),OpCode(0xFD, 0x35)) -> 3,
      OpCode.generateListByReg(OpCode(0x80),1,0)->1,
      OpCode.generateListByReg(OpCode(0x88),1,0)->1,
      OpCode.generateListByReg(OpCode(0x90),1,0)->1,
      OpCode.generateListByReg(OpCode(0x98),1,0)->1,
      OpCode.generateListByReg(OpCode(0xA0),1,0)->1,
      OpCode.generateListByReg(OpCode(0xA8),1,0)->1,
      OpCode.generateListByReg(OpCode(0xB0),1,0)->1,
      OpCode.generateListByReg(OpCode(0xB8),1,0)->1,
      OpCode.generateListByReg(OpCode(0x04),1,3)->1,
      OpCode.generateListByReg(OpCode(0x05),1,3)->1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system: Z80System): (List[SystemChangeBase], Int) = {
    val oper = Arithmetic8Bit.operation.find(code)
    val instrSize = Arithmetic8Bit.instSize.find(code)
    val operandLoc=Arithmetic8Bit.operand.find(code)
    val operand=system.getValueFromLocation(operandLoc)
    val prevFlags=new Flag(system.getRegValue("F"))

    val (value,destLocation,flags) = oper match {
      case o: ArithmeticOpLocationAccum =>
        val (value, flags) = handleArithmetic8Bit(o.operation, system.getRegValue("A"),operand,prevFlags)
        (value,LoadLocation.register("A"),flags)
      case o: ArithmeticOpLocationFlags =>
        val (value, flags) = handleArithmetic8Bit(o.operation, system.getRegValue("A"),operand,prevFlags)
        (value,LoadLocation.empty,flags)
      case o: ArithmeticOpVariableLocation =>
        val (value, flags) = handleArithmetic8Bit(o.operation, 0, operand, prevFlags, changeCarry = false)
        (value,operandLoc,flags)
    }
    val chgList=destLocation match {
      case loc if loc==LoadLocation.empty => List()
      case _ => List(system.putValueToLocation(destLocation,value))
    }
    (chgList++List(new RegisterChange("F", flags)), instrSize)
  }

  private def handleArithmetic8Bit(oper:ArithmeticOperation,prevValueIn:Int,operandIn:Int,prevFlags:Flag,changeCarry:Boolean=true):(Int,Int)={
    //http://www.z80.info/z80sflag.htm
    val prevCarry=prevFlags.flagValue(Flag.C)

    val (prevValue,operand)=oper match {
      case ArithmeticOpType.Inc | ArithmeticOpType.Dec => (operandIn,1)
      // NEG = 0-A
      case ArithmeticOpType.Neg => (0,prevValueIn)
      case _ => (prevValueIn,operandIn)
    }
    val (valueUnsigned,valueSigned,valueHalf,valueOut)=doCalculate(oper,prevValue,operand,prevCarry)
    val newF=calcFlags(oper,valueUnsigned,valueSigned,valueHalf,valueOut,prevFlags,changeCarry)
    (valueOut,newF)
  }

  private def doCalculate(oper:ArithmeticOperation,value:Int,operand:Int,carry:Int):(Int,Int,Int,Int)= {
    val (valueUnsigned,valueSigned)=oper match {
      case ArithmeticOpType.Add | ArithmeticOpType.Inc =>
        (value+operand,Z80Utils.rawByteTo2Compl(value)+Z80Utils.rawByteTo2Compl(operand))
      case ArithmeticOpType.AddC =>
        (value+operand+carry,Z80Utils.rawByteTo2Compl(value)+Z80Utils.rawByteTo2Compl(operand)+carry)
      case ArithmeticOpType.Sub | ArithmeticOpType.Comp | ArithmeticOpType.Dec | ArithmeticOpType.Neg =>
        (value-operand,Z80Utils.rawByteTo2Compl(value)-Z80Utils.rawByteTo2Compl(operand))
      case ArithmeticOpType.SubC =>
        (value-operand-carry,Z80Utils.rawByteTo2Compl(value)-Z80Utils.rawByteTo2Compl(operand)-carry)
      case ArithmeticOpType.And => (value & operand,value & operand)
      case ArithmeticOpType.Xor => (value ^ operand,value ^ operand)
      case ArithmeticOpType.Or => (value | operand,value | operand)
      case ArithmeticOpType.Cpl => (~value,~value)
    }
    val valueHalf=oper match {
      case ArithmeticOpType.Add | ArithmeticOpType.Inc => (value & 0x0F)+(operand & 0x0F)
      case ArithmeticOpType.AddC => (value & 0x0F)+(operand & 0x0F)+carry
      case ArithmeticOpType.Sub | ArithmeticOpType.Comp | ArithmeticOpType.Dec | ArithmeticOpType.Neg  =>
        (value & 0x0F)-(operand & 0x0F)
      case ArithmeticOpType.SubC => (value & 0x0F)-(operand & 0x0F)-carry
      case _ => OpCode.ANY
    }
    val valueOut=valueUnsigned & 0xFF
    (valueUnsigned,valueSigned,valueHalf,valueOut)
  }

  private def calcFlags(oper:ArithmeticOperation,valueUnsigned:Int,valueSigned:Int,valueHalf:Int,valueOut:Int,prevFlags:Flag,changeCarry:Boolean):Int={
    val flagS=oper match {
      case ArithmeticOpType.Cpl => prevFlags(Flag.S)
      case _ => Z80Utils.isNegativeByte(valueUnsigned)
    }
    val flagZ=oper match {
      case ArithmeticOpType.Cpl => prevFlags(Flag.Z)
      case _ => valueOut==0
    }
    val flagH=oper match {
      case ArithmeticOpType.Add | ArithmeticOpType.AddC | ArithmeticOpType.Inc => valueHalf>0x0F
      case ArithmeticOpType.Sub | ArithmeticOpType.SubC | ArithmeticOpType.Comp | ArithmeticOpType.Dec |
           ArithmeticOpType.Neg => valueHalf<0x00
      case ArithmeticOpType.And | ArithmeticOpType.Cpl => true
      case ArithmeticOpType.Xor | ArithmeticOpType.Or => false
    }
    val flagP=oper match {
      case ArithmeticOpType.Cpl => prevFlags(Flag.P)
      //parity
      case ArithmeticOpType.And | ArithmeticOpType.Xor | ArithmeticOpType.Or => Z80Utils.isEvenBits(valueUnsigned)
      // overflow
      case _ => (valueSigned > 0x7F) || (valueSigned < -0x80)
    }
    val flagN=oper match {
      case ArithmeticOpType.Sub | ArithmeticOpType.Comp | ArithmeticOpType.Dec |
           ArithmeticOpType.SubC | ArithmeticOpType.Cpl | ArithmeticOpType.Neg => true
      case _ => false
    }
    val flagC=
      (changeCarry,oper) match {
        case (false,_) | (_,ArithmeticOpType.Cpl) => prevFlags(Flag.C)
        case (_,ArithmeticOpType.Add) | (_,ArithmeticOpType.AddC) | (_,ArithmeticOpType.Inc) => valueUnsigned>valueOut
        case (_,ArithmeticOpType.Sub) | (_,ArithmeticOpType.SubC) | (_,ArithmeticOpType.Comp) |
             (_,ArithmeticOpType.Dec) | (_,ArithmeticOpType.Neg) => valueUnsigned<valueOut
        case _ => false
      }

    Flag.set(flagS,flagZ,flagH,flagP,flagN,flagC)
  }
}
