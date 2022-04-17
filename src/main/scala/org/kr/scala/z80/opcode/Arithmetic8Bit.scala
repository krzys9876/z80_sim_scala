package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object Arithmetic8Bit extends OperationSpec with OpCodeHandler {
  // Z80 manual page 50 (NOTE: ADD A,(HL) is 0x86, not 0x88!
  val operationListMap: Map[List[OpCode],AritheticOpLocationBase] = Map(
    List(OpCode(0x87),OpCode(0x80),OpCode(0x81),OpCode(0x82),OpCode(0x83),OpCode(0x84),OpCode(0x85),
      OpCode(0x86),OpCode(0xDD, 0x86),OpCode(0xFD, 0x86),OpCode(0xC6)) -> new ArithmeticOpLocationAccum(Add8b),
    List(OpCode(0x8F),OpCode(0x88),OpCode(0x89),OpCode(0x8A),OpCode(0x8B),OpCode(0x8C),OpCode(0x8D),
      OpCode(0x8E),OpCode(0xDD, 0x8E),OpCode(0xFD, 0x8E),OpCode(0xCE)) -> new ArithmeticOpLocationAccum(AddC8b),
    List(OpCode(0x97),OpCode(0x90),OpCode(0x91),OpCode(0x92),OpCode(0x93),OpCode(0x94),OpCode(0x95),
      OpCode(0x96),OpCode(0xDD, 0x96),OpCode(0xFD, 0x96),OpCode(0xD6)) -> new ArithmeticOpLocationAccum(Sub8b),
    List(OpCode(0x9F),OpCode(0x98),OpCode(0x99),OpCode(0x9A),OpCode(0x9B),OpCode(0x9C),OpCode(0x9D),
      OpCode(0x9E),OpCode(0xDD, 0x9E),OpCode(0xFD, 0x9E),OpCode(0xDE)) -> new ArithmeticOpLocationAccum(SubC8b),
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
    List(OpCode(0xED,0x44)) -> new ArithmeticOpLocationAccum(ArithmeticOpType.Neg),
    List(OpCode(0x3F)) -> new ArithmeticOpLocationFlags(ArithmeticOpType.Ccf),
    List(OpCode(0x37)) -> new ArithmeticOpLocationFlags(ArithmeticOpType.Scf)
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
      OpCode(0xFE)) -> LoadLocation.registerAddrDirOffset("PC", 1),
    List(OpCode(0x3F),OpCode(0x37)) -> LoadLocation.empty
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
      OpCode(0x2F),OpCode(0x3F),OpCode(0x37)) -> 1,
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
    val prevFlags=system.getFlags

    val (value,destLocation,flags) = oper match {
      case o: ArithmeticOpLocationAccum =>
        val (value, flags) = handleArithmetic8Bit(o.operation, system.getRegValue("A"),operand,prevFlags)
        (value,LoadLocation.register("A"),flags)
      case o: ArithmeticOpLocationFlags =>
        val (value, flags) = handleArithmetic8Bit(o.operation, system.getRegValue("A"),operand,prevFlags)
        (value,LoadLocation.empty,flags)
      case o: ArithmeticOpVariableLocation =>
        val (value, flags) = handleArithmetic8Bit(o.operation, 0, operand, prevFlags)
        (value,operandLoc,flags)
    }
    val chgList=destLocation match {
      case loc if loc==LoadLocation.empty => List()
      case _ => List(system.putValueToLocation(destLocation,value))
    }
    (chgList++List(new RegisterChange("F", flags)), instrSize)
  }

  private def handleArithmetic8Bit(oper:ArithmeticOperation,prevValueIn:Int,operandIn:Int,prevFlags:Flag):(Int,Int)={
    //http://www.z80.info/z80sflag.htm
    val prevCarry=prevFlags.flagValue(Flag.C)

    val (prevValue,operand)=oper match {
      case ArithmeticOpType.Inc | ArithmeticOpType.Dec => (operandIn,1)
      // NEG = 0-A
      case ArithmeticOpType.Neg => (0,prevValueIn)
      case _ => (prevValueIn,operandIn)
    }

    val (valueOut,flags)=oper match {
      case Add8b => Add8b.calcAll(ArithmeticOpInput(prevValue,operand,prevFlags))
      case AddC8b => AddC8b.calcAll(ArithmeticOpInput(prevValue,operand,prevFlags))
      case Sub8b => Sub8b.calcAll(ArithmeticOpInput(prevValue,operand,prevFlags))
      case SubC8b => SubC8b.calcAll(ArithmeticOpInput(prevValue,operand,prevFlags))
      case _ =>
        val (valueUnsigned,valueSigned,valueHalf,valueOut)=doCalculate(oper,prevValue,operand,prevCarry)
        val newF=calcFlags(oper,valueUnsigned,valueSigned,valueHalf,valueOut,prevFlags)
        (valueOut,new Flag(newF))
    }
    (valueOut,flags())
  }

  private def doCalculate(oper:ArithmeticOperation,value:Int,operand:Int,carry:Int):(Int,Int,Int,Int)= {
    val (valueUnsigned,valueSigned)=oper match {
      case ArithmeticOpType.Inc =>
        (value+operand,Z80Utils.rawByteTo2Compl(value)+Z80Utils.rawByteTo2Compl(operand))
      case ArithmeticOpType.Comp | ArithmeticOpType.Dec | ArithmeticOpType.Neg =>
        (value-operand,Z80Utils.rawByteTo2Compl(value)-Z80Utils.rawByteTo2Compl(operand))
      case ArithmeticOpType.And => (value & operand,value & operand)
      case ArithmeticOpType.Xor => (value ^ operand,value ^ operand)
      case ArithmeticOpType.Or => (value | operand,value | operand)
      case ArithmeticOpType.Cpl => (~value,~value)
      case ArithmeticOpType.Ccf | ArithmeticOpType.Scf => (OpCode.ANY,OpCode.ANY)
    }
    val valueHalf=oper match {
      case ArithmeticOpType.Inc => (value & 0x0F)+(operand & 0x0F)
      case ArithmeticOpType.Comp | ArithmeticOpType.Dec | ArithmeticOpType.Neg  =>
        (value & 0x0F)-(operand & 0x0F)
      case _ => OpCode.ANY
    }
    val valueOut=valueUnsigned & 0xFF
    (valueUnsigned,valueSigned,valueHalf,valueOut)
  }

  private def calcFlags(oper:ArithmeticOperation,valueUnsigned:Int,valueSigned:Int,valueHalf:Int,valueOut:Int,prevFlags:Flag):Int={
    val flagS=oper match {
      case ArithmeticOpType.Cpl | ArithmeticOpType.Ccf | ArithmeticOpType.Scf => prevFlags(Flag.S)
      case _ => Z80Utils.isNegativeByte(valueUnsigned)
    }
    val flagZ=oper match {
      case ArithmeticOpType.Cpl | ArithmeticOpType.Ccf | ArithmeticOpType.Scf => prevFlags(Flag.Z)
      case _ => valueOut==0
    }
    val flagH=oper match {
      case ArithmeticOpType.Inc => valueHalf>0x0F
      case ArithmeticOpType.Comp | ArithmeticOpType.Dec |
           ArithmeticOpType.Neg => valueHalf<0x00
      case ArithmeticOpType.And | ArithmeticOpType.Cpl => true
      case ArithmeticOpType.Xor | ArithmeticOpType.Or | ArithmeticOpType.Scf => false
      case ArithmeticOpType.Ccf => prevFlags(Flag.C)
    }
    val flagP=oper match {
      case ArithmeticOpType.Cpl | ArithmeticOpType.Ccf | ArithmeticOpType.Scf => prevFlags(Flag.P)
      //parity
      case ArithmeticOpType.And | ArithmeticOpType.Xor | ArithmeticOpType.Or => Z80Utils.isEvenBits(valueUnsigned)
      // overflow
      case _ => (valueSigned > 0x7F) || (valueSigned < -0x80)
    }
    val flagN=oper match {
      case ArithmeticOpType.Comp | ArithmeticOpType.Dec |
           ArithmeticOpType.Cpl | ArithmeticOpType.Neg => true
      case _ => false
    }
    val flagC=
      oper match {
        case ArithmeticOpType.Cpl | ArithmeticOpType.Inc | ArithmeticOpType.Dec => prevFlags(Flag.C)
        case ArithmeticOpType.Ccf => !prevFlags(Flag.C)
        case ArithmeticOpType.Scf => true
        case ArithmeticOpType.Comp |
             ArithmeticOpType.Neg => valueUnsigned<valueOut
        case ArithmeticOpType.And | ArithmeticOpType.Or | ArithmeticOpType.Xor => false
      }

    Flag.set(flagS,flagZ,flagH,flagP,flagN,flagC)
  }
}

// TODO: refactor to extract calculation logics from match clauses to separate classes

object Add8b extends ArithmeticOperationCalc("ADD_8B")
  with FlagSSignByte with FlagZZero with FlagHCarryByte
  with FlagPOverflowByte with FlagNReset with FlagCCarry {

  override def calc(input:ArithmeticOpInput):ArithmeticOpResult= {
    new ArithmeticOpResultByte(
      input.value + input.operand,
      Z80Utils.rawByteTo2Compl(input.value) + Z80Utils.rawByteTo2Compl(input.operand),
      (input.value & 0x0F)+(input.operand & 0x0F)
    )
  }
}

object AddC8b extends ArithmeticOperationCalc("ADD_8B_CARRY")
  with FlagSSignByte with FlagZZero with FlagHCarryByte
  with FlagPOverflowByte with FlagNReset with FlagCCarry {

  override def calc(input:ArithmeticOpInput):ArithmeticOpResult= {
    new ArithmeticOpResultByte(
      input.value + input.operand + input.flags.flagValue(Flag.C),
      Z80Utils.rawByteTo2Compl(input.value) + Z80Utils.rawByteTo2Compl(input.operand) + input.flags.flagValue(Flag.C),
      (input.value & 0x0F)+(input.operand & 0x0F)+input.flags.flagValue(Flag.C)
    )
  }
}

object Sub8b extends ArithmeticOperationCalc("SUB_8B")
  with FlagSSignByte with FlagZZero with FlagHBorrow
  with FlagPOverflowByte with FlagNSet with FlagCBorrow {

  override def calc(input:ArithmeticOpInput):ArithmeticOpResult= {
    new ArithmeticOpResultByte(
      input.value - input.operand,
      Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand),
      (input.value & 0x0F)-(input.operand & 0x0F)
    )
  }
}

object SubC8b extends ArithmeticOperationCalc("SUB_8B_CARRY")
  with FlagSSignByte with FlagZZero with FlagHBorrow
  with FlagPOverflowByte with FlagNSet with FlagCBorrow {

  override def calc(input:ArithmeticOpInput):ArithmeticOpResult= {
    new ArithmeticOpResultByte(
      input.value - input.operand - input.flags.flagValue(Flag.C),
      Z80Utils.rawByteTo2Compl(input.value) - Z80Utils.rawByteTo2Compl(input.operand) - input.flags.flagValue(Flag.C),
      (input.value & 0x0F) - (input.operand & 0x0F) - input.flags.flagValue(Flag.C)
    )
  }
}
