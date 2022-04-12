package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object Arithmetic16Bit extends OperationSpec with OpCodeHandler {
  // Z80 manual page 52
  val operationListMap: Map[List[OpCode],AritheticOpLocationBase] = Map(
    List(OpCode(0x09),OpCode(0x19),OpCode(0x29),OpCode(0x39),
      OpCode(0xDD,0x09),OpCode(0xFD,0x09),OpCode(0xDD,0x19),OpCode(0xFD,0x19),OpCode(0xDD,0x39),OpCode(0xFD,0x39),
      OpCode(0xDD,0x29),OpCode(0xFD,0x29)) -> new ArithmeticOpVariableLocation(ArithmeticOpType.Add),
    List(OpCode(0xED,0x4A),OpCode(0xED,0x5A),OpCode(0xED,0x6A),OpCode(0xED,0x7A))
      -> new ArithmeticOpVariableLocation(ArithmeticOpType.AddC),
    List(OpCode(0xED,0x42),OpCode(0xED,0x52),OpCode(0xED,0x62),OpCode(0xED,0x72))
      -> new ArithmeticOpVariableLocation(ArithmeticOpType.SubC),
    List(OpCode(0x03),OpCode(0x13),OpCode(0x23),OpCode(0x33),OpCode(0xDD,0x23),OpCode(0xFD,0x23))
      -> new ArithmeticOpVariableLocation(ArithmeticOpType.Inc),
    List(OpCode(0xDB),OpCode(0x1B),OpCode(0x2B),OpCode(0x3B),OpCode(0xDD,0x2B),OpCode(0xFD,0x2B))
      -> new ArithmeticOpVariableLocation(ArithmeticOpType.Dec)
  )
  val operation: OpCodeMap[AritheticOpLocationBase] = new OpCodeMap(operationListMap, AritheticOpLocationBase.empty)

  val sourceListMap: Map[List[OpCode],LoadLocation] = Map(
    List(OpCode(0x09),OpCode(0xDD,0x09),OpCode(0xFD,0x09),OpCode(0xED,0x4A),OpCode(0xED,0x42),
      OpCode(0x03),OpCode(0xDB)) -> LoadLocation.register("BC"),
    List(OpCode(0x19),OpCode(0xDD,0x19),OpCode(0xFD,0x19),OpCode(0xED,0x5A),OpCode(0xED,0x52),
      OpCode(0x13),OpCode(0x1B)) -> LoadLocation.register("DE"),
    List(OpCode(0x29),OpCode(0xED,0x6A),OpCode(0xED,0x62),OpCode(0x23),OpCode(0x2B)) -> LoadLocation.register("HL"),
    List(OpCode(0x39),OpCode(0xDD,0x39),OpCode(0xFD,0x39),OpCode(0xED,0x7A),OpCode(0xED,0x72),
      OpCode(0x33),OpCode(0x3B)) -> LoadLocation.register("SP"),
    List(OpCode(0xDD,0x29),OpCode(0xDD,0x23),OpCode(0xDD,0x2B)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD,0x29),OpCode(0xFD,0x23),OpCode(0xFD,0x2B)) -> LoadLocation.register("IY")
  )
  val source: OpCodeMap[LoadLocation] = new OpCodeMap(sourceListMap, LoadLocation.empty)

  val destinationListMap: Map[List[OpCode],LoadLocation] = Map(
    List(OpCode(0x09),OpCode(0x19),OpCode(0x29),OpCode(0x39),OpCode(0xED,0x4A),OpCode(0xED,0x42),
      OpCode(0xED,0x5A),OpCode(0xED,0x52),OpCode(0xED,0x6A),OpCode(0xED,0x62),OpCode(0xED,0x7A),OpCode(0xED,0x72),
      OpCode(0x23),OpCode(0x2B)) -> LoadLocation.register("HL"),
    List(OpCode(0xDD,0x09),OpCode(0xDD,0x19),OpCode(0xDD,0x39),OpCode(0xDD,0x29),
      OpCode(0xDD,0x23),OpCode(0xDD,0x2B)) -> LoadLocation.register("IX"),
    List(OpCode(0xFD,0x09),OpCode(0xFD,0x19),OpCode(0xFD,0x39),OpCode(0xFD,0x29),
      OpCode(0xFD,0x23),OpCode(0xFD,0x2B)) -> LoadLocation.register("IY"),
    List(OpCode(0x03),OpCode(0xDB)) -> LoadLocation.register("BC"),
    List(OpCode(0x13),OpCode(0x1B)) -> LoadLocation.register("DE"),
    List(OpCode(0x33),OpCode(0x3B)) -> LoadLocation.register("SP")
  )
  val destination: OpCodeMap[LoadLocation] = new OpCodeMap(destinationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x09),OpCode(0x19),OpCode(0x29),OpCode(0x39),OpCode(0x03),OpCode(0x13),
      OpCode(0x23),OpCode(0x33),OpCode(0xDB),OpCode(0x1B),OpCode(0x2B),OpCode(0x3B)) -> 1,
    List(OpCode(0xDD,0x09),OpCode(0xFD,0x09),OpCode(0xDD,0x19),OpCode(0xFD,0x19),OpCode(0xDD,0x39),OpCode(0xFD,0x39),
      OpCode(0xDD,0x29),OpCode(0xFD,0x29),OpCode(0xED,0x4A),OpCode(0xED,0x5A),OpCode(0xED,0x6A),OpCode(0xED,0x7A),
      OpCode(0xED,0x42),OpCode(0xED,0x52),OpCode(0xED,0x62),OpCode(0xED,0x72),
      OpCode(0xDD,0x23),OpCode(0xFD,0x23),OpCode(0xDD,0x2B),OpCode(0xFD,0x2B)) -> 2
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    val oper = operation.find(code)
    val instrSize = instSize.find(code)
    val sourceLoc=source.find(code)
    val destLoc=destination.find(code)
    val operand=system.getValueFromLocation(sourceLoc)
    val prevValue=system.getValueFromLocation(destLoc)
    val prevFlags=system.getRegValue("F")

    val chgList=oper match {
      case o : ArithmeticOpVariableLocation =>
        val (value, flags) = handleArithmetic16Bit(o.operation, prevValue, prevFlags, operand)
        List(system.putValueToLocation(destLoc,value), new RegisterChange("F", flags))
    }
    (chgList, instrSize)
  }

  private def handleArithmetic16Bit(oper:ArithmeticOperation, prevValueIn:Int, prevFlags:Int, operandIn:Int):(Int,Int)={
    //http://www.z80.info/z80sflag.htm
    val (prevValue,operand)=oper match {
      case ArithmeticOpType.Inc | ArithmeticOpType.Dec => (operandIn,1)
      case _ => (prevValueIn,operandIn)
    }

    val (valueUnsigned,valueSigned,valueHalf,valueOut)=doCalculate(oper,prevValue,operand,new Flag(prevFlags).flagValue(Flag.C))
    val newF=calcFlags(oper,prevFlags,valueUnsigned,valueSigned,valueHalf,valueOut)

    (valueOut,newF)
  }

  private def doCalculate(oper:ArithmeticOperation,value:Int,operand:Int,carry:Int):(Int,Int,Int,Int)= {
    val (valueUnsigned, valueSigned) = oper match {
      case ArithmeticOpType.Add | ArithmeticOpType.Inc => (value + operand, Z80Utils.rawWordTo2Compl(value) + Z80Utils.rawWordTo2Compl(operand))
      case ArithmeticOpType.Dec => (value - operand, Z80Utils.rawWordTo2Compl(value) - Z80Utils.rawWordTo2Compl(operand))
      case ArithmeticOpType.AddC => (value + operand + carry, Z80Utils.rawWordTo2Compl(value) + Z80Utils.rawWordTo2Compl(operand) + carry)
      case ArithmeticOpType.SubC => (value - operand - carry, Z80Utils.rawWordTo2Compl(value) - Z80Utils.rawWordTo2Compl(operand) - carry)
    }
    val valueHalf = oper match {
      case ArithmeticOpType.Add => (value & 0x0FFF) + (operand & 0x0FFF)
      case ArithmeticOpType.AddC =>  (value & 0x0FFF) + (operand & 0x0FFF) + carry
      case ArithmeticOpType.SubC => (value & 0x0FFF) - (operand & 0x0FFF) - carry
      case _ => OpCode.ANY
    }
    val valueOut = valueUnsigned & 0xFFFF
    (valueUnsigned, valueSigned, valueHalf, valueOut)
  }

  private def calcFlags(oper:ArithmeticOperation,prevFlags:Int,
                        valueUnsigned:Int,valueSigned:Int,valueHalf:Int,valueOut:Int):Int=
    oper match {
      case ArithmeticOpType.Add =>
        new Flag(prevFlags)
          .set(Flag.H,valueHalf > 0x0FFF)
          .reset(Flag.N)
          .set(Flag.C,valueUnsigned>valueOut)()
      case ArithmeticOpType.AddC => Flag.set(
        Z80Utils.isNegativeWord(valueUnsigned),
        valueOut==0,
        valueHalf > 0x0FFF,
        (valueSigned > 0x7FFF) || (valueSigned < -0x8000),
        n=false,
        valueUnsigned>valueOut
      )
      case ArithmeticOpType.SubC => Flag.set(
        Z80Utils.isNegativeWord(valueUnsigned),
        valueOut==0,
        valueHalf < 0,
        (valueSigned > 0x7FFF) || (valueSigned < -0x8000),
        n=true,
        valueUnsigned<valueOut
      )
      case ArithmeticOpType.Inc | ArithmeticOpType.Dec => prevFlags
  }
}
