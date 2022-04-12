package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.{Flag, RegisterChange, SystemChangeBase, Z80System}
import org.kr.scala.z80.utils.Z80Utils

object RotateShift extends OperationSpec with OpCodeHandler {
  // Z80 manual page 54 (NOTE: error in OpCode for RCL L and (HL))
  val operationListMap: Map[List[OpCode],ArithmeticOperation] = Map(
    List(OpCode(0xCB,0x07),OpCode(0xCB,0x00),OpCode(0xCB,0x01),OpCode(0xCB,0x02),OpCode(0xCB,0x03),
      OpCode(0xCB,0x04),OpCode(0xCB,0x05),OpCode(0xCB,0x06),OpCode(0xDD,0xCB,0x06),
      OpCode(0xFD,0xCB,0x06)) -> ArithmeticOpType.Rlc,
    List(OpCode(0x07)) -> ArithmeticOpType.Rlca,
    List(OpCode(0xCB,0x0F),OpCode(0xCB,0x08),OpCode(0xCB,0x09),OpCode(0xCB,0x0A),OpCode(0xCB,0x0B),
      OpCode(0xCB,0x0C),OpCode(0xCB,0x0D),OpCode(0xCB,0x0E),OpCode(0xDD,0xCB,0x0E),
      OpCode(0xFD,0xCB,0x0E)) -> ArithmeticOpType.Rrc,
    List(OpCode(0x0F)) -> ArithmeticOpType.Rrca,
    List(OpCode(0xCB,0x17),OpCode(0xCB,0x10),OpCode(0xCB,0x11),OpCode(0xCB,0x12),OpCode(0xCB,0x13),
      OpCode(0xCB,0x14),OpCode(0xCB,0x15),OpCode(0xCB,0x16),OpCode(0xDD,0xCB,0x16),
      OpCode(0xFD,0xCB,0x16)) -> ArithmeticOpType.Rl,
    List(OpCode(0x17)) -> ArithmeticOpType.Rla,
    List(OpCode(0xCB,0x1F),OpCode(0xCB,0x18),OpCode(0xCB,0x19),OpCode(0xCB,0x1A),OpCode(0xCB,0x1B),
      OpCode(0xCB,0x1C),OpCode(0xCB,0x1D),OpCode(0xCB,0x1E),OpCode(0xDD,0xCB,0x1E),
      OpCode(0xFD,0xCB,0x1E)) -> ArithmeticOpType.Rr,
    List(OpCode(0x1F)) -> ArithmeticOpType.Rra,
    List(OpCode(0xCB,0x27),OpCode(0xCB,0x20),OpCode(0xCB,0x21),OpCode(0xCB,0x22),OpCode(0xCB,0x23),
      OpCode(0xCB,0x24),OpCode(0xCB,0x25),OpCode(0xCB,0x26),OpCode(0xDD,0xCB,0x26),
      OpCode(0xFD,0xCB,0x26)) -> ArithmeticOpType.Sla,
    List(OpCode(0xCB,0x2F),OpCode(0xCB,0x28),OpCode(0xCB,0x29),OpCode(0xCB,0x2A),OpCode(0xCB,0x2B),
      OpCode(0xCB,0x2C),OpCode(0xCB,0x2D),OpCode(0xCB,0x2E),OpCode(0xDD,0xCB,0x2E),
      OpCode(0xFD,0xCB,0x2E)) -> ArithmeticOpType.Sra,
    List(OpCode(0xCB,0x3F),OpCode(0xCB,0x38),OpCode(0xCB,0x39),OpCode(0xCB,0x3A),OpCode(0xCB,0x3B),
      OpCode(0xCB,0x3C),OpCode(0xCB,0x3D),OpCode(0xCB,0x3E),OpCode(0xDD,0xCB,0x3E),
      OpCode(0xFD,0xCB,0x3E)) -> ArithmeticOpType.Srl
  )

  val operation: OpCodeMap[ArithmeticOperation] = new OpCodeMap(operationListMap, ArithmeticOpType.None)

  val locationListMap: Map[List[OpCode], LoadLocation] = Map(
    List(OpCode(0x07),OpCode(0x0F),OpCode(0x17),OpCode(0x1F)) -> LoadLocation.register("A"),
    List(OpCode(0xCB,0x06),OpCode(0xCB,0x0E),OpCode(0xCB,0x16),OpCode(0xCB,0x1E),
      OpCode(0xCB,0x26),OpCode(0xCB,0x2E),OpCode(0xCB,0x3E)) -> LoadLocation.registerAddr("HL"),
    List(OpCode(0xDD,0xCB,0x06),OpCode(0xDD,0xCB,0x0E),OpCode(0xDD,0xCB,0x16),OpCode(0xDD,0xCB,0x1E),
      OpCode(0xDD,0xCB,0x26),OpCode(0xDD,0xCB,0x2E),OpCode(0xDD,0xCB,0x3E)
    ) -> LoadLocation.registerAddrIndirOffset("IX",2),
    List(OpCode(0xFD,0xCB,0x06),OpCode(0xFD,0xCB,0x0E),OpCode(0xFD,0xCB,0x16),OpCode(0xFD,0xCB,0x1E),
      OpCode(0xFD,0xCB,0x26),OpCode(0xFD,0xCB,0x2E),OpCode(0xFD,0xCB,0x3E)
    ) -> LoadLocation.registerAddrIndirOffset("IY",2)
  )++
  //registers
    OpCode.generateMapByReg(OpCode(0xCB,0x00),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x08),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x10),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x18),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x20),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x28),2,0)++
    OpCode.generateMapByReg(OpCode(0xCB,0x03),2,0)

  val location: OpCodeMap[LoadLocation] = new OpCodeMap(locationListMap, LoadLocation.empty)

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0x07),OpCode(0x0F),OpCode(0x17),OpCode(0x1F)) ->1,
    List(OpCode(0xCB,0x06),OpCode(0xCB,0x0E),OpCode(0xCB,0x16),OpCode(0xCB,0x1E),OpCode(0xCB,0x26),
      OpCode(0xCB,0x2E),OpCode(0xCB,0x3E)) ->2,
    List(OpCode(0xDD,0xCB,0x06),OpCode(0xFD,0xCB,0x06),OpCode(0xDD,0xCB,0x0E),OpCode(0xFD,0xCB,0x0E),
      OpCode(0xDD,0xCB,0x16),OpCode(0xFD,0xCB,0x16),OpCode(0xDD,0xCB,0x1E),OpCode(0xFD,0xCB,0x1E),
      OpCode(0xDD,0xCB,0x26),OpCode(0xFD,0xCB,0x26),OpCode(0xDD,0xCB,0x2E),OpCode(0xFD,0xCB,0x2E),
      OpCode(0xDD,0xCB,0x3E),OpCode(0xFD,0xCB,0x3E)) ->4,
    OpCode.generateListByReg(OpCode(0xCB,0x00),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x08),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x10),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x18),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x20),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x28),2,0)->2,
    OpCode.generateListByReg(OpCode(0xCB,0x38),2,0)->2
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)

  override def handle(code: OpCode)(implicit system:Z80System):(List[SystemChangeBase],Int) = {
    val oper = operation.find(code)
    val instrSize = instSize.find(code)
    val loc=location.find(code)
    val prevValue=system.getValueFromLocation(loc)
    val prevFlags=system.getRegValue("F")

    val (value, flags) = handleRotateShift(oper, prevValue, prevFlags)

    (List(
      system.putValueToLocation(loc,value),
      new RegisterChange("F", flags)
    ),
      instrSize)
  }

  private def handleRotateShift(operation:ArithmeticOperation,prevValueIn:Int,prevFlags:Int):(Int,Int)={
    val (valueOut,newCarry)=
      doRotateValue(operation,prevValueIn,new Flag(prevFlags).flagValue(Flag.C))
    val newF=calcFlags(operation,valueOut,prevFlags,newCarry)

    (valueOut,newF)
  }

  private def calcFlags(oper:ArithmeticOperation, value:Int, prevF: Int, carry: Boolean)= {
    //http://www.z80.info/z80sflag.htm
    oper match {
      case ArithmeticOpType.Rlca | ArithmeticOpType.Rrca | ArithmeticOpType.Rla |  ArithmeticOpType.Rra =>
        new Flag(prevF).reset(Flag.H).reset(Flag.N).set(Flag.C,carry)()
      case _ => Flag.set(
        Z80Utils.isNegative(value),
        value==0,
        h = false,
        Z80Utils.isEvenBits(value),
        n = false,
        carry)
    }
  }

  private def doRotateValue(oper:ArithmeticOperation, value:Int, carry:Int):(Int,Boolean)= {
    val bit7=Z80Utils.getBit(value,7)
    val bit0=Z80Utils.getBit(value,0)

    val newValue=oper match {
      case ArithmeticOpType.Rlc | ArithmeticOpType.Rlca => ((value << 1) & 0xFF) + (if(bit7) 1 else 0)
      case ArithmeticOpType.Rrc | ArithmeticOpType.Rrca => ((value >> 1) & 0xFF) + (if(bit0) 0x80 else 0)
      case ArithmeticOpType.Rl | ArithmeticOpType.Rla => ((value << 1) & 0xFF) + carry
      case ArithmeticOpType.Rr | ArithmeticOpType.Rra => ((value >> 1) & 0xFF) + (carry << 7)
      case ArithmeticOpType.Sla => (value << 1) & 0xFF
      case ArithmeticOpType.Sra => ((value >> 1) & 0xFF) + (if(bit7) 0x80 else 0)
    }
    val newCarry=oper match {
      case ArithmeticOpType.Rlc | ArithmeticOpType.Rlca | ArithmeticOpType.Sla |
           ArithmeticOpType.Rl | ArithmeticOpType.Rla=> bit7
      case ArithmeticOpType.Rrc | ArithmeticOpType.Rrca | ArithmeticOpType.Sra |
           ArithmeticOpType.Rr | ArithmeticOpType.Rra=> bit0
    }
    (newValue,newCarry)
  }

}

