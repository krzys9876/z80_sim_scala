package org.kr.scala.z80.opcode

class ExchangeLocationBase (val reg1:String,val reg2: String)
class ExchangeLocation(override val reg1:String,override val reg2: String) extends ExchangeLocationBase(reg1,reg2)
class ExchangeLocationIndirect(override val reg1:String,override val reg2: String) extends ExchangeLocationBase(reg1,reg2)

object ExchangeLocationBase {
  val empty:ExchangeLocationBase=new ExchangeLocationBase("","")
}

object Exchange extends OperationSpec(OpType.ExchangeType) {
  // Z80 manual page 47
  val exchangeListMap: Map[List[OpCode],List[ExchangeLocationBase]] = Map(
    //register pair
    List(OpCode(0xEB)) -> List(new ExchangeLocation("DE","HL")),
    List(OpCode(0x08)) -> List(new ExchangeLocation("AF","AF1")),
    List(OpCode(0xD9)) -> List(new ExchangeLocation("BC","BC1"),
      new ExchangeLocation("DE","DE1"),new ExchangeLocation("HL","HL1")),
    //register with memory at SP
    List(OpCode(0xE3)) -> List(new ExchangeLocationIndirect("SP","HL")),
    List(OpCode(0xDD, 0xE3)) -> List(new ExchangeLocationIndirect("SP","IX")),
    List(OpCode(0xFD, 0xE3)) -> List(new ExchangeLocationIndirect("SP","IY"))
  )
  val exchangeLoc: OpCodeMap[List[ExchangeLocationBase]] = new OpCodeMap(exchangeListMap, List(ExchangeLocationBase.empty))

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xEB),OpCode(0x08),OpCode(0xD9),
      OpCode(0xE3)) -> 1,
    List(OpCode(0xDD, 0xE3),OpCode(0xFD, 0xE3)) -> 2
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}
