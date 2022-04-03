package org.kr.scala.z80.opcode

case class ExchangeLocation(reg1:String,reg1Indirect:Boolean, reg2: String)

object ExchangeLocation {
  val empty:ExchangeLocation=ExchangeLocation("",reg1Indirect=false,"")
}

object Exchange extends OperationSpec {
  val exchangeListMap: Map[List[OpCode],List[ExchangeLocation]] = Map(
    //register pair
    List(OpCode(0xEB, OpCode.ANY)) -> List(ExchangeLocation("DE",reg1Indirect=false,"HL")),
    List(OpCode(0x08, OpCode.ANY)) -> List(ExchangeLocation("AF",reg1Indirect=false,"AF1")),
    List(OpCode(0xD9, OpCode.ANY)) -> List(ExchangeLocation("BC",reg1Indirect=false,"BC1"),
      ExchangeLocation("DE",reg1Indirect=false,"DE1"),ExchangeLocation("HL",reg1Indirect=false,"HL1")),
    List(OpCode(0xE3, OpCode.ANY)) -> List(ExchangeLocation("SP",reg1Indirect=true,"HL")),
  )
  val exchangeLoc: OpCodeMap[List[ExchangeLocation]] = new OpCodeMap(exchangeListMap, List(ExchangeLocation.empty))

  val instructionSizeListMap: Map[List[OpCode], Int] = Map(
    List(OpCode(0xEB, OpCode.ANY),OpCode(0x08, OpCode.ANY),OpCode(0xD9, OpCode.ANY),
      OpCode(0xE3, OpCode.ANY)) -> 1
  )
  override val instSize: OpCodeMap[Int] = new OpCodeMap(instructionSizeListMap, 0)
}
