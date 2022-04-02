package org.kr.scala.z80

class OpCodeMap[To](override val mapOfLists:Map[List[OpCode],To],override val defaultTo:To) extends MapHandler[OpCode,To](mapOfLists)  {
  override lazy val defaultFrom:OpCode=>OpCode = opcode => OpCode(opcode.main,OpCode.ANY)
}
