package org.kr.scala.z80.opcode

abstract class OperationSpec(protected val operationType:OpType) {
  val instSize:OpCodeMap[Int]
  lazy val isOper: OpCode=>Boolean = opcode => instSize.contains(opcode)
  lazy val opType: OpCode=>Option[OpType] = opcode => if(isOper(opcode)) Some(operationType) else None
}
