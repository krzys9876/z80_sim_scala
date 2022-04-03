package org.kr.scala.z80.opcode

abstract class OperationSpec {
  val instSize:OpCodeMap[Int]
  lazy val isOper: OpCode=>Boolean = opcode => instSize.contains(opcode)
}
