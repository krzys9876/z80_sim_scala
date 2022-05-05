package org.kr.scala.z80.opcode

abstract class OperationSpec {
  val instSize:OpCodeMap[Int]
}
