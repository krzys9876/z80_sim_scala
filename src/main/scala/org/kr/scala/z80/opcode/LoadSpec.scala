package org.kr.scala.z80.opcode

abstract class LoadSpec(override protected val operationType: OpType) extends OperationSpec(operationType) {
  val sourceLoc:OpCodeMap[LoadLocation]
  val destLoc:OpCodeMap[LoadLocation]
}