package org.kr.scala.z80.opcode

abstract class LoadSpec extends OperationSpec {
  val sourceLoc:OpCodeMap[LoadLocation]
  val destLoc:OpCodeMap[LoadLocation]
}