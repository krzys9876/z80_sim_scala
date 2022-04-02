package org.kr.scala.z80.opcode

abstract class LoadSpec {
  val sourceLoc:OpCodeMap[LoadLocation]
  val destLoc:OpCodeMap[LoadLocation]
  val instSize:OpCodeMap[Int]
  lazy val isOper: OpCode=>Boolean = opcode => instSize.contains(opcode)
}