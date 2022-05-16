package org.kr.scala.z80.opcode

import org.kr.scala.z80.system.RegSymbol


abstract class Location(val isWord:Boolean=false) {
  val label: String
  val isWordString:String=if(isWord) "w" else "b"
}

class IncorrectLocation(message : String) extends Exception(message)

case class EmptyLocation() extends Location() {
  override lazy val label:String="empty"
}

case class RegisterLocation(register:RegSymbol)
  extends Location(false) {
  override lazy val label:String=register.toString
}

case class ImmediateLocation(immediate:Int,override val isWord:Boolean=false)
  extends Location(isWord) {
  override lazy val label:String=f"$immediate$isWordString"
}

case class IndirectAddrLocation(offsetPC:Int, override val isWord:Boolean=false)
  extends Location(isWord) {
  override lazy val label:String=f"(PC+0x$offsetPC%02X)$isWordString"
}

case class RegisterAddrLocation(addressReg:RegSymbol, override val isWord:Boolean=false)
  extends Location(isWord) {
  override lazy val label:String=f"(${addressReg.toString})"
}

case class RegisterAddrDirOffsetLocation(addressReg:RegSymbol, directOffset:Int, override val isWord:Boolean=false)
  extends Location(isWord) {
  override lazy val label:String=f"(${addressReg.toString}+0x$directOffset%02X)$isWordString"
}

case class RegisterAddrIndirOffsetLocation(addressReg:RegSymbol, indirectOffset2Compl:Int, override val isWord:Boolean=false)
  extends Location(isWord) {
  override lazy val label:String=f"(${addressReg.toString}+d)"
}