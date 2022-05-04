package org.kr.scala.z80.opcode

case class Location(reg:String, immediate:Int, offsetPC:Int, addressReg:String, directOffset:Int, indirectOffset2Compl:Int, isWord:Boolean=false) {
  override lazy val toString: String =
    this match {
      case Location(r,_,_,_,_,_,_) if r!="" => f"$r$isWordString"
      case Location(_,i,_,_,_,_,_) if i!=OpCode.ANY => f"$i$isWordString"
      case Location(_,_,pco,_,_,_,_) if pco!=OpCode.ANY => f"(PC+0x$pco%02X)$isWordString"
      case Location(_,_,_,r,dirO,indirO,_) if r!="" =>
        (dirO, indirO) match {
          case (OpCode.ANY,OpCode.ANY) => f"($r)$isWordString"
          case (o,OpCode.ANY) => f"($r+0x$o%02X)$isWordString"
          case (OpCode.ANY,o) => f"($r+(PC+0x$o%02X))$isWordString"
        }
      case _ => "empty"
    }

  lazy val label: String =
    this match {
      case Location(r,_,_,_,_,_,_) if r!="" => f"$r"
      case Location(_,i,_,_,_,_,_) if i!=OpCode.ANY => f"$i$isWordString"
      case Location(_,_,pco,_,_,_,_) if pco!=OpCode.ANY => f"(PC+0x$pco%02X)$isWordString"
      case Location(_,_,_,r,dirO,indirO,_) if r!="" =>
        (dirO, indirO) match {
          case (OpCode.ANY,OpCode.ANY) => f"($r)"
          case (o,OpCode.ANY) => f"($r+0x$o%02X)$isWordString"
          case (OpCode.ANY,o) => f"($r+d)"
        }
      case _ => "empty"
    }

  private val isWordString=if(isWord) "w" else "b"
}

object Location {
  // register
  def register(r:String,isWord:Boolean=false):Location=Location(r,OpCode.ANY,OpCode.ANY,"",OpCode.ANY,OpCode.ANY,isWord)
  // immediate value - fixed for the opcode (e.g. RST)
  def immediate(i:Int,isWord:Boolean=false):Location=Location("",i,OpCode.ANY,"",OpCode.ANY,OpCode.ANY,isWord)
  // a value in a memory address located after OpCode,
  // eg. LD (nn),HL - nn=address of a memory location where contents of HL is loaded
  def indirAddress(a:Int,isWord:Boolean=false):Location=Location("",OpCode.ANY,a,"",OpCode.ANY,OpCode.ANY,isWord)
  // a value in a memory address located in 16-bit register or a register pair
  def registerAddr(r:String,isWord:Boolean=false):Location=Location("",OpCode.ANY,OpCode.ANY,r,OpCode.ANY,OpCode.ANY,isWord)
  // same as registerAddr but with directly specified address offset, e.g. PC+1
  def registerAddrDirOffset(r:String,o:Int,isWord:Boolean=false):Location=Location("",OpCode.ANY,OpCode.ANY,r,o,OpCode.ANY,isWord)
  // same as registerAddr but with address offset located in memory address PC+x,
  // e.g. (IX+d), where d is a value from memory address after OpCode (PC+x)
  def registerAddrIndirOffset(r:String,o:Int,isWord:Boolean=false):Location=Location("",OpCode.ANY,OpCode.ANY,r,OpCode.ANY,o,isWord)

  def empty:Location=Location("",OpCode.ANY,OpCode.ANY,"",OpCode.ANY,OpCode.ANY)
}