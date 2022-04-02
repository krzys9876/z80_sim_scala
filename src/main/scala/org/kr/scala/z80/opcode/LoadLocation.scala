package org.kr.scala.z80.opcode

case class LoadLocation(reg:String, immediate:Int, offsetPC:Int, addressReg:String, directOffset:Int, indirectOffset:Int) {
  override def toString: String =
    this match {
      case LoadLocation(r,_,_,_,_,_) if r!="" => f"$r"
      case LoadLocation(_,i,_,_,_,_) if i!=OpCode.ANY => f"0x$i%02X"
      case LoadLocation(_,_,pco,_,_,_) if pco!=OpCode.ANY => f"(PC+0x${pco+1}%02X,0x$pco%02X)"
      case LoadLocation(_,_,_,r,dirO,indirO) if r!="" =>
        (dirO, indirO) match {
          case (OpCode.ANY,OpCode.ANY) => f"($r)"
          case (o,OpCode.ANY) => f"($r+0x$o%02X)"
          case (OpCode.ANY,o) => f"($r+(PC+0x$o%02X))"
        }
    }
}

object LoadLocation {
  def register(r:String):LoadLocation=LoadLocation(r,OpCode.ANY,OpCode.ANY,"",OpCode.ANY,OpCode.ANY)
  def immediate(i:Int):LoadLocation=LoadLocation("",i,OpCode.ANY,"",OpCode.ANY,OpCode.ANY)
  def indirAddress(a:Int):LoadLocation=LoadLocation("",OpCode.ANY,a,"",OpCode.ANY,OpCode.ANY)
  def registerAddr(r:String):LoadLocation=LoadLocation("",OpCode.ANY,OpCode.ANY,r,OpCode.ANY,OpCode.ANY)
  def registerAddrDirOffset(r:String,o:Int):LoadLocation=LoadLocation("",OpCode.ANY,OpCode.ANY,r,o,OpCode.ANY)
  def registerAddrIndirOffset(r:String,o:Int):LoadLocation=LoadLocation("",OpCode.ANY,OpCode.ANY,r,OpCode.ANY,o)

  def empty:LoadLocation=LoadLocation("",OpCode.ANY,OpCode.ANY,"",OpCode.ANY,OpCode.ANY)
}
