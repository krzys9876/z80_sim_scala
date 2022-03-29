package org.kr.scala.z80

class Register(val reg:Map[String,Int]) {
  def apply(regSymbol:String):Int=reg.getOrElse(regSymbol,0)
  def set(regSymbol:String,value:Int): Register= {
    new Register(reg++Map(regSymbol->value))
  }
}

object Register {
  def blank:Register=new Register(Map())
  def apply(register: Register):Register=new Register(register.reg)
  def getRegCode(numeric:Int):String={
    // Z80 documentation, e.g. page 72
    numeric match {
      case 7 => "A"
      case 0 => "B"
      case 1 => "C"
      case 2 => "D"
      case 3 => "E"
      case 4 => "H"
      case 5 => "L"
    }
  }

}
