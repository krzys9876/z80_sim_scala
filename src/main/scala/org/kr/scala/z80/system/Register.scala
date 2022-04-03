package org.kr.scala.z80.system

import org.kr.scala.z80.utils.Z80Utils

class Register(val reg:Map[String,Int]) {
  def apply(regSymbol:String):Int=
    regSymbol match {
      case "AF" | "BC" | "DE" | "HL" =>
        Z80Utils.makeWord(getRaw(regSymbol.substring(0, 1)), getRaw(regSymbol.substring(1, 2)))
      case _ => getRaw(regSymbol)
    }

  private def getRaw(regSymbol:String):Int=reg.getOrElse(regSymbol, 0)

  def set(regSymbol:String,value:Int): Register= {
    val newRegMap=regSymbol match {
      case "AF" | "BC" | "DE" | "HL" =>
        Map(regSymbol.substring(0,1)->Z80Utils.getH(value),
          regSymbol.substring(1,2)->Z80Utils.getL(value))
      case _ => Map(regSymbol->value)
    }
    new Register(reg++newRegMap)
  }

  def setRelative(regSymbol:String,relativeValue:Int): Register=
    set(regSymbol,Z80Utils.add16bit(apply(regSymbol),relativeValue))
  def movePC(forward:Int): Register=
    setRelative("PC",forward)
}

object Register {
  def blank:Register=new Register(Map())
  def apply(register: Register):Register=new Register(register.reg)
}