package org.kr.scala.z80.utils

//TODO: determine if this is needed
case class SysInt(value: Int, any:Boolean=false) {
  def apply():Int=value
}

object SysInt {
  val ANY:SysInt=SysInt(Int.MinValue,any=true)
}
