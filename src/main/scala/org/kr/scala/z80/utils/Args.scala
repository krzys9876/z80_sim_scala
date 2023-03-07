package org.kr.scala.z80.utils

import org.kr.args.{ArgsAsClass, Argument}

class Args(args: Array[String]) extends ArgsAsClass(args) {
  val mode:Argument[String] = Argument.required
  val hexFile:Argument[String] = Argument.required
  val basicFile:Argument[String] = Argument.optional("")
  val memoryType:Argument[String] = Argument.optional("fast")
  val interrupts:Argument[Boolean] = Argument.optional(true)
  private val stepsM:Argument[Double] = Argument.optional(-1.0)

  parse()

  lazy val steps:Long = if(stepsM() <= 0.0 || stepsM() > (Long.MaxValue/1000000)) Long.MaxValue else (stepsM()*1000000.0).toLong
}
