package org.kr.scala.z80.system

import scala.annotation.tailrec

class Z80SystemController(override val state:Z80System) extends BaseStateMonad[Z80System](state) {}

object Z80SystemController {
  def apply(state: Z80System):Z80SystemController = new Z80SystemController(state)
  def blank:Z80SystemController = new Z80SystemController(Z80System.blank)

  def run:(Int=>(Z80System=>Z80SystemController))=
    (toGo=>(system=>Z80SystemController(steps(Z80SystemController(system),toGo).state)))

  @tailrec
  private def steps(start:BaseStateMonad[Z80System], toGo:Int):BaseStateMonad[Z80System]={
    toGo match {
      case 0 => Z80SystemController(start.state)
      case _ => steps(start >>= step(),toGo-1)
    }
  }

  private def step: () => Z80System => Z80SystemController = () => system =>
    Z80SystemController(system.step)
}
