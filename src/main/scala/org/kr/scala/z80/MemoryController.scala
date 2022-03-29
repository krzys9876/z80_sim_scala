package org.kr.scala.z80

class MemoryController(override val state:Memory) extends BaseStateMonad[Memory](state) {
}

object MemoryController {
  def apply(state: Memory):MemoryController = new MemoryController(state)
  def blank(size:Int):MemoryController = new MemoryController(Memory.blank(size))

  def poke: (Int, Int) => Memory => MemoryController = (address, value) => memory =>
    MemoryController(Memory(memory.replaceAt(address, value)))
}
