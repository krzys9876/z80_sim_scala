package org.kr.scala.z80.system

class MemoryController(override val state:Memory) extends BaseStateMonad[Memory](state) {
  def >>= (fChangeState: Memory=>MemoryController):MemoryController=fChangeState(state)
}

object MemoryController {

  def apply(state: Memory):MemoryController = new MemoryController(state)
  def blank(size:Int):MemoryController = new MemoryController(Memory.blank(size))

  def poke: (Int, Int) => Memory => MemoryController = (address, value) => memory =>
    MemoryController(Memory(memory.replaceAt(address, value)))
  def pokeMulti: (Int, Vector[Int]) => Memory => MemoryController = (address, values) => memory =>
    MemoryController(Memory(memory.replaceAt(address, values)))
  def loadHexLine:String => Memory => MemoryController = line => memory =>
    MemoryController(Memory(memory.loadHexLine(line)))
  def loadHexLines:List[String] => Memory => MemoryController = lines => memory =>
    MemoryController(Memory(memory.loadHexLines(lines)))


}
