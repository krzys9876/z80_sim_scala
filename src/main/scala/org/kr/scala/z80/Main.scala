package org.kr.scala.z80

object Main extends App {
  /*val s=MemoryController.blank(10)
  val s1 = s >>= MemoryController.poke(1,2) >>= MemoryController.poke(3,123) >>= (mem=>MemoryController(mem.replaceAt(7,77)))
  print(s1.state.mem)*/

  val z=Z80SystemController.blank
  println(z.state.registerController.get("PC"))
  val zAfter1=z >>= Z80SystemController.run(1)
  println(zAfter1.state.registerController.get("PC"))
  val zAfter2=zAfter1 >>= Z80SystemController.run(65536*100)
  println(zAfter2.state.registerController.get("PC"))
}