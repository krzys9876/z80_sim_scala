package org.kr.scala.z80.test

import org.kr.scala.z80.system._
import org.scalatest.funsuite.AnyFunSuite

class MemoryTest extends AnyFunSuite {
  implicit val debugger:Debugger=DummyDebugger

  testAll(new ImmutableMemoryHandler(),"immutable")
  testAll(new MutableMemoryHandler(),"mutable")

  def testAll(implicit memoryHandler: MemoryHandler,prefix:String):Unit = {
    test(f"init blank memory ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.blank(5))
      //when
      //then
      assert(memoryController.get.copy.equals(Vector[Int](0, 0, 0, 0, 0)))
    }

    test(f"init preloaded memory ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.preloaded(Vector[Int](1, 2, 3), 4))
      //when
      //then
      assert(memoryController.get.copy.equals(Vector[Int](1, 2, 3, 0)))
    }

    test(f"poke memory ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.blank(5))
      //when
      val afterState = memoryController >>== memoryHandler.poke(1, 34) >>== memoryHandler.poke(4, 56) >>== memoryHandler.poke(0, 12)
      //then
      assert(afterState.get.copy.equals(Vector[Int](12, 34, 0, 0, 56)))
    }

    test(f"poke multiple values ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.blank(5))
      //when
      val afterState = memoryController >>== memoryHandler.pokeMulti(1, Vector(34, 56, 78))
      //then
      assert(afterState.get.copy.equals(Vector[Int](0, 34, 56, 78, 0)))
    }

    test(f"poke word ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.blank(5))
      //when
      val afterState = memoryController >>== memoryHandler.pokeW(2, 0x1234)
      //then
      assert(afterState.get.copy.equals(Vector[Int](0, 0, 0x34, 0x12, 0)))
    }

    test(f"poke word with overflow (16bit) ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.blank(0x10000))
      //when
      val afterState = memoryController >>== memoryHandler.pokeW(0xFFFF, 0x1234)
      //then
      assert(afterState.get.copy(0xFFFF) == 0x34)
      assert(afterState.get.copy(0x0000) == 0x12)
    }

    test(f"poke multi wih lock and overflow (16bit) ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.blank(10))
      //when
      val afterState = memoryController >>== memoryHandler.lockTo(4) >>== memoryHandler.pokeMulti(8, Vector(1, 2, 3, 4, 5, 6, 7, 8, 9))
      //then
      //println(afterState.get.mem)
      assert(afterState.get.copy.equals(Vector[Int](0, 0, 0, 0, 7, 8, 9, 0, 1, 2)))
    }

    test(f"poke  multi with fully locked memory ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.blank(10)) >>== memoryHandler.lockTo(10)
      //when
      val afterState = memoryController >>== memoryHandler.pokeMulti(8, Vector(1, 2, 3, 4, 5, 6, 7, 8, 9))
      //then
      //println(afterState.get.mem)
      assert(afterState.get.copy.equals(Vector[Int](0, 0, 0, 0, 0, 0, 0, 0, 0, 0)))
    }


    test(f"poke memory direct function declaration ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.blank(5))
      //when
      val afterState = memoryController >>== memoryHandler.poke(2, 67)
      //then
      assert(afterState.get.copy.equals(Vector[Int](0, 0, 67, 0, 0)))
    }

    test(f"lock lower memory ($prefix)") {
      //given
      val memoryController = StateWatcher[MemoryContents](memoryHandler.blank(10)) >>==
        memoryHandler.pokeMulti(0,Vector(100, 101, 102, 103, 104, 105, 106, 107, 108, 109)) >>==
          memoryHandler.lockTo(5)
      //when
      val memoryAfter =
        memoryController >>==
          memoryHandler.poke(0, 200) >>==
          memoryHandler.pokeMulti(1, Vector(201, 202)) >>==
          memoryHandler.pokeMulti(3, Vector(203, 204, 205, 206)) >>==
          memoryHandler.pokeMulti(7, Vector(207, 208)) >>==
          memoryHandler.poke(9, 209)
      //then
      assert(memoryAfter.get.copy.equals(Vector[Int](100, 101, 102, 103, 104, 205, 206, 207, 208, 209)))
    }
  }
}
