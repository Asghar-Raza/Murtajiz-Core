package Core
import chisel3._
import chisel3.util
import org.scalatest._
import chiseltest._

class InstructionmemoryTest extends FreeSpec with ChiselScalatestTester{
    "inst memory test" in {
        test(new InstructionMemory()){c=>
        c.io.instAddr.poke(0.U)
        c.clock.step(2)
        c.io.instOut.expect(0x03210093.U)
        }
    }
}