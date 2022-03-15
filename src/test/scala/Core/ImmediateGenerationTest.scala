package Core
import chisel3._
import chisel3.util
import org.scalatest._
import chiseltest._

class ImmediateGenerationTest extends FreeSpec with ChiselScalatestTester{
    "immediate generation test" in {
        test(new ImmediateGeneration()){c=>
        // val a = 0xfc528193.asUInt
        c.io.inst.poke("hfa402323".U)
        c.io.PC.poke(0.U)
        c.clock.step(100)    
        c.io.SB_Imm.expect(-90.S)    
        
        }
    }
}