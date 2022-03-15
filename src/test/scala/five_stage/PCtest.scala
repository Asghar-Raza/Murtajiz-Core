package five_stage
import chisel3._
import chisel3.util
import org.scalatest._
import chiseltest._

class PCtest extends FreeSpec with ChiselScalatestTester{
    "program counter test" in {
        test(new PC()){c=>
        c.io.input.poke(8.U)   
        c.clock.step(100) 
        c.io.pc.expect(8.U)    
        c.io.pc4.expect(12.U)    
        }
    }
}