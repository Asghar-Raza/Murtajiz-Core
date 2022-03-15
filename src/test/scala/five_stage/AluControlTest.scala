package five_stage
import chisel3._
import chisel3.util
import org.scalatest._
import chiseltest._

class RegFileTest extends FreeSpec with ChiselScalatestTester{
    "alu control test " in {
        test(new AluControl()){c=>
        c.io.func3.poke(1.U)    
        c.io.func7.poke(1.U)    
        c.io.branch.poke(1.U)  
        c.clock.step(100)
        c.io.AluControlOut.expect("b10001".U)  
        }
    }
}