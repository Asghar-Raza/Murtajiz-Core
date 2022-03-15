package Core
import chisel3._
import chisel3.util
import org.scalatest._
import chiseltest._

class TopTest extends FreeSpec with ChiselScalatestTester{
    "top module test" in {
        test(new Top()){c=>
            // c.io.pc.expect(0.U)    
        // c.io.Instruction.expect("h00a00193".U)    
        // c.io.rs1.expect(0.U)    
        // c.io.rs2.expect(-5.U)    
        // c.io.CoreAluA.expect(15.S)    
        // c.io.CoreAluB.expect(-5.S)    
        c.clock.step(100)
        // c.io.writeBack.expect(10.S)       
        // c.io.AluResult.expect(11.S)    
        }
    }
}