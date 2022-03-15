package five_stage
import chisel3._
import chisel3.util
import org.scalatest._
import chiseltest._

class ControlUnitTest extends FreeSpec with ChiselScalatestTester{
    "control unit test" in {
        test(new ControlUnit()){c=>
        c.io.Opcode.poke("b1100111".U)
        c.clock.step(100)
        c.io.Branch.expect(0.U)    
        c.io.RegWrite.expect(0.U)    
        c.io.Immediate.expect(0.U)    
        c.io.Load.expect(0.U)    
        c.io.Store.expect(0.U)    
        c.io.UType.expect(0.U)    
        c.io.Auipc.expect(0.U)    
        c.io.Jalr.expect(1.U)    
        c.io.jal.expect(0.U)    
        }
    }
}