package Core
import chisel3._
import chisel3.util
import org.scalatest._
import chiseltest._

class RegFileTest1 extends FreeSpec with ChiselScalatestTester{
    "reg file test" in {
        test(new Regfile()){c=>
        c.io.rs1.poke(1.U)    
        c.io.rs2.poke(31.U)    
        c.io.rd.poke(31.U)    
        c.io.writeBack.poke(100.S)    
        c.io.writeEnable.poke(1.U)    
        c.clock.step(100) 
        c.io.Aout.expect(0.S)    
        c.io.Bout.expect(100.S)    
        }
    }
}