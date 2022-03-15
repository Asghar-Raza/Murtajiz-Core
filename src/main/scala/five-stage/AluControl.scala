package five_stage
import chisel3._
import chisel3.util._

class AluControl extends Module{
    val io = IO(new Bundle{
        val func3 = Input(UInt(3.W))
        val func7 = Input(UInt(1.W))
        val branch = Input(UInt(1.W))
        val AluControlOut = Output(UInt(5.W))
    })
    val out1 = Cat(0.U,io.func7,io.func3)
    val out2 = Cat(2.U,io.func3)
    when(io.branch === 1.U){
        io.AluControlOut := out2
    }.otherwise{
        io.AluControlOut := out1
        
    }
}