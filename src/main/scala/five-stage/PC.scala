package five_stage
import chisel3._
import chisel3.util._

class PC extends Module{
    val io = IO(new Bundle{
        val input = Input(UInt(32.W))
        val pc4 = Output(UInt(32.W))
        val pc = Output(UInt(32.W))
    })

    val reg = RegNext(0.U(32.W))
    io.pc := reg 
    reg := io.input
    io.pc4 := reg + 4.U
    
}