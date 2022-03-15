package Core
import chisel3._
import chisel3.util._

class Regfile extends Module{
    val io = IO(new Bundle{
        val rs1 = Input(UInt(5.W))
        val rs2 = Input(UInt(5.W))
        val rd = Input(UInt(5.W))
        val writeBack = Input(SInt(32.W))
        val writeEnable = Input(UInt(1.W))
        val Aout = Output(SInt(32.W))
        val Bout = Output(SInt(32.W))
       
    })
    val registers = Reg(Vec(32,SInt(32.W)))
    registers(0) := (0.S)
    when(io.writeEnable === 1.U){
        registers(io.rd) := io.writeBack
    }.otherwise{
        registers(io.rd) := 0.S

    }
    io.Aout := registers(io.rs1)
    io.Bout := registers(io.rs2)
}