package Core
import chisel3._
import chisel3.util.experimental.loadMemoryFromFile

class DataMemory extends Module{
    val io = IO(new Bundle{
        val DataAddr = Input(UInt(32.W))
        val DataIn = Input(SInt(32.W))
        val store = Input(UInt(1.W))
        val load = Input(UInt(1.W))
        val DataOut = Output(SInt(32.W))
    })
    val mem = Mem(1024,SInt(32.W))
    when(io.load === 1.U){
        io.DataOut:=mem(io.DataAddr)
    }.otherwise{
        io.DataOut:=0.S
    }

    when(io.store===1.U){
        mem(io.DataAddr) := io.DataIn
    }
}