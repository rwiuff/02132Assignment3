import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt(16.W))
    val dataRead = Input(UInt(32.W))
    val writeEnable = Output(Bool())
    val dataWrite = Output(UInt(32.W))
    val xMax = Input(UInt(16.W))
    val yMax = Input(UInt(16.W))
  })

  // State enum and register
  val idle :: black :: initialise :: xCond :: yCond :: xInc :: borderPix :: setZero :: innerPixel :: done :: Nil =
    Enum(9)
  val stateReg = RegInit(idle)

  // Support registers
  val addressReg = RegInit(0.U(16.W))
  val dataReg = Reg(Vec(32, UInt(32.W)))
  val xReg = RegInit(0.U(32.W))
  val yReg = RegInit(0.U(32.W))

  // Default values
  io.writeEnable := false.B
  io.address := 0.U(16.W)
  io.dataWrite := false.B
  io.done := false.B



  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := black
        addressReg := 0.U(16.W)
      }
    }

    //function here to add black border
    is(black){
      when(xReg == 0){
        io.address = xReg + yReg * 20
        io.writeEnable = true.B
        io.dataWrite = 0.U(32.W)
        stateReg := black
      } .otherwise (xReg == 19){
        io.address = xReg + yReg * 20
        io.writeEnable = true.B
        io.dataWrite = 0.U(32.W)
        stateReg := black
    }
     when(yReg == 0){
      io.address = yReg + xReg*20
      io.writeEnable = true.B
      io.dataWrite = 0.U(32.W)
      stateReg := black
     }.otherwise (yReg == 19){
      io.address = yReg + xReg*20
      io.writeEnable = true.B
      io.dataWrite = 0.U(32.W)
      stateReg := black
     }

      stateReg := initialise
    }


    is(initialise) {
      when(xReg === 59.U(32.W)) {
        xReg := 0.U(32.W)
        stateReg := xCond
      }.otherwise {
        io.address := xReg(16, 0)
        dataReg(xReg) := io.dataRead
        xReg := xReg + 1.U(32.W)
        stateReg := initialise
      }
    }

    is(xCond) {
      when(xReg === 19.U(32.W)) {
        stateReg := done
      }.otherwise {
        stateReg := yCond
      }
    }

    is(yCond) {
      when(yReg === 19.U(32.W)) {
        stateReg := xInc
      }.otherwise {
        addressReg := xReg + 20.U(32.W) * yReg + 400.U(32.W)
        stateReg := borderPix
      }
    }

    is(xInc) {
      yReg := 0.U(32.W)
      xReg := xReg + 1.U(32.W)
      stateReg := xCond
    }

    is(borderPix) {
      when(
        xReg === 0.U(32.W) || yReg === 0.U(32.W) || xReg === 19.U(
          32.W
        ) || yReg === 19.U(32.W)
      ) {
        stateReg := setZero
      }.otherwise {
        stateReg := innerPixel
      }
    }

    is(setZero){
      io.writeEnable := true.B
      io.address := addressReg
      io.dataWrite := 0.U(32.W)
      yReg := yReg + 1.U(32.W)
      stateReg := yCond
    }

    /* is(innerPixel){
      when(dataReg(xReg + 20.U(32.W)) === 0){
        
      } 
    }*/

    // is(read) {
    //   dataMemory.io.address := addressReg
    //   dataReg := Cat(0.U(24.W), ~dataMemory.io.dataRead(7, 0))
    //   stateReg := write
    // }

    // is(write) {
    //   dataMemory.io.address := addressReg + 400.U(16.W)
    //   dataMemory.io.writeEnable := true.B
    //   addressReg := addressReg + 1.U(16.W)
    //   when(addressReg === 399.U(16.W)) {
    //     stateReg := done
    //   }.otherwise {
    //     stateReg := read
    //   }
    // }

    is(done) {
      io.done := true.B
      stateReg := done
    }
  }

}
