/* Best-Offset prefetcher (Michaud, HPCA 2016). */
package barf

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem.CacheBlockBytes

case class BestOffsetPrefetcherParams(rrEntries: Int = 64, scoreMax: Int = 31,
    roundMax: Int = 100, badScore: Int = 2, degree: Int = 2,
    offsets: Seq[Int] = (1 to 64)) extends CanInstantiatePrefetcher {
  require((rrEntries & (rrEntries - 1)) == 0 && rrEntries > 0, "rrEntries must be a power of two")
  require(offsets.nonEmpty, "offsets must not be empty")
  def desc = "Best-Offset Prefetcher"
  def instantiate()(implicit p: Parameters) = Module(new BestOffsetPrefetcher(this)(p))
}

/** Dynamically scores candidate cache-line offsets against recent accesses. */
class BestOffsetPrefetcher(params: BestOffsetPrefetcherParams)(implicit p: Parameters)
    extends AbstractPrefetcher()(p) {
  private val blockBits = log2Up(p(CacheBlockBytes))
  private val nOffsets = params.offsets.size
  private val rrIdxBits = log2Ceil(params.rrEntries)
  private val scoreBits = log2Ceil(params.scoreMax + 1)
  private val offsetIdxBits = log2Ceil(nOffsets).max(1)
  private val addressBlockBits = 48
  private val offsets = VecInit(params.offsets.map(_.U))
  private val rrTable = Reg(Vec(params.rrEntries, UInt(addressBlockBits.W)))
  private val rrValid = RegInit(VecInit(Seq.fill(params.rrEntries)(false.B)))
  private val scores = RegInit(VecInit(Seq.fill(nOffsets)(0.U(scoreBits.W))))
  private val testPtr = RegInit(0.U(offsetIdxBits.W))
  private val roundCnt = RegInit(0.U(log2Ceil(params.roundMax + 1).max(1).W))
  private val bestOffsetIdx = RegInit(0.U(offsetIdxBits.W))
  private val bestOffsetValid = RegInit(false.B)
  private def rrIdx(block: UInt): UInt = block(rrIdxBits - 1, 0)
  private val snoopBlock = io.snoop.bits.block

  when (io.snoop.valid) {
    val writeIndex = rrIdx(snoopBlock)
    rrTable(writeIndex) := snoopBlock
    rrValid(writeIndex) := true.B
    val priorBlock = snoopBlock - offsets(testPtr)
    val priorIndex = rrIdx(priorBlock)
    when (rrValid(priorIndex) && rrTable(priorIndex) === priorBlock && scores(testPtr) < params.scoreMax.U) {
      scores(testPtr) := scores(testPtr) + 1.U
    }
    testPtr := Mux(testPtr === (nOffsets - 1).U, 0.U, testPtr + 1.U)
    roundCnt := roundCnt + 1.U
    val anyMaxed = scores.map(_ >= params.scoreMax.U).reduce(_ || _)
    when (roundCnt >= (params.roundMax - 1).U || anyMaxed) {
      val (bestScore, bestIndex) = scores.zipWithIndex.tail.foldLeft((scores(0), 0.U(offsetIdxBits.W))) {
        case ((scoreSoFar, indexSoFar), (score, index)) =>
          (Mux(score > scoreSoFar, score, scoreSoFar), Mux(score > scoreSoFar, index.U, indexSoFar))
      }
      bestOffsetValid := bestScore >= params.badScore.U
      when (bestScore >= params.badScore.U) { bestOffsetIdx := bestIndex }
      for (index <- 0 until nOffsets) { scores(index) := 0.U }
      roundCnt := 0.U
    }
  }

  private val prefBase = RegInit(0.U(addressBlockBits.W))
  private val prefCount = RegInit(params.degree.U(log2Ceil(params.degree + 1).max(1).W))
  when (io.request.fire) { prefCount := prefCount + 1.U }
  when (io.snoop.valid && bestOffsetValid && !io.snoop.bits.write) {
    prefBase := snoopBlock
    prefCount := 0.U
  }
  private val prefetchBlock = prefBase +& ((prefCount +& 1.U) * offsets(bestOffsetIdx))
  io.request.valid := prefCount < params.degree.U && !prefetchBlock(addressBlockBits)
  io.request.bits.address := prefetchBlock(addressBlockBits - 1, 0) << blockBits
  io.request.bits.write := false.B
}
