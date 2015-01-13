package com.colingodsey.logos

import java.io.InputStream
import java.nio.ByteBuffer

import scala.collection.IndexedSeqOptimized

package object collections {
	object IVec3 {
		def apply(point: Vec3): IVec3 = IVec3(
			math.floor(point.x).toInt,
			math.floor(point.y).toInt,
			math.floor(point.z).toInt)
	}

	final case class IVec3(x: Int, y: Int, z: Int) {
		def toVec3 = Vec3(x, y, z)

		override lazy val hashCode = (x, y, z).hashCode
	}

	implicit def IPoint3DToPoint3D(x: IVec3) = x.toVec3

	final implicit class ByteBufferSeq(val buf: ByteBuffer) extends IndexedSeq[Byte]
			with IndexedSeqOptimized[Byte, IndexedSeq[Byte]] {
		def apply(idx : Int): Byte = buf.get(idx + buf.position)
		def length = buf.remaining

		override def stringPrefix: String = "ByteBufferSeq"

		override def seq = this

		/*override def slice(start: Int, end: Int): IndexedSeq[Byte] =
			(ByteString.empty ++ this).slice(start, end)*/
	}


	/*implicit class ActorLoggingExt(val ac: LoggingAdapter) extends AnyVal {
		def ifdebug(msg: => String) {
			if(ac.isDebugEnabled) ac.debug(msg)
		}
	}*/

	implicit class SeqInputStream(seq: Iterable[Byte]) extends InputStream {
		private val iterator = seq.iterator

		private var _position = 0

		def position = _position

		def read: Int = {
			if(!iterator.hasNext) return -1

			_position += 1

			iterator.next & 0xFF
		}
	}
}
