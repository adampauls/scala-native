package scala.scalanative
package nir

import java.awt.image.PixelInterleavedSampleModel
import java.io.{OutputStream, PrintWriter}
import java.nio._
//import pythonparse._

package object serialization {
  def serializeText(defns: Seq[Defn], buffer: ByteBuffer): Unit = {
    val builder = Show.newBuilder
    builder.defns_(defns)
    buffer.put(builder.toString.getBytes)
  }

  @inline
  private def withBigEndian[T](buf: ByteBuffer)(body: ByteBuffer => T): T = {
    val o = buf.order()
    buf.order(ByteOrder.BIG_ENDIAN)
    try body(buf)
    finally buf.order(o)
  }

  def serializeBinary(defns: Seq[Defn], out: OutputStream): Unit =
    new BinarySerializer().serialize(defns, out)

//  def serializeBinary(defns: Seq[Ast.stmt], out: OutputStream): Unit = {
//    val writer = new PrintWriter(out)
//    for (stat <- defns) {
//      writer.println(stat.render())
//    }
//    writer.close()
//  }

  def deserializeBinary(buffer: ByteBuffer, bufferName: String): Seq[Defn] =
    withBigEndian(buffer) {
      new BinaryDeserializer(_, bufferName).deserialize()
    }
}
