package com.colingodsey.logos.cla.shadow

import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.reflect.ClassTag

import scala.scalajs.concurrent.JSExecutionContext

import scala.scalajs.js
import scala.scalajs.js.typedarray.{Int32Array, Int8Array}

object VMImpl {
   def distributedExec[T](chunkSize: Int, items: Iterable[T])(f: T => Unit): Unit =
    items foreach f

   def newDefaultExecutionContext: ExecutionContext = JSExecutionContext.queue

    def newNativeArray[T: ClassTag](length: Int): mutable.IndexedSeq[T] = {
      implicitly[ClassTag[T]].runtimeClass match {
        case _ if !scalajs.runtime.Bits.areTypedArraysSupported =>
          new js.Array[T](length)
        case java.lang.Byte.TYPE =>
          new Int8Array(length).asInstanceOf[js.Array[T]]
        case java.lang.Integer.TYPE =>
          new Int32Array(length).asInstanceOf[js.Array[T]]
        case _ if !scalajs.runtime.Bits.areTypedArraysSupported =>
          new js.Array[T](length)
      }
    }

 }
