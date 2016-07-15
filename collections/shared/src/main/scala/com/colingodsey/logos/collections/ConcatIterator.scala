package com.colingodsey.logos.collections


class ConcatIterator[T](itrs: Iterator[Iterator[T]]) extends Iterator[T] {
  private var curItr: Iterator[T] = null

  def hasNext: Boolean = (curItr != null && curItr.hasNext) || itrs.hasNext

  def next(): T = {
    if(curItr == null) curItr = itrs.next

    val x = curItr.next()

    //if(!curItr.hasNext)
    ???
  }
}

/*
@inline private def itrComp[T](): Iterator[T] = new Iterator[T] {

    }
 */