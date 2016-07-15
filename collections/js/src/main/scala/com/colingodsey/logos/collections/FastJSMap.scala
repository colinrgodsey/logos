package com.colingodsey.logos.collections

import scala.annotation.tailrec
import scala.collection.generic.MutableMapFactory
import scala.collection.{mutable, CustomParallelizable, immutable}

import scala.scalajs.js

//TODO: builder
//TODO: overall worried that startIdx might get lost while using empty
sealed trait FastJSMap[+B] extends immutable.AbstractMap[String, B]
    with immutable.Map[String, B]
    with immutable.MapLike[String, B, FastJSMap[B]] { self =>

  override def size: Int
  override def contains(key: String): Boolean
  override def head: (String, B)
  override def +[B1 >: B](pair: (String, B1)): FastJSMap[B1]

  //always makes sure you're appending something with an equal or greater start index
  protected def append[B1 >: B](other: FastJSMap[B1]): FastJSMap[B1]

  def startIdx: Int

  protected def addWithStartIdx[B1 >: B](pair: (String, B1), idx: Int): FastJSMap[B1] =
    this + pair

  override def empty: FastJSMap[B] = FastJSMap.empty

  //TODO: we should really set this as final and make a new method for doing this with a min startIdx between the two
  final def ++[B1 >: B](other: FastJSMap[B1]): FastJSMap[B1] = {
    if(other.isEmpty) this
    else if(this.isEmpty) other
    else if(other.startIdx < startIdx) other ++ this
    else append(other)
  }
/*
  def keyIteratorIterator: Iterator[Iterator[String]] =

  def fastKeyIterator: Iterator[String] = {
    if(size == 0) Iterator.empty
    else new Iterator[String] {
      val stack = new js.Array[Node]
      var curItr: Iterator[String] = self.iterator

      @inline def last = stack(stack.length - 1)

      //stream?
      def inner(node: Node): Unit = {
        stack.push(node)

        node match {
          case x: FastJSMap.Leaf[B] =>
            stack push x
          case x: FastJSMap.Branch[B] =>
            stack push x.left
            stack push x.right
          case FastJSMap.Empty =>

        }
      }

      def advanceNode(): Unit = mast match {
        case Branch
      }

      def hasNext: Boolean = (!curItr && curItr.hasNext) ||

      def next(): B = ???
    }

  }*/

  def compact: FastJSMap[B]
}

object FastJSMap /*extends MutableMapFactory[FastJSMap] */{
  val safeHasOwnProperty =
    js.Dynamic.global.Object.prototype.hasOwnProperty
        .asInstanceOf[js.ThisFunction1[js.Dictionary[_], String, Boolean]]
  /*@inline def safeHasOwnProperty(obj: js.Any, key: String) =
    js.Dynamic.global.Object.prototype.hasOwnProperty.call(obj, key).asInstanceOf[Boolean]*/
  /*@inline def safeHasOwnProperty(obj: js.Any, key: String) =
    js.typeOf(obj.asInstanceOf[js.Dynamic].selectDynamic(key)) != "undefined"*/

  final val MaxLeafSize = 32
  final val MinBranchSize = 16

  private def apply[B](tr: TraversableOnce[(String, B)]): FastJSMap[B] = {
    val dict = js.Dictionary.empty[B]

    var size = 0
    tr foreach { x =>
      if(!safeHasOwnProperty(dict, x._1)) {
        dict += x
        size += 1
      }
    }

    apply(dict, size, 0)
  }

  private def apply[B](dict: js.Dictionary[B], size: Int, startIdx: Int): FastJSMap[B] =
    if(size < 0) sys.error("oh god why it burns it burnnnssss")
    else if(size == 0) Empty
    else if(size >= MaxLeafSize) Branch(dict, startIdx)
    else Leaf(dict, size, startIdx)

  def apply[B](pairs: (String, B)*): FastJSMap[B] = apply(pairs)

  def empty[B]: FastJSMap[B] = Empty

  @inline private def charCodeAt(str: String, idx: js.Any): Int =
    str.asInstanceOf[js.Dynamic].charCodeAt(idx).asInstanceOf[Int]

  //returns + if thiz > that, - if thiz < that
  private def partialStringCompare(thiz: String, that: String, offset: Int, limit: Int = -1): Int = {
    if(limit == 0) return 0

    var idx = math.min(thiz.length - offset, that.length - offset) - 1
    var strIdx = offset
    var count = 0

    while(idx >= 0 && (limit == -1 || count < limit)) {
      //TODO: check if we should use JS charCodeAt or if scala one is fine
      val x = charCodeAt(thiz, strIdx) - charCodeAt(that, strIdx)

      if(x != 0) return x

      idx -= 1
      strIdx += 1
      count += 1
    }

    if(limit == -1)
      thiz.length - that.length
    else
      math.max(thiz.length - offset, limit) - math.max(that.length - offset, limit)
  }

  private def numSimilarChars(thiz: String, that: String, offset: Int, limit: Int = -1): Int = {
    var i = 0

    @inline def hasChars = (offset + i) < thiz.length &&
        (offset + i) < that.length && (limit == -1 || i < limit)
    @inline def charsEqual = charCodeAt(thiz, offset + i) == charCodeAt(that, offset + i)

    while(hasChars && charsEqual) i += 1

    i
  }

  //TODO: this needs a startIdx
  private case object Empty extends FastJSMap[Nothing] {
    def get(key: String): Option[Nothing] = None

    def +[B1 >: Nothing](kv: (String, B1)): FastJSMap[B1] = FastJSMap(kv)

    def iterator: Iterator[(String, Nothing)] = Iterator.empty

    def -(key: String): FastJSMap[Nothing] = Empty

    def startIdx = Int.MaxValue
    override def size: Int = 0
    override def contains(key: String): Boolean = false
    override protected def addWithStartIdx[B1 >: Nothing](
        pair: (String, B1), idx: Int): FastJSMap[B1] = {
      val dict = js.Dictionary.empty[B1]

      dict(pair._1) = pair._2

      FastJSMap(dict, 1, idx)
    }

    def compact: Empty.type = this

    def append[B1 >: Nothing](other: FastJSMap[B1]): FastJSMap[B1] = other
  }

  private object Branch {
    def apply[B](dict: js.Dictionary[B], startIdx: Int, pair: Option[(String, B)] = None): Branch[B] = {
      val keys = js.Object.keys(dict.asInstanceOf[js.Object])

      require(keys.length >= 3, "must have at least 3 keys")

      //sorts smallest to the left
      val sorted = keys.sorted(new Ordering[String] {
        def compare(x: String, y: String): Int =
          partialStringCompare(x, y, startIdx)
      })

      val centerIdx = sorted.length / 2
      val centerKey = sorted(centerIdx)
      val centerValue = dict(centerKey)

      var leftSize = centerIdx
      var rightSize = sorted.length - centerIdx - 1

      //use head of sorted because its smaller
      val commonChars = sorted.iterator.map(numSimilarChars(_, sorted.head, startIdx)).min

      val left = js.Dictionary.empty[B]
      val right = js.Dictionary.empty[B]

      var i = 0
      while (i < sorted.length) {
        val key = sorted(i)

        if(i < centerIdx) left(key) = dict(key)
        else if(i > centerIdx) right(key) = dict(key)

        i += 1
      }

      if(pair != None) {
        val (key, value) = pair.get
        val addComp = partialStringCompare(key, centerKey, startIdx)

        require(addComp != 0, "should never happen, constructor is not made for replacing")

        if(addComp > 0) { //right
          right(key) = value
          rightSize += 1
        } else {
          left(key) = value
          leftSize += 1
        }
      }

      val leftNode = FastJSMap(left, leftSize, startIdx + commonChars)
      val rightNode = FastJSMap(right, rightSize, startIdx + commonChars)

      Branch(leftNode, centerKey, centerValue, rightNode, startIdx, commonChars)

    }
  }

  //startIdx should never change
  private case class Branch[+B](left: FastJSMap[B], centerKey: String, centerValue: B,
      right: FastJSMap[B], startIdx: Int, commonChars: Int) extends FastJSMap[B] {
    override val size: Int = left.size + 1 + right.size

    //require(commonChars == 0, "broken for now...")

    def get(key: String): Option[B] = {
      val comp = partialStringCompare(key, centerKey, startIdx)

      comp match {
        case _ if comp > 0 => //right
          right.get(key)
        case 0 => //center
          Some(centerValue)
        case _ if comp < 0 => //left
          left.get(key)
      }
    }

    //TODO: add compaction logic using size
    def -(key: String): FastJSMap[B] = {
      val comp = partialStringCompare(key, centerKey, startIdx)

      val out = comp match {
        case _ if comp > 0 => //right
          val newRight = right - key

          if(!(newRight eq right)) copy(right = newRight)
          else this
        case _ if comp < 0 => //left
          val newLeft = left - key

          if(!(newLeft eq left)) copy(left = newLeft)
          else this
        case 0 => (left, right) match { //center
          case _ if (size - 1) < MinBranchSize =>
            //if its small enough, just compact into a leaf minus the center
            _compact(centerKey)
          case (Empty, _) => right
          case (_, Empty) => left
          case (left: Leaf[B], _) => right ++ left
          case (_, right: Leaf[B]) => left ++ right
          case (left: Branch[B], right: Branch[B]) =>
            //use head. head should be sorted on the fast maps
            val (key, value) = right.head
            Branch(left, key, value, right - key, startIdx, 0)
        }

      }

      //TODO: should we move this to before the addition?
      out.compact
    }

    def +[B1 >: B](pair: (String, B1)): Branch[B1] = {
      val key = pair._1
      val value = pair._2

      val numCommon = numSimilarChars(centerKey, key, startIdx, commonChars)
      val doesntShareCommon = numCommon < commonChars
      val comp = partialStringCompare(key, centerKey, startIdx + numCommon)

      def withoutCommon = this.copy(startIdx = startIdx + numCommon,
        commonChars = commonChars - numCommon)

      comp match {
        case 0 =>
          copy(centerKey = key, centerValue = value)
        case _ if doesntShareCommon && comp > 0  =>
          Branch(Empty, key, value, withoutCommon, startIdx, numCommon)
        case _ if doesntShareCommon && comp < 0  =>
          Branch(withoutCommon, key, value, Empty, startIdx, numCommon)
        case _ if comp > 0 =>
          copy(right = right.addWithStartIdx(pair, startIdx + commonChars))
        case _ if comp < 0 =>
          copy(left = left.addWithStartIdx(pair, startIdx + commonChars))
        case _ => sys.error(s"unexpected $comp $doesntShareCommon")
      }
    }

    def compact: FastJSMap[B] = _compact(null)

    private def _compact(minus: String): FastJSMap[B] = if(size < FastJSMap.MinBranchSize) {
      val dict = js.Dictionary.empty[B]

      foreach(pair => dict(pair._1) = pair._2)

      if(minus != null) dict.delete(minus)

      Leaf(dict, if(minus == null) size - 1 else size, startIdx)
    } else this

    override def keysIterator = left.keysIterator ++
        Iterator.single(centerKey) ++
        right.keysIterator

    def iterator: Iterator[(String, B)] =
      left.iterator ++
          Iterator.single(centerKey -> centerValue) ++
          right.iterator

    override def head: (String, B) = if(left.isEmpty) right.head else left.head

    override def contains(key: String): Boolean = {
      val comp = partialStringCompare(key, centerKey, startIdx)
      if(comp > 0) right.contains(key)
      else if(comp == 0) true
      else left.contains(key)
    }

    //TODO: optimized
    def append[B1 >: B](other: FastJSMap[B1]): FastJSMap[B1] = {
      var acc: FastJSMap[B1] = this

      other.foreach(acc += _)

      acc
    }
  }

  private case class Leaf[+B](
      val dict: js.Dictionary[_],
      override val size: Int,
      val startIdx: Int) extends FastJSMap[B] { _this =>
    require(size != 0, "Leaf must have size!")

    @inline final def jsKeys: IndexedSeq[String] = js.Object.keys(objectDict)

    @inline private def objectDict = dict.asInstanceOf[js.Object]
    @inline private def dynamicDict = dict.asInstanceOf[js.Dynamic]
    @inline private def typedRawGet(key: String): B = dynamicDict.selectDynamic(key).asInstanceOf[B]
    @inline private def typedDict[T] = dict.asInstanceOf[js.Dictionary[T]]

    def get(key: String) = if(contains(key)) Some(typedRawGet(key)) else None

    def +[B1 >: B](pair: (String, B1)): FastJSMap[B1] = {
      val (key, value) = pair
      val newKey = !contains(key)

      if(size >= MaxLeafSize && newKey) { //split
        Branch[B1](typedDict[B1], startIdx, Some(pair))
      } else {
        val copy = js.Dictionary.empty[B1]
        for(x <- jsKeys) copy(x) = typedRawGet(x)

        copy(key) = value

        if(newKey) Leaf(copy, size + 1, startIdx)
        else Leaf(copy, size, startIdx)
      }
    }

    override def head: (String, B) = {
      val min = jsKeys.min

      min -> typedRawGet(min)
    }

    override def keysIterator = jsKeys.iterator

    def iterator: Iterator[(String, B)] = jsKeys.iterator.map(key => (key, typedRawGet(key)))

    def -(key: String): FastJSMap[B] = if(contains(key)) {
      if(size == 1) Empty
      else {
        val newDict = js.Dictionary.empty[B]
        for(x <- jsKeys) newDict(x) = typedRawGet(x)

        newDict.delete(key)

        copy(dict = newDict, size = size - 1)
      }
    } else this

    @inline final override def contains(key: String): Boolean =
      safeHasOwnProperty(dict, key)

    def compact = this

    def append[B1 >: B](other: FastJSMap[B1]): FastJSMap[B1] = {
      if(other.size == 0) this
      else if(other.size == 1) this + other.head
      else {
        val copy = js.Dictionary.empty[B1]
        foreach(copy += _)
        other.foreach(copy += _)

        val newStartIdx = other match {
          case x: Leaf[B1] => math.min(x.startIdx, startIdx)
          case x: Branch[B1] => math.min(x.startIdx, startIdx)
          case _ => startIdx
        }

        FastJSMap(copy, copy.keys.toSeq.length, newStartIdx)
      }
    }
  }
}

