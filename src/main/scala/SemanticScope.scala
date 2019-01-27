package latte

import latte.Ins.This

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class SemanticScope(parent: SemanticScope,
                    sTypeDef: STypeDef) {

  case class MethodRecorder(method: SMethodDef,
                            paramCount: Int)

  val leftValueMap: mutable.LinkedHashMap[String, LeftValue] = mutable.LinkedHashMap[String, LeftValue]()
  val innerMethodMap: mutable.HashMap[String, MethodRecorder] = mutable.HashMap[String, MethodRecorder]()
  var aThis: This = _

  def this(parent: SemanticScope) = {
    this(parent, null)
  }

  def this(sTypeDef: STypeDef) = {
    this(null, sTypeDef)
  }

  def getLeftValue(name: String): Option[LeftValue] = {
    if (leftValueMap.contains(name))
      Some(leftValueMap(name))
    else if (parent != null)
      parent.getLeftValue(name)
    else
      None
  }

  def getLocalVariables: mutable.LinkedHashMap[String, STypeDef] = {
    val map: mutable.LinkedHashMap[String, STypeDef] = mutable.LinkedHashMap()
    if (parent != null)
      map ++= parent.getLocalVariables
    leftValueMap.foreach(x => map += (x._1 -> x._2.typeOf()))
    map
  }

  def putLeftValue(name: String,
                   v: LeftValue): Unit = {
    leftValueMap += name -> v
  }

  def addMethodRef(name: String,
                   innerMethod: MethodRecorder): Unit = {
    innerMethodMap += name -> innerMethod
  }

  def getLeftValues(count: Int): ListBuffer[LeftValue] = {
    val list = ListBuffer[LeftValue]()
    if (parent != null)
      list ++= parent.getLeftValues(count)
    val it = leftValueMap.values.toIterator
    while (list.length != count && it.hasNext) {
      list += it.next()
    }
    list
  }

  def typeOf: Option[STypeDef] = {
    if (sTypeDef != null)
      Some(sTypeDef)
    else if (parent != null)
      parent.typeOf
    else
      None
  }

  def getThis: Option[This] = {
    if (aThis != null)
      Some(aThis)
    else if (parent != null)
      parent.getThis
    else
      None
  }

  def getIndex(leftValue: LeftValue): Option[Int] = {
    if (parent != null) {
      return parent.getIndex(leftValue)
    }
    var i = if (getThis.isEmpty) 0 else 1
    if (parent != null) i += parent.leftValueMap.size
    if (leftValueMap.values.exists(_ == leftValue)) {
      i += leftValueMap.values.takeWhile(_ == leftValue).size
      Some(i)
    } else
      None
  }

  def generateTempName: String = {
    var i = 0
    while (getLeftValue(s"*$i").nonEmpty)
      i += 1
    s"*$i"
  }


}
