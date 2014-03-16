package org.mockito.internal.stubbing.answers

import org.mockito.stubbing.Answer
import org.mockito.invocation.InvocationOnMock
import scala.reflect.runtime
import scala.reflect.runtime.universe.typeOf
import scala.util.{Success, Try}

class Returns (param: Any) extends Answer[Any] with java.io.Serializable {

  private[this] val value: Any = if (isAnyVal(param)) {
    // 'param' is AnyVal, we have to extract primitive value from the instance
    param.getClass.getDeclaredFields match {
      case Array(p) => {
        p.setAccessible(true)
        val v = p.get(param)
        p.setAccessible(false)
        v
      }
      case _ => throw new IllegalStateException("unable to get primitive value from Value Class")
    }
  } else {
    param
  }

  private[this] val serialVersionUID = -6245608253574215396L

  def answer(invocation: InvocationOnMock): Any =
    if (invocation.getMethod.getReturnType.isPrimitive) value else param

  def printReturnType(): String = value.getClass.getSimpleName

  def getReturnType(): Class[_] = value.getClass

  def returnsNull(): Boolean = value == null

  override def toString(): String = "Returns: " + value

  private[this] def isAnyVal(t: Any): Boolean = {
    val mirror = runtime.currentMirror

    Try(mirror.reflect(t).symbol.toType) match {
      case Success(tpe) => tpe <:< typeOf[AnyVal]
      case _ => false
    }
  }
}
