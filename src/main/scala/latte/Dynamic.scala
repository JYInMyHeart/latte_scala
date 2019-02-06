package latte

import java.lang.invoke._
import java.lang.reflect.Method

import scala.util.control.Breaks

object Dynamic {
  @SuppressWarnings(Array("unused")) val GET_FIELD = 1
  @SuppressWarnings(Array("unused")) val GET_STATIC = 2
  @SuppressWarnings(Array("unused")) val PUT_FIELD = 3
  @SuppressWarnings(Array("unused")) val PUT_STATIC = 4
  @SuppressWarnings(Array("unused")) val INVOKE_VIRTUAL = 5
  val INVOKE_STATIC = 6
  val INVOKE_SPECIAL = 7
  @SuppressWarnings(Array("unused")) val NEW_INVOKE_SPECIAL = 8
  @SuppressWarnings(Array("unused")) val INVOKE_INTERFACE = 9

  private val PRIMITIVE_BOX_CAST_BASE = 233

  val methodHandle: MethodHandle =
    MethodHandles
      .lookup()
      .findStatic(
        classOf[Dynamic],
        "invoke",
        MethodType.methodType(
          classOf[Object],
          classOf[Class[_]],
          classOf[String],
          classOf[Array[Boolean]],
          classOf[Array[Object]]))

  def bootstrap(
      lookup: MethodHandles.Lookup,
      methodName: String,
      methodType: MethodType): CallSite = {
    val primitives = for (i <- 0 until methodType.parameterCount() - 1)
      yield methodType.parameterType(i + 1).isPrimitive

    var mh =
      MethodHandles.insertArguments(methodHandle, 1, lookup.lookupClass(), methodName, primitives)
    mh = mh.asCollector(classOf[Array[Object]], methodType.parameterCount() - 1).asType(methodType)
    val mCallSite = new MutableCallSite(mh)
    mCallSite.setTarget(mh)
    mCallSite
  }

  def fillMethodCandidates(c:Class[_],invoker:Class[_],method:String,args:Array[Object],methodList:List[Method]): Unit ={
    for(m <- c.getDeclaredMethods){
      val loop = new Breaks
      loop.breakable{
        if(m.getName != method) loop.break()
        if(m.getParameterCount != args.length) loop.break()
        if(java.lang.reflect.Modifier.isPrivate(m.getModifiers)){
          if(invoker != c) loop.break()
        }else if(java.lang.reflect.Modifier.isProtected(m.getModifiers)){
          if(c.getPackage != invoker.getPackage && !c.isAssignableFrom(invoker)) loop.break()
        }else if(java.lang.reflect.Modifier.isPublic(m.getModifiers)){
          if(c.getPackage != invoker.getPackage) loop.break()
        }
      }
    }
  }
}
