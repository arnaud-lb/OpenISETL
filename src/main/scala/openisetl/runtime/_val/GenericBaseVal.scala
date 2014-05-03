/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime._val

import collection.mutable.ArrayBuffer
import openisetl.runtime.ExecuteException

object BaseVal {
	
	def maxParams = 10
	
	def callMethod(n:Int) = {
		val c = classOf[BaseVal]
		val getMethod = classOf[Class[BaseVal]]
		                        .getMethod("getMethod", 
		                        classOf[String], classOf[Array[Class[_]]])
		val params = if (n < maxParams) {
			ArrayBuffer.empty[Class[_]].padTo(n, c).toArray
		} else {
			Array(classOf[Array[BaseVal]])
		}
		getMethod.invoke(c, "call", params)
			.asInstanceOf[java.lang.reflect.Method]
	}
	
	def meth(name:String) = classOf[BaseVal].getMethod(name, classOf[BaseVal])
	def meth0(name:String) = classOf[BaseVal].getMethod(name)
	def meth2(name:String) = classOf[BaseVal].getMethod(name, classOf[BaseVal], classOf[BaseVal])
	
	val add = meth("add")
	val sub = meth("sub")
	val mul = meth("mul")
	val div = meth("div")
	val mod = meth("mod")
	val exp = meth("exp")
	val _with = meth("_with")
	val less = meth("less")
	val size = meth0("size")
	val inv = meth0("inv")
	val not = meth0("not")
	val rel = meth("rel")
	val eq = meth("eq")
	val head = meth0("head")
	val tail = meth0("tail")
	val last = meth0("last")
	val init = meth0("init")
	val index = meth("index")
	val updated = meth2("updated")
	val slice = meth2("slice")
	val subset = meth("subset")
	val in = meth("in")
	val iter = meth0("iter")
}

abstract class GenericBaseVal {
	
	def typeName : String
	override def toString = typeName + "()"

	def invalid1[T](msg:String, default:T = BooleanVal._true) : T = {
		if(true) throw new ExecuteException("Invalid operation " + msg)
		default
	}

	private def invalid2[T](op:String, rhs:BaseVal, default:T = BooleanVal._true) : T = {
		val msg = typeName + " " + op + " " + rhs.typeName
		invalid1(msg, default);
	}
	
	def add(_rhs:BaseVal) : BaseVal = invalid2("+", _rhs)
	def sub(_rhs:BaseVal) : BaseVal = invalid2("-", _rhs)
	def mul(_rhs:BaseVal) : BaseVal = invalid2("*", _rhs)
	def div(_rhs:BaseVal) : BaseVal = invalid2("/", _rhs)
	def mod(_rhs:BaseVal) : BaseVal = invalid2("mod", _rhs)
	def exp(_rhs:BaseVal) : BaseVal = invalid2("**", _rhs)
	def _with(_rhs:BaseVal) : BaseVal = invalid2("with", _rhs)
	def less(_rhs:BaseVal) : BaseVal = invalid2("less", _rhs)
	
	def size() : BaseVal = invalid1("# on a " + typeName)
	def inv() : BaseVal = invalid1("inv on a " + typeName)
	def not() : BaseVal = invalid1("not on a " + typeName)
	def rel(_rhs:BaseVal) : Int = invalid2("rel", _rhs, 0)
	def eq(_rhs:BaseVal) : Boolean = invalid2("=", _rhs, false)
	
	def head() : BaseVal = invalid1("take on a " + typeName)
	def tail() : BaseVal = invalid1("take on a " + typeName)
	def last() : BaseVal = invalid1("take on a " + typeName)
	def init() : BaseVal = invalid1("take on a " + typeName)
	
	def index(_index:BaseVal) : BaseVal = invalid1(typeName + "(" + _index.typeName + ")")
	def updated(_index:BaseVal, value:BaseVal) : BaseVal = {
		invalid1(typeName + "(" + _index.typeName + ") := ")
	}
	def slice(_start:BaseVal, _end:BaseVal) : BaseVal = {
		invalid1(typeName + "(" + _start.typeName + ".." + _end.typeName + ")")
	}
	def invalidRange[A](_start:BaseVal, _end:BaseVal, msg:String, dummy : A) : A = {
		invalid1(msg + ": " + _start + ".." + _end);
		dummy
	}
	def to[A](_to : GenericBaseVal, it: A)(add : (A, GenericBaseVal) => A) : A = {
		invalid2("..", _to, it)
	}
	
	def subset(_lhs:BaseVal) : Boolean = {
		invalid1(_lhs.typeName + " subset " + typeName, false)
	}
	def in(_lhs:BaseVal) : Boolean = {
		invalid1(_lhs.typeName + " in " + typeName, false)
	}
	
	def random() : BaseVal = {
		invalid1("random(" + typeName + ")")
	}

	def iter() : Iterator[BaseVal] = {
		invalid1("iterate over a " + typeName, null)
	}

	def paramCount = -1
	
	def call() : BaseVal = call(Array.empty[BaseVal])
	
	def call(a1 : BaseVal) : BaseVal = call(Array(a1))
	
	def call(a1 : BaseVal, a2 : BaseVal) : BaseVal = call(Array(a1, a2))
	
	def call(a1 : BaseVal, a2 : BaseVal, a3 : BaseVal) : BaseVal = {
		call(Array(a1, a2, a3))
	}
	
	def call(a1 : BaseVal, a2 : BaseVal, a3 : BaseVal, 
			a4 : BaseVal) : BaseVal = {
		call(Array(a1, a2, a3, a4))
	}
			
	def call(a1 : BaseVal, a2 : BaseVal, a3 : BaseVal, 
			a4 : BaseVal, a5 : BaseVal) : BaseVal = {
		call(Array(a1, a2, a3, a4, a5))
	}
			
	def call(a1 : BaseVal, a2 : BaseVal, a3 : BaseVal, 
			a4 : BaseVal, a5 : BaseVal, a6 : BaseVal) : BaseVal = {
		call(Array(a1, a2, a3, a4, a5, a6))
	}
			
	def call(a1 : BaseVal, a2 : BaseVal, a3 : BaseVal, 
			a4 : BaseVal, a5 : BaseVal, a6 : BaseVal, 
			a7 : BaseVal) : BaseVal = {
		call(Array(a1, a2, a3, a4, a5, a6, a6, a7))
	}
			
	def call(a1 : BaseVal, a2 : BaseVal, a3 : BaseVal, 
			a4 : BaseVal, a5 : BaseVal, a6 : BaseVal, 
			a7 : BaseVal, a8 : BaseVal) : BaseVal = {
		call(Array(a1, a2, a3, a4, a5, a6, a7, a8))
	}
			
	def call(a1 : BaseVal, a2 : BaseVal, a3 : BaseVal, 
			a4 : BaseVal, a5 : BaseVal, a6 : BaseVal, 
			a7 : BaseVal, a8 : BaseVal, a9 : BaseVal) : BaseVal = {
		call(Array(a1, a2, a3, a4, a5, a6, a7, a8, a9))
	}
			
	def call(a1 : BaseVal, a2 : BaseVal, a3 : BaseVal, 
			a4 : BaseVal, a5 : BaseVal, a6 : BaseVal, 
			a7 : BaseVal, a8 : BaseVal, a9 : BaseVal, 
			a10 : BaseVal) : BaseVal = {
		call(Array(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10))
	}
	
	def call(a:Array[BaseVal]) : BaseVal = {
		
		if (paramCount != -1) {
			
			// Callable objects override paramCount and one of the
			// call() methods. Callables with variadic parameters
			// override this method only.
			
			throw new ExecuteException("Invalid number of arguments"
					+ ", found " + a.length + ", expected " + paramCount);
		} else {
			invalid1("call " + typeName)
		}
	}
}

