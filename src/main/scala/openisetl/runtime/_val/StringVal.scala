/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime._val

import scala.collection.immutable.Set
import scala.annotation.tailrec
import openisetl.runtime.ExecuteException

object StringVal {
	val empty = new StringVal("") 
}
case class StringVal(final val value: String) 
	extends GenericBaseVal {
	
	override def typeName = "String"
	override def toString = "\"" + value.replaceAll("[\"\\\\]", "\\\\$1") + "\"";

	override def add(_rhs : BaseVal) : BaseVal = {
		if (_rhs.isInstanceOf[StringVal]) {
			val rhs = _rhs.asInstanceOf[StringVal]
			return new StringVal(value + rhs.value)
		}
		super.add(_rhs)
	}
	
	override def mul(_rhs : BaseVal) : BaseVal = {
		
		@tailrec
		def mul0(s: String, r: String, e: Int): String = e match {
		  case 0 => "" + r
		  case 1 => s + r
		  case 2 => s + s + r
		  case odd if odd % 2 == 1 => mul0(s, r, (odd - 1))
		  case even => mul0(s, "", even / 2)
		}
		
		if (_rhs.isInstanceOf[IntegerVal]) {
			val rhs = _rhs.asInstanceOf[IntegerVal]
			if (rhs.value < 0) {
				throw new ExecuteException("Invalid multiplier");
			}
			new StringVal(mul0(value, "", rhs.value))
		} else {
			super.sub(_rhs)
		}
	}
	
	override def slice(_start:BaseVal, _end:BaseVal) : BaseVal = {
		val start = _start match {
			case IntegerVal(x) => x 
			case x:UndefinedVal => 0
			case _ => return super.slice(_start,_end)
		}
		if (start < 0 || start > value.length)
			return invalidRange(_start,_end, "Slice lower bound not contained in string", this)
			
		val end = _end match {
			case IntegerVal(x) => x
			case x:UndefinedVal => value.length
			case _ => return super.slice(_start,_end)
		}
		if (end < 0 || end > value.length)
			return invalidRange(_start,_end, "Slice higher bound not contained in string", this) 
			
		new StringVal(value.substring(start, end - start))
	}
	
	override def to[A](_to : GenericBaseVal, it: A)(add : (A, GenericBaseVal) => A) : A = {
		val from : Int = value.length match {
			case 1 => value.charAt(0)
			case _ => return invalidRange(this,_to, "String range over single chars only", it) 
		}
		val to : Int = _to match {
			case StringVal(x) => x.length match {
				case 1 => x.charAt(0)
				case _ => return invalidRange(this,_to, "String range over single chars only", it)
			}
			case _ => return super.to(_to, it)(add)
		}
		var x = to
		var r = it
		while (x < to) {
			r = add(r, new StringVal(x.asInstanceOf[Char].toString))
			x += 1
		}
		r
	}

}