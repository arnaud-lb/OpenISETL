/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime._val

import scala.collection.immutable.Set

object SetVal {
	val emptyValue : Set[BaseVal] = Set.empty
	val empty = new SetVal(emptyValue) 
}
case class SetVal(final val value: Set[BaseVal]) 
	extends GenericBaseVal {
	
	override def typeName = "Set"
	override def toString = value.mkString("{", ",", "}")
	
	override def add(_rhs : BaseVal) : BaseVal = {
		if (_rhs.isInstanceOf[SetVal]) {
			val rhs = _rhs.asInstanceOf[SetVal]
			return new SetVal(value ++ rhs.value)
		}
		super.add(_rhs)
	}
	
	override def sub(_rhs : BaseVal) : BaseVal = {
		if (_rhs.isInstanceOf[SetVal]) {
			val rhs = _rhs.asInstanceOf[SetVal]
			return new SetVal(value -- rhs.value)
		}
		super.sub(_rhs)
	}
	
	override def mul(_rhs : BaseVal) : BaseVal = {
		if (_rhs.isInstanceOf[SetVal]) {
			val rhs = _rhs.asInstanceOf[SetVal]
			return new SetVal(value intersect rhs.value)
		}
		super.sub(_rhs)
	}
	
	override def _with(_rhs : BaseVal) : BaseVal = new SetVal(value + _rhs)
	
	override def less(_rhs : BaseVal) : BaseVal = new SetVal(value - _rhs)
	
	override def head() : BaseVal = {
		if (value.isEmpty) UndefinedVal.undefined
		else value.head
	}
	
	override def tail() : BaseVal = {
		if (value.isEmpty) new SetVal(value)
		else new SetVal(value.tail)
	}
	
	override def subset(_lhs:BaseVal) = {
		if (_lhs.isInstanceOf[SetVal]) {
			val lhs = _lhs.asInstanceOf[SetVal].value
			lhs subsetOf value
		} else {
			super.subset(_lhs)
		}
	}
	
	override def in(lhs:BaseVal) = value contains lhs
	
	override def random = head
}