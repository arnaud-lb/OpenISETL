/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime._val

import openisetl.runtime.ExecuteException

object TupleVal {
	type valueType = Vector[BaseVal]
	val valueCompanion = Vector
	val emptyValue : Vector[BaseVal] = Vector.empty
	val empty = new TupleVal(emptyValue)
	
	/*
	import collection.generic.CanBuildFrom
	val filterBF = new CanBuildFrom[valueType, BaseVal, valueType] {
		override def apply(from : valueType) = apply
		override def apply() = new collection.mutable.Builder[BaseVal,valueType] {
			private val builder = valueCompanion.newBuilder[BaseVal]
			override def +=(elem: BaseVal) = {
				elem match {
					case u:UndefinedVal =>
					case _ => builder += elem 
				}
				this
			}
			override def clear = {
				builder.clear
				this
			}
			override def result = builder.result 
		}
	} 
	*/
}
case class TupleVal(final val value: TupleVal.valueType) 
		extends GenericBaseVal {
	
	override def typeName = "Tuple"
	override def toString = value.mkString("[", ",", "]")

	override def add(_rhs : BaseVal) : BaseVal = _rhs match {
		case TupleVal(other_value) => new TupleVal(value ++ other_value)
		case _ => super.add(_rhs)
	}
	
	override def _with(_rhs : BaseVal) : TupleVal = new TupleVal(value :+ _rhs)
	
	override def head : BaseVal = {
		if (value.isEmpty) UndefinedVal.undefined
		else value.head
	}
	
	override def tail : BaseVal = {
		if (value.isEmpty) new TupleVal(value)
		else new TupleVal(value.tail)
	}
	
	override def last : BaseVal = {
		if (value.isEmpty) UndefinedVal.undefined
		else value.last
	}
	
	override def init : BaseVal = {
		if (value.isEmpty) new TupleVal(value)
		else new TupleVal(value.init)
	}

	override def index(_index:BaseVal) : BaseVal = _index match {
		case IntegerVal(i) => {
			if (i < 1) throw new ExecuteException("Invalid index");
			if (i > value.size) return UndefinedVal.undefined
			else value(i-1)
		}
		case _ => super.index(_index)
	}


	override def updated(_index:BaseVal, _val:BaseVal) : BaseVal = _index match {
		case IntegerVal(i) => {
			if (i < 1) throw new ExecuteException("Invalid index");
			if (i > value.size) new TupleVal(value.padTo(i-1, UndefinedVal.undefined) :+ _val)
			else new TupleVal(value.updated(i-1, _val))
		}
		case _ =>	super.updated(_index, _val)
	}
	
	override def paramCount = 1
	override def call(_index:BaseVal) : BaseVal = {
		index(_index)
	}
	
	override def slice(_start:BaseVal, _end:BaseVal) : BaseVal = {
		val start = _start match {
			case IntegerVal(x) => x >= 1 match {
				case true => x - 1
				case false => return invalidRange(_start,_end, "Slice lower bound too low", this)
			}
			case x:UndefinedVal => 0
			case _ => return super.slice(_start,_end)
		}
		val end = _end match {
			case IntegerVal(x) => x <= value.size match {
				case true => x
				case false => value.size
			}
			case x:UndefinedVal => value.size
			case _ => return super.slice(_start,_end)
		}
		new TupleVal(value.slice(start, end))
	}
	
	override def in(lhs:BaseVal) = value contains lhs

	override def iter() = value.iterator
}