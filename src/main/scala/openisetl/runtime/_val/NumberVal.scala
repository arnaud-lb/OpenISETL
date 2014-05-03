/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime._val

import java.math.BigInteger

final case class IntegerVal(final val value:Int)
	extends GenericBaseVal {
	
	override def typeName = "Integer"
	override def toString = value.toString

	override def add(_rhs : BaseVal) : BaseVal = _rhs match {
		case IntegerVal(other_value) => {
			val r = value.asInstanceOf[Long] + other_value.asInstanceOf[Long]
			if (r > Integer.MAX_VALUE || r < Integer.MIN_VALUE) {
				new BignumVal(java.math.BigInteger.valueOf(r))
			} else {
				new IntegerVal(r.asInstanceOf[Int])
			}
		}
		case RealVal(other_value) => new RealVal(value + other_value)
		case _ => super.add(_rhs)
	}
	
	override def sub(_rhs : BaseVal) : BaseVal = _rhs match {
		case IntegerVal(other_value) => {
			val r = value.asInstanceOf[Long] - other_value.asInstanceOf[Long]
			if (r > Integer.MAX_VALUE || r < Integer.MIN_VALUE) {
				new BignumVal(java.math.BigInteger.valueOf(r))
			} else {
				new IntegerVal(r.asInstanceOf[Int])
			}
		}
		case RealVal(other_value) => new RealVal(value - other_value)
		case _ => super.sub(_rhs)
	}
	
	override def mul(_rhs : BaseVal) : BaseVal = _rhs match {
		case IntegerVal(other_value) => new RealVal(value * other_value)
		case RealVal(other_value) => new RealVal(value * other_value)
		case _ => super.mul(_rhs)
	}
	
	override def div(_rhs : BaseVal) : BaseVal = _rhs match {
		case IntegerVal(other_value) => new RealVal(value.asInstanceOf[Double] / other_value.asInstanceOf[Double])
		case RealVal(other_value) => new RealVal(value / other_value)
		case _ => super.div(_rhs)
	}
	
	override def mod(_rhs : BaseVal) : BaseVal = _rhs match {
		case IntegerVal(other_value) => new IntegerVal(value % other_value)
		case _ => super.mod(_rhs)
	}
	
	override def rel(_rhs : BaseVal) : Int = _rhs match {
		case IntegerVal(other_value) => value - other_value
		case RealVal(other_value) => {
			if (other_value > value) -1
			else if (other_value < value) 1
			else 0
		}
		case _ => super.rel(_rhs)
	}
	
	override def eq(_rhs : BaseVal) : Boolean = _rhs match {
		case IntegerVal(other_value) => value == other_value
		case RealVal(other_value) => value == other_value
		case _ => false
	}
	
	override def to[A](_to : GenericBaseVal, it: A)(add : (A, GenericBaseVal) => A) : A = {
		val to = _to match {
			case IntegerVal(x) => x
			case _ => return super.to(_to, it)(add)
		}
		var x = value
		var r = it
		while (x < to) {
			r = add(r, new IntegerVal(x))
			x += 1
		}
		r
	}
}

final case class RealVal(final val value:Double)
	extends GenericBaseVal {
	
	override def typeName = "Real"
	override def toString = value.toString
	
	override def add(_rhs : BaseVal) : BaseVal = _rhs match {
		case RealVal(other_value) => new RealVal(value + other_value)
		case IntegerVal(other_value) => new RealVal(value + other_value)
		case _ => super.add(_rhs)
	}
	
	override def sub(_rhs : BaseVal) : BaseVal = _rhs match {
		case RealVal(other_value) => new RealVal(value - other_value)
		case IntegerVal(other_value) => new RealVal(value - other_value)
		case _ => super.sub(_rhs)
	}
	
	override def mul(_rhs : BaseVal) : BaseVal = _rhs match {
		case RealVal(other_value) => new RealVal(value * other_value)
		case IntegerVal(other_value) => new RealVal(value * other_value)
		case _ => super.mul(_rhs)
	}
	
	override def div(_rhs : BaseVal) : BaseVal = _rhs match {
		case RealVal(other_value) => new RealVal(value / other_value)
		case IntegerVal(other_value) => new RealVal(value / other_value)
		case _ => super.div(_rhs)
	}
	
	override def rel(_rhs : BaseVal) : Int = _rhs match {
		case IntegerVal(other_value) => {
			if (other_value > value) -1
			else if (other_value < value) 1
			else 0
		}
		case RealVal(other_value) => {
			if (other_value > value) -1
			else if (other_value < value) 1
			else 0
		}
		case _ => super.rel(_rhs)
	}
	
	override def eq(_rhs : BaseVal) : Boolean = _rhs match {
		case RealVal(other_value) => value == other_value
		case IntegerVal(other_value) => value == other_value
		case _ => false
	}
}

final case class BignumVal(final val value:java.math.BigInteger)
	extends GenericBaseVal {

	override def typeName = "Bignum"
	override def toString = value.toString

	override def add(_rhs : BaseVal) : BaseVal = _rhs match {
		case b : BignumVal => new BignumVal(value.add(b.value))
		case IntegerVal(other_value) => new BignumVal(value.add(BigInteger.valueOf(other_value)))
		case _ => super.add(_rhs)
  }
}
