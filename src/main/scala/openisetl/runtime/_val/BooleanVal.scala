/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime._val

object BooleanVal {
	val _true = new BooleanVal(true)
	val _false = new BooleanVal(false)
}
/* must not be case class, comparison is done on instances */
class BooleanVal private (final val value : Boolean)
	extends GenericBaseVal {

	override def typeName = "Boolean"
	override def toString = if (value) "true" else "false" 
}