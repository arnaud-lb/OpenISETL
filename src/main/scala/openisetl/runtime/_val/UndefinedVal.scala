/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime._val

object UndefinedVal {
	val undefined = new UndefinedVal
}
class UndefinedVal private extends BaseVal {
	override def typeName = "Undefined"
	override def toString = typeName
}
