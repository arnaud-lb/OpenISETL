/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime.func

import openisetl.runtime.annotations.Function
import openisetl.runtime._val._

@Function(parameters=Array[String]("element", "..."))
object SetCreateFunc extends FuncVal {
	override def funcName = "set_create"
	override def paramCount = -1
	override def call(a:Array[BaseVal]) = {
		new SetVal(SetVal.emptyValue ++ a)
	}
}