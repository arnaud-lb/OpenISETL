/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime.func

import openisetl.runtime._val.{BaseVal,UndefinedVal,FuncVal}

object PrintFunc extends FuncVal {
	override def funcName = "print"
	override def paramCount = -1
	override def call(a:Array[BaseVal]) = {
		a.foreach { v =>
			println(v.toString + ";")
		}
		UndefinedVal.undefined
	}
}