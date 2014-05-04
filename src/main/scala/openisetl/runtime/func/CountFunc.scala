/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime.func

import openisetl.runtime._val._

object CountFunc extends FuncVal {
	override def funcName = "__count"
	override def paramCount = 1
	override def call(v:BaseVal) = v.size()
}