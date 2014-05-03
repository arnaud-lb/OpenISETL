/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler.analysis

import openisetl.compiler.parser.DeepVisitorAdapter
import openisetl.compiler.node._

object AccessModeAn {
	sealed abstract class Mode()
	case class Load private[AccessModeAn]() extends Mode
	case class Store private[AccessModeAn]() extends Mode
	case class LoadStore private[AccessModeAn]() extends Mode
	
	val load = Load()
	val store = Store()
	val loadStore = LoadStore()
	
	val accessMode = 0
}

class AccessModeAn extends DeepVisitorAdapter with AnDataHolderClient {

	def setAccessMode(n:Identifier, mode:AccessModeAn.Mode) : Unit = {
		setAnData(n, AccessModeAn.accessMode -> mode)
	}
	
	def getAccessMode(n:Identifier) = {
		getAnData[AccessModeAn.Mode](n, AccessModeAn.accessMode)
	}
	
	override def walkLvalue(l:Lvalue) : Unit = l match {
		case IdentifierLvalue(i) =>
      setAccessMode(i, AccessModeAn.store)
    case DimLvalue(IdentifierLvalue(i),dim) =>
			setAccessMode(i, AccessModeAn.loadStore)
			super.walkExpr(dim)
		case Rlvalue(i) =>
			setAccessMode(i, AccessModeAn.loadStore)
		case _ => super.walkLvalue(l)
	}
	
	override def visitIdentifier(i:Identifier) : Unit = {
		setAccessMode(i, AccessModeAn.load)
	}
}
