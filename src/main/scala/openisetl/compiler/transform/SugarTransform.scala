/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler.transform

import openisetl.compiler.parser.TransformVisitorAdapter
import openisetl.compiler.node._

class SugarTransform extends TransformVisitorAdapter {
	
	override def walkStmt(t : Stmt) = t match {
		case PrintStmt(args) => {
			visitNullExprStmt(NullExprStmt(CallExpr(Identifier("print"), args)))
		}
		case _ => super.walkStmt(t)
	}
	
	override def walkExpr(t : Expr) = t match {
		case BinopExpr(l,r,NotinOp()) => {
			visitUnopExpr(UnopExpr(BinopExpr(l,r,InOp()), NotOp()))
		}
		case BinopExpr(l,r,ImplOp()) => {
			visitBinopExpr(BinopExpr(UnopExpr(l,NotOp()), r, OrOp()))
		}
		case BinopExpr(l,r,CoalesceOp()) => {
			visitCallExpr(CallExpr(Identifier("coalesce"), List(l,r)))
		}
		case UnopExpr(e,SizeOp()) => {
			visitCallExpr(CallExpr(Identifier("count"), List(e)))
		}
		case SetExpr(args) => {
			visitCallExpr(CallExpr(Identifier("set_create"), args))
		}
		case TupleExpr(args) => {
			visitCallExpr(CallExpr(Identifier("tuple_create"), args))
		}
		case _ => super.walkExpr(t)
	}
}
