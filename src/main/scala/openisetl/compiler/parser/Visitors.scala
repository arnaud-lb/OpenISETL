/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */


package openisetl.compiler.parser

import openisetl.compiler.node._

/**
 * Visitor interfaces and implementations
 *
 * Clients can use both pattern matching
 * and visitor-like methods
 * 
 * Pattern matching is done in walk* methods,
 * starting from super types and dispatching
 * to more specialized types, ending with
 * visit methods.
 *
 * walkAny, walkNode, [walkStmt | walkLvalue | walkExpr]
 * are called for each node, in order. The last one
 * dispatches to a visit method.
 * 
 * In TransformVisitorAdapter only methods with matching
 * return type are called. If a Stmt is allowed, the node
 * is dispatched directly to walkStmt, without calling
 * walkAny and walkNode. If a Idenfitifer is allowed, only
 * visitIdentifier is called.
 */

trait Visitor[T] {
	
	def walkStmt(t : Stmt) : T
	def walkLvalue(t : Lvalue) : T
	def walkExpr(t : Expr) : T
	def walkNode(n: Node) : T
	
	def visitPrintStmt(s:PrintStmt) : T
	def visitReturnStmt(s:ReturnStmt) : T
	def visitLocalStmt(s:LocalStmt) : T
	def visitAssignStmt(s:AssignStmt) : T
	def visitExprStmt(s:ExprStmt) : T
	def visitNullExprStmt(s:NullExprStmt) : T
	def visitWhileStmt(s:WhileStmt) : T
	def visitForStmt(s:ForStmt) : T
	def visitIfStmt(s:IfStmt) : T
	def visitStmtsStmt(s:StmtsStmt) : T
	
	def visitIdentifierLvalue(l:IdentifierLvalue) : T
	def visitRlvalue(l:Rlvalue) : T
	def visitDimLvalue(l:DimLvalue) : T
	def visitMultipleLvalue(l:MultipleLvalue) : T
	
	def visitUndefinedConst(e:UndefinedConst) : T
	def visitBooleanConst(e:BooleanConst) : T
	def visitStringConst(e:StringConst) : T
	def visitIntegerConst(e:IntegerConst) : T
	def visitIdentifier(e:Identifier) : T
	def visitBinopExpr(e:BinopExpr) : T
	def visitUnopExpr(e:UnopExpr) : T
	def visitCallExpr(e:CallExpr) : T
	def visitTupleExpr(e:TupleExpr) : T
	def visitSetExpr(e:SetExpr) : T
	def visitFuncDeclExpr(e:FuncDeclExpr) : T
	def visitQtfExpr(e:QtfExpr) : T
}

trait AbstractVisitorAdapter[T] extends Visitor[T] {
	
	val AbstractVisitorAdapter_default : T

	def visitPrintStmt(s:PrintStmt) : T = defaultWalkChilds(s)
	def visitReturnStmt(s:ReturnStmt) : T = defaultWalkChilds(s)
	def visitLocalStmt(s:LocalStmt) : T = defaultWalkChilds(s)
	def visitAssignStmt(s:AssignStmt) : T = defaultWalkChilds(s)
	def visitExprStmt(s:ExprStmt) : T = defaultWalkChilds(s)
	def visitNullExprStmt(s:NullExprStmt) : T = defaultWalkChilds(s)
	def visitWhileStmt(s:WhileStmt) : T = defaultWalkChilds(s)
	def visitForStmt(s:ForStmt) : T = defaultWalkChilds(s)
	def visitIfStmt(s:IfStmt) : T = defaultWalkChilds(s)
	def visitStmtsStmt(s:StmtsStmt) : T = defaultWalkChilds(s)
	
	def visitIdentifierLvalue(l:IdentifierLvalue) : T = defaultWalkChilds(l)
	def visitRlvalue(l:Rlvalue) : T = defaultWalkChilds(l)
	def visitDimLvalue(l:DimLvalue) : T = defaultWalkChilds(l)
	def visitMultipleLvalue(l:MultipleLvalue) : T = defaultWalkChilds(l)
	
	def visitUndefinedConst(e:UndefinedConst) : T = defaultWalkChilds(e)
	def visitBooleanConst(e:BooleanConst) : T = defaultWalkChilds(e)
	def visitStringConst(e:StringConst) : T = defaultWalkChilds(e)
	def visitIntegerConst(e:IntegerConst) : T = defaultWalkChilds(e)
	def visitIdentifier(e:Identifier) : T = defaultWalkChilds(e)
	def visitBinopExpr(e:BinopExpr) : T = defaultWalkChilds(e)
	def visitUnopExpr(e:UnopExpr) : T = defaultWalkChilds(e)
	def visitCallExpr(e:CallExpr) : T = defaultWalkChilds(e)
	def visitTupleExpr(e:TupleExpr) : T = defaultWalkChilds(e)
	def visitSetExpr(e:SetExpr) : T = defaultWalkChilds(e)
	def visitFuncDeclExpr(e:FuncDeclExpr) : T = defaultWalkChilds(e)
	def visitQtfExpr(e:QtfExpr) : T = defaultWalkChilds(e)
	
	def walkStmt(t : Stmt) : T = t match {
		case s @ PrintStmt(_) => visitPrintStmt(s)
		case s @ ReturnStmt(_) => visitReturnStmt(s)
		case s @ LocalStmt(_) => visitLocalStmt(s)
		case s @ AssignStmt(_,_) => visitAssignStmt(s)
		case s @ ExprStmt(_) => visitExprStmt(s)
		case s @ NullExprStmt(_) => visitNullExprStmt(s)
		case s @ WhileStmt(_,_) => visitWhileStmt(s)
		case s @ ForStmt(_,_,_) => visitForStmt(s)
		case s @ IfStmt(_,_,_) => visitIfStmt(s)
		case s @ StmtsStmt(_) => visitStmtsStmt(s)
	}
	def walkLvalue(t : Lvalue) : T = t match {
		case l @ IdentifierLvalue(_) => visitIdentifierLvalue(l)
		case l @ Rlvalue(_) => visitRlvalue(l)
		case l @ DimLvalue(_,_) => visitDimLvalue(l)
		case l @ MultipleLvalue(_) => visitMultipleLvalue(l) 
	}
	def walkExpr(t : Expr) : T = t match {
		case e @ UndefinedConst(_) => visitUndefinedConst(e)
		case e @ BooleanConst(_) => visitBooleanConst(e)
		case e @ StringConst(_) => visitStringConst(e)
		case e @ IntegerConst(_) => visitIntegerConst(e)
		case e @ Identifier(_) => visitIdentifier(e)
		case e @ BinopExpr(_,_,_) => visitBinopExpr(e)
		case e @ UnopExpr(_,_) => visitUnopExpr(e)
		case e @ CallExpr(_,_) => visitCallExpr(e)
		case e @ TupleExpr(_) => visitTupleExpr(e)
		case e @ SetExpr(_) => visitSetExpr(e)
		case e @ FuncDeclExpr(_,_) => visitFuncDeclExpr(e)
		case e @ QtfExpr(_,_,_,_) => visitQtfExpr(e)
	}
	
	def walkNode(n: Node) : T = n match {
		case s @ Stmt() => walkStmt(s)
		case e @ Expr() => walkExpr(e)
		case l @ Lvalue() => walkLvalue(l)
		case (Op() | QtfOp()) => AbstractVisitorAdapter_default
	}
	
	def walkAny(a: Any) : T = a match {
		case n@Node() => walkNode(n)
		case l:List[_] => walkList(l)
		case Some(x) => walkAny(x)
		case None => AbstractVisitorAdapter_default
		case s:String => AbstractVisitorAdapter_default
		case s:Int => AbstractVisitorAdapter_default
	}
	
	def walkList(l: List[Any]) : T = {
		l foreach walkAny
		AbstractVisitorAdapter_default
	}
	def walkChilds(n: Node) : T = {
		n.productIterator foreach walkAny
		AbstractVisitorAdapter_default
	}
	def defaultWalkChilds(n: Node) : T = AbstractVisitorAdapter_default
	
	def walkStmts(l: List[Stmt]) : T = {
		l foreach walkStmt
		AbstractVisitorAdapter_default
	}
}

trait VisitorAdapter extends AbstractVisitorAdapter[Unit] {
	val AbstractVisitorAdapter_default : Unit = Unit
}

trait DeepVisitorAdapter extends VisitorAdapter {
	
	def inPrintStmt(s:PrintStmt) : Unit = defaultIn(s)
	def inReturnStmt(s:ReturnStmt) : Unit = defaultIn(s)
	def inLocalStmt(s:LocalStmt) : Unit = defaultIn(s)
	def inAssignStmt(s:AssignStmt) : Unit = defaultIn(s)
	def inExprStmt(s:ExprStmt) : Unit = defaultIn(s)
	def inNullExprStmt(s:NullExprStmt) : Unit = defaultIn(s)
	def inWhileStmt(s:WhileStmt) : Unit = defaultIn(s)
	def inForStmt(s:ForStmt) : Unit = defaultIn(s)
	def inIfStmt(s:IfStmt) : Unit = defaultIn(s)
	def inStmtsStmt(s:StmtsStmt) : Unit = defaultIn(s)
	
	def inIdentifierLvalue(l:IdentifierLvalue) : Unit = defaultIn(l)
	def inRlvalue(l:Rlvalue) : Unit = defaultIn(l)
	def inDimLvalue(l:DimLvalue) : Unit = defaultIn(l)
	def inMultipleLvalue(l:MultipleLvalue) : Unit = defaultIn(l)
	
	def inUndefinedConst(e:UndefinedConst) : Unit = defaultIn(e)
	def inBooleanConst(e:BooleanConst) : Unit = defaultIn(e)
	def inStringConst(e:StringConst) : Unit = defaultIn(e)
	def inIntegerConst(e:IntegerConst) : Unit = defaultIn(e)
	def inIdentifier(e:Identifier) : Unit = defaultIn(e)
	def inBinopExpr(e:BinopExpr) : Unit = defaultIn(e)
	def inUnopExpr(e:UnopExpr) : Unit = defaultIn(e)
	def inCallExpr(e:CallExpr) : Unit = defaultIn(e)
	def inTupleExpr(e:TupleExpr) : Unit = defaultIn(e)
	def inSetExpr(e:SetExpr) : Unit = defaultIn(e)
	def inFuncDeclExpr(e:FuncDeclExpr) : Unit = defaultIn(e)
	def inQtfExpr(e:QtfExpr) : Unit = defaultIn(e)
	
	def outPrintStmt(s:PrintStmt) : Unit = defaultOut(s)
	def outReturnStmt(s:ReturnStmt) : Unit = defaultOut(s)
	def outLocalStmt(s:LocalStmt) : Unit = defaultOut(s)
	def outAssignStmt(s:AssignStmt) : Unit = defaultOut(s)
	def outExprStmt(s:ExprStmt) : Unit = defaultOut(s)
	def outNullExprStmt(s:NullExprStmt) : Unit = defaultOut(s)
	def outWhileStmt(s:WhileStmt) : Unit = defaultOut(s)
	def outForStmt(s:ForStmt) : Unit = defaultOut(s)
	def outIfStmt(s:IfStmt) : Unit = defaultOut(s)
	def outStmtsStmt(s:StmtsStmt) : Unit = defaultOut(s)
	
	def outIdentifierLvalue(l:IdentifierLvalue) : Unit = defaultOut(l)
	def outRlvalue(l:Rlvalue) : Unit = defaultOut(l)
	def outDimLvalue(l:DimLvalue) : Unit = defaultOut(l)
	def outMultipleLvalue(l:MultipleLvalue) : Unit = defaultOut(l)
	
	def outUndefinedConst(e:UndefinedConst) : Unit = defaultOut(e)
	def outBooleanConst(e:BooleanConst) : Unit = defaultOut(e)
	def outStringConst(e:StringConst) : Unit = defaultOut(e)
	def outIntegerConst(e:IntegerConst) : Unit = defaultOut(e)
	def outIdentifier(e:Identifier) : Unit = defaultOut(e)
	def outBinopExpr(e:BinopExpr) : Unit = defaultOut(e)
	def outUnopExpr(e:UnopExpr) : Unit = defaultOut(e)
	def outCallExpr(e:CallExpr) : Unit = defaultOut(e)
	def outTupleExpr(e:TupleExpr) : Unit = defaultOut(e)
	def outSetExpr(e:SetExpr) : Unit = defaultOut(e)
	def outFuncDeclExpr(e:FuncDeclExpr) : Unit = defaultOut(e)
	def outQtfExpr(e:QtfExpr) : Unit = defaultOut(e)
		
	def defaultIn(n:Node) : Unit = Unit
	def defaultOut(n:Node) : Unit = Unit
	
	override def visitPrintStmt(s:PrintStmt) : Unit = {inPrintStmt(s);walkChilds(s);outPrintStmt(s)}
	override def visitReturnStmt(s:ReturnStmt) : Unit = {inReturnStmt(s);walkChilds(s);outReturnStmt(s)}
	override def visitLocalStmt(s:LocalStmt) : Unit = {inLocalStmt(s);walkChilds(s);outLocalStmt(s)}
	override def visitAssignStmt(s:AssignStmt) : Unit = {inAssignStmt(s);walkChilds(s);outAssignStmt(s)}
	override def visitExprStmt(s:ExprStmt) : Unit = {inExprStmt(s);walkChilds(s);outExprStmt(s)}
	override def visitNullExprStmt(s:NullExprStmt) : Unit = {inNullExprStmt(s);walkChilds(s);outNullExprStmt(s)}
	override def visitWhileStmt(s:WhileStmt) : Unit = {inWhileStmt(s);walkChilds(s);outWhileStmt(s)}
	override def visitForStmt(s:ForStmt) : Unit = {inForStmt(s);walkChilds(s);outForStmt(s)}
	override def visitIfStmt(s:IfStmt) : Unit = {inIfStmt(s);walkChilds(s);outIfStmt(s)}
	override def visitStmtsStmt(s:StmtsStmt) : Unit = {inStmtsStmt(s);walkChilds(s);outStmtsStmt(s)}
	
	override def visitIdentifierLvalue(l:IdentifierLvalue) : Unit = {inIdentifierLvalue(l);walkChilds(l);outIdentifierLvalue(l)}
	override def visitRlvalue(l:Rlvalue) : Unit = {inRlvalue(l);walkChilds(l);outRlvalue(l)}
	override def visitDimLvalue(l:DimLvalue) : Unit = {inDimLvalue(l);walkChilds(l);outDimLvalue(l)}
	override def visitMultipleLvalue(l:MultipleLvalue) : Unit = {inMultipleLvalue(l);walkChilds(l);outMultipleLvalue(l)}
	
	override def visitUndefinedConst(e:UndefinedConst) : Unit = {inUndefinedConst(e);walkChilds(e);outUndefinedConst(e)}
	override def visitBooleanConst(e:BooleanConst) : Unit = {inBooleanConst(e);walkChilds(e);outBooleanConst(e)}
	override def visitStringConst(e:StringConst) : Unit = {inStringConst(e);walkChilds(e);outStringConst(e)}
	override def visitIntegerConst(e:IntegerConst) : Unit = {inIntegerConst(e);walkChilds(e);outIntegerConst(e)}
	override def visitIdentifier(e:Identifier) : Unit = {inIdentifier(e);walkChilds(e);outIdentifier(e)}
	override def visitBinopExpr(e:BinopExpr) : Unit = {inBinopExpr(e);walkChilds(e);outBinopExpr(e)}
	override def visitUnopExpr(e:UnopExpr) : Unit = {inUnopExpr(e);walkChilds(e);outUnopExpr(e)}
	override def visitCallExpr(e:CallExpr) : Unit = {inCallExpr(e);walkChilds(e);outCallExpr(e)}
	override def visitTupleExpr(e:TupleExpr) : Unit = {inTupleExpr(e);walkChilds(e);outTupleExpr(e)}
	override def visitSetExpr(e:SetExpr) : Unit = {inSetExpr(e);walkChilds(e);outSetExpr(e)}
	override def visitFuncDeclExpr(e:FuncDeclExpr) : Unit = {inFuncDeclExpr(e);walkChilds(e);outFuncDeclExpr(e)}
	override def visitQtfExpr(e:QtfExpr) : Unit = {inQtfExpr(e);walkChilds(e);outQtfExpr(e)}

}

trait ReduceVisitorAdapter[T] extends AbstractVisitorAdapter[Option[T]] {
	
	val AbstractVisitorAdapter_default : Option[T] = None
	
	override def walkList(l: List[Any]) : Option[T] = {
		walkIterator(l.iterator);
	}
	override def walkChilds(n: Node) : Option[T] = {
		walkIterator(n.productIterator)
	}
	
	def walkIterator(i:Iterator[Any]) : Option[T] = {
		for (e <- i) {
			walkAny(e) match {
				case r@Some(_) => return r
				case _ => None
			}
		}
		None
	}
	
	override def defaultWalkChilds(n: Node) : Option[T] = walkChilds(n)
}

trait TransformVisitorAdapter extends Visitor[Node] {

	override def visitPrintStmt(e:PrintStmt) : PrintStmt = PrintStmt(e.args map walkExpr)
	override def visitReturnStmt(e:ReturnStmt) : ReturnStmt = ReturnStmt(e.expr map walkExpr)
	override def visitLocalStmt(e:LocalStmt) : LocalStmt = LocalStmt(e.locals map visitIdentifier)
	override def visitAssignStmt(e:AssignStmt) : AssignStmt = AssignStmt(walkLvalue(e.lv), walkExpr(e.expr))
	override def visitExprStmt(e:ExprStmt) : ExprStmt = ExprStmt(walkExpr(e.expr))
	override def visitNullExprStmt(e:NullExprStmt) : NullExprStmt = NullExprStmt(walkExpr(e.expr))
	override def visitWhileStmt(e:WhileStmt) : WhileStmt = WhileStmt(walkExpr(e.cond), e.stmts map walkStmt)
	override def visitForStmt(e:ForStmt) : ForStmt = ForStmt(e.lvs map walkLvalue, walkExpr(e.sub), e.stmts map walkStmt)
	override def visitIfStmt(e:IfStmt) : IfStmt = IfStmt(walkExpr(e.cond), e.stmts map walkStmt, e.fb map walkStmt)
	override def visitStmtsStmt(e:StmtsStmt) : StmtsStmt = StmtsStmt(e.stmts map walkStmt)
	
	override def visitIdentifierLvalue(e:IdentifierLvalue) : IdentifierLvalue = IdentifierLvalue(visitIdentifier(e.id))
	override def visitRlvalue(e:Rlvalue) : Rlvalue = Rlvalue(visitIdentifier(e.id))
	override def visitDimLvalue(e:DimLvalue) : DimLvalue = DimLvalue(walkLvalue(e.lv),walkExpr(e.dim))
	override def visitMultipleLvalue(e:MultipleLvalue) : MultipleLvalue = MultipleLvalue(e.lvs map walkLvalue)
	
	override def visitUndefinedConst(e:UndefinedConst) : UndefinedConst = e
	override def visitBooleanConst(e:BooleanConst) : BooleanConst = e
	override def visitStringConst(e:StringConst) : StringConst = e
	override def visitIntegerConst(e:IntegerConst) : IntegerConst = e
	override def visitIdentifier(e:Identifier) : Identifier = e
	override def visitBinopExpr(e:BinopExpr) : BinopExpr = BinopExpr(walkExpr(e.lhs),walkExpr(e.rhs),e.op)
	override def visitUnopExpr(e:UnopExpr) : UnopExpr = UnopExpr(walkExpr(e.expr),e.op)
	override def visitCallExpr(e:CallExpr) : CallExpr = CallExpr(walkExpr(e.fun),e.args map walkExpr)
	override def visitTupleExpr(e:TupleExpr) : TupleExpr = TupleExpr(e.args map walkExpr)
	override def visitSetExpr(e:SetExpr) : SetExpr = SetExpr(e.args map walkExpr)
	override def visitFuncDeclExpr(e:FuncDeclExpr) : FuncDeclExpr = FuncDeclExpr(e.params map visitIdentifier, e.stmts map walkStmt)
	override def visitQtfExpr(e:QtfExpr) : QtfExpr = QtfExpr(e.lvs map walkLvalue, walkExpr(e.sub), walkExpr(e.cond), e.op)
		
	override def walkStmt(t : Stmt) : Stmt = t match {
		case e @ PrintStmt(_) => visitPrintStmt(e)
		case e @ ReturnStmt(_) => visitReturnStmt(e)
		case e @ LocalStmt(_)  => visitLocalStmt(e)
		case e @ AssignStmt(_, _) => visitAssignStmt(e)
		case e @ ExprStmt(_) => visitExprStmt(e)
		case e @ NullExprStmt(_) => visitNullExprStmt(e)
		case e @ WhileStmt(_, _) => visitWhileStmt(e)
		case e @ ForStmt(_, _, _) => visitForStmt(e)
		case e @ IfStmt(_, _, _) => visitIfStmt(e)
		case e @ StmtsStmt(_) => visitStmtsStmt(e)
	}
	
	override def walkLvalue(t : Lvalue) : Lvalue = t match {
		case e @ IdentifierLvalue(_) => visitIdentifierLvalue(e)
		case e @ Rlvalue(_) => visitRlvalue(e)
		case e @ DimLvalue(_,_) => visitDimLvalue(e)
		case e @ MultipleLvalue(_) => visitMultipleLvalue(e)
	}
	
	override def walkExpr(t : Expr) : Expr = t match {
		case e @ UndefinedConst(_) => visitUndefinedConst(e)
		case e @ BooleanConst(_) => visitBooleanConst(e)
		case e @ StringConst(_) => visitStringConst(e)
		case e @ IntegerConst(_) => visitIntegerConst(e)
		case e @ Identifier(_) => visitIdentifier(e)
		case e @ BinopExpr(_,_,_) => visitBinopExpr(e)
		case e @ UnopExpr(_,_) => visitUnopExpr(e)
		case e @ CallExpr(_,_) => visitCallExpr(e)
		case e @ TupleExpr(_) => visitTupleExpr(e)
		case e @ SetExpr(_) => visitSetExpr(e)
		case e @ FuncDeclExpr(_,_) => visitFuncDeclExpr(e)
		case e @ QtfExpr(_,_,_,_) => visitQtfExpr(e)
	}
	
	override def walkNode(n: Node) : Node = n match {
		case s @ Stmt() => walkStmt(s)
		case e @ Expr() => walkExpr(e)
		case l @ Lvalue() => walkLvalue(l)
		case (Op() | QtfOp()) => n
	}
	
	def walkAny(a: Any) : Any = a match {
		case n@Node() => walkNode(n)
		case l:List[_] => walkList(l)
		case Some(x) => walkAny(x)
		case s:String =>
		case i:Int =>
	}
	
	def walkList(l: List[Any]) : List[Any] = l map walkAny
	
	def walkStmts(l: List[Stmt]) : List[Stmt] = l map walkStmt
}
