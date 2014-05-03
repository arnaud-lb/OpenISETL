/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */


package openisetl.compiler.node
import openisetl.compiler.analysis.dataholder.AnDataHolder

class NodeExt[-T] { def unapply(o:T) = o != null }
object Node extends NodeExt[Node]
sealed abstract class Node() extends AnDataHolder with Product

// Stmt
object Stmt { def unapply(o:Stmt) = o != null  } 
sealed abstract class Stmt() extends Node

final case class PrintStmt(args: List[Expr]) extends Stmt
final case class ReturnStmt(expr: Option[Expr]) extends Stmt
final case class LocalStmt(locals: List[Identifier]) extends Stmt
final case class AssignStmt(lv: Lvalue, expr: Expr) extends Stmt
final case class ExprStmt(expr: Expr) extends Stmt
final case class NullExprStmt(expr: Expr) extends Stmt
final case class StmtsStmt(stmts: List[Stmt]) extends Stmt

final case class WhileStmt(cond: Expr, stmts : List[Stmt]) extends Stmt
final case class ForStmt(lvs: List[Lvalue], sub: Expr, stmts : List[Stmt]) extends Stmt
final case class IfStmt(cond: Expr, stmts: List[Stmt], fb: Option[Stmt]) extends Stmt

// Expr
object Expr extends NodeExt[Expr]
sealed abstract class Expr() extends Node

final case class UndefinedConst(text: String) extends Expr
final case class BooleanConst(text: String) extends Expr
final case class StringConst(text: String) extends Expr
final case class IntegerConst(text: String) extends Expr
final case class Identifier(id: String) extends Expr

final case class BinopExpr(lhs: Expr, rhs: Expr, op: BinOp) extends Expr
final case class UnopExpr(expr: Expr, op: UnOp) extends Expr
final case class CallExpr(fun: Expr, args: List[Expr]) extends Expr
final case class TupleExpr(args: List[Expr]) extends Expr
final case class SetExpr(args: List[Expr]) extends Expr
final case class FuncDeclExpr(params: List[Identifier], stmts: List[Stmt]) extends Expr
final case class QtfExpr(lvs: List[Lvalue], sub: Expr, cond: Expr, op: QtfOp) extends Expr

// Lvalue

object Lvalue extends NodeExt[Lvalue]
sealed abstract class Lvalue() extends Node

final case class IdentifierLvalue(id: Identifier) extends Lvalue
final case class Rlvalue(id: Identifier) extends Lvalue
final case class DimLvalue(lv: Lvalue, dim: Expr) extends Lvalue
final case class MultipleLvalue(lvs: List[Lvalue]) extends Lvalue

// Op
//                  ___Op____
//                 /         \
//            BinOp          UnOp
//           /    \         /    \
//      RelOp     ArOp   RelUnOp  ArUnOp

object Op extends NodeExt[Op]
sealed abstract class Op() extends Node

object BinOp extends NodeExt[BinOp]
sealed abstract class BinOp() extends Op

object RelOp extends NodeExt[RelOp]
sealed abstract class RelOp() extends BinOp

final case class GtOp() extends RelOp()
final case class LtOp() extends RelOp()
final case class GeOp() extends RelOp()
final case class LeOp() extends RelOp()
final case class EqOp() extends RelOp()
final case class NeOp() extends RelOp()
final case class SubsetOp() extends RelOp()
final case class InOp() extends RelOp()
final case class NotinOp() extends RelOp()
final case class AndOp() extends RelOp()
final case class OrOp() extends RelOp()
final case class ImplOp() extends RelOp()

object ArOp extends NodeExt[ArOp]
sealed abstract class ArOp() extends BinOp

final case class AddOp() extends ArOp()
final case class SubOp() extends ArOp()
final case class MulOp() extends ArOp()
final case class DivOp() extends ArOp()
final case class ModOp() extends ArOp()
final case class ExpOp() extends ArOp()
final case class CoalesceOp() extends ArOp()
final case class WithOp() extends ArOp()
final case class LessOp() extends ArOp()

object UnOp extends NodeExt[UnOp]
sealed abstract class UnOp() extends Op

object ArUnOp extends NodeExt[ArUnOp]
sealed abstract class ArUnOp() extends UnOp

final case class SizeOp() extends ArUnOp()
final case class InvOp() extends ArUnOp()

object RelUnOp extends NodeExt[RelUnOp]
sealed abstract class RelUnOp() extends UnOp

final case class NotOp() extends RelUnOp()

object QtfOp extends NodeExt[QtfOp]
sealed abstract class QtfOp() extends Node

final case class ForallOp() extends QtfOp
final case class ExistsOp() extends QtfOp
final case class ChooseOp() extends QtfOp