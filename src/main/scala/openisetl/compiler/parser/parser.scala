/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler.parser

import scala.io.Source
import java.io.File
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scala.util.parsing.combinator.PackratParsers
import scala.collection.immutable.List

import openisetl.compiler.node._

class OpenisetlParser extends StandardTokenParsers with PackratParsers with Application {
	
  lexical.reserved += (
		  "func","proc","local","return",
		  "end","if","then","else","elseif","while","for","print",
		  "in","notin","not","do","subset","to",
		  "take","from","fromb","frome",
		  "union","inter","div","mod","with","less",
		  "choose","exists","forall","impl","iff",
		  "and","or","true","false","OM","om")
		  
  lexical.delimiters += ("+","-","**","*","/",
		  "/=",":=","=",
		  ">","<","<=",">=",
		  "(",")","{","}","[","]",
		  ",","|","..",".",":",";","#","?")
		  
  def parse(file:File) : ParseResult[List[Stmt]] = {
	  parse(Source.fromFile(file))
  }
  
  def parse(source:Source) : ParseResult[List[Stmt]] = {
	  parse(source.foldLeft(new StringBuffer)(_ append _).toString)
  }
  
  def parse(input:String) : ParseResult[List[Stmt]] = {
	  phrase(program)(new PackratReader(new lexical.Scanner(input)))
  }
  
  implicit def string2binop(s: String) : BinOp = s match {
	  case "+" => AddOp()
	  case "union" => AddOp()
	  case "-" => SubOp()
	  case "/" => DivOp()
	  case "div" => DivOp()
	  case "*" => MulOp()
	  case "inter" => MulOp()
	  case "**" => ExpOp()
	  case "mod" => ModOp()
	  case "?" => CoalesceOp()
	  case "with" => WithOp()
	  case "less" => LessOp()
	  
	  case ">" => GtOp()
	  case "<" => LtOp()
	  case ">=" => GeOp()
	  case "<=" => LeOp()
	  case "=" => EqOp()
	  case "/=" => NeOp()
	  case "iff" => EqOp()
	  
	  case "and" => AndOp()
	  case "or" => OrOp()
	  case "impl" => ImplOp()
	  
	  case "subset" => SubsetOp()
	  case "in" => InOp()
	  case "notin" => NotinOp()
  }
  
  implicit def string2unop(s: String) : UnOp = s match {
  	  case "#" => SizeOp()
  	  case "-" => InvOp()
  	  case "not" => NotOp()
  }
  
  implicit def string2qtfop(s:String) : QtfOp = s match {
	  case "forall" => ForallOp()
	  case "exists" => ExistsOp()
	  case "choose" => ChooseOp()
  }
  
  def asBinop(p:this.~[~[Expr,String],Expr]) = p match {
	  case lhs ~ op ~ rhs => BinopExpr(lhs,rhs,op)
  }
  
  def asUnop(p:this.~[String,Expr]) = p match {
	  case op ~ e => UnopExpr(e,op)
  }
  
  lazy val program = stmts ^^ { case s => s }
  
  lazy val stmts = ((stmt <~ ";")*) ^^ { case s => s }
  lazy val stmts_opt_semi = stmts ~ ((stmt <~ (";"?))?) ^^ { case ss ~ s => ss ++ s.toList }
  lazy val fun_stmts = (localstmt*) ~ stmts ^^ { case l ~ s => l ++ s }
    
  lazy val stmt: PackratParser[Stmt] = (
		  "print" ~> rep1sep(expr, ",") ^^ { case e => PrintStmt(e) }
		  | "return" ~> (expr?) ^^ { case e => ReturnStmt(e) }
		  | lvalue ~ ":=" ~ expr ^^ { case lv ~ _ ~ e => AssignStmt(lv, e) }
		  | expr ^^ { case e => NullExprStmt(e) }
 		  | whileStmt
		  | ifStmt
		  | forStmt
  )
  
  lazy val localstmt = (
		  "local" ~ rep1sep(ident, ",") <~ ";" 
		  ^^ { case _ ~  l => LocalStmt(l.map(Identifier(_))) }
  )
  
  lazy val ifStmt = (
		  "if" ~> expr ~ "then"
		  	~ stmts ~
		  elseStmt
		  ^^ { case e ~ _ ~ st ~ fb => IfStmt(e, st, fb) }
  )
  
  lazy val elseStmt : PackratParser[Option[Stmt]] = (
		  "elseif" ~> expr ~ "then"
		  	~ stmts ~
		  elseStmt
		  ^^ { case e ~ _ ~ st ~ fb => Some(IfStmt(e, st, fb)) }
		  
		  | "else"
		  	~> stmts <~
		  "end" ~ "if"
		  ^^ { case st => Some(StmtsStmt(st)) }
		  
		  | "end" ~ "if"
		  ^^ { case _ => None } 
  )
  
  lazy val whileStmt = (
		  "while" ~> expr ~ "do" ~ stmts <~ "end" <~ "while" 
		  ^^ { case e ~ _ ~ p => WhileStmt(e, p) }
  )
  
  lazy val forStmt = (
		  "for" ~> repsep(lvalue, ",") ~ "in" ~ expr ~ "do"
		  	~ stmts <~
		  "end" ~ "for"
		  ^^ { case lvs ~ _ ~ e ~ _ ~ st => ForStmt(lvs, e, st) }
  ) 
  
  lazy val func0 = (
		  "(" ~> repsep(ident, ",") ~ ")" ~ ";"
		  	~ fun_stmts <~
		  "end"
  )
  
  lazy val funcDecl = (
		  (("func" ~> func0 <~ "func") | ("proc" ~> func0 <~ "proc"))
		  ^^ {
		 	  case params ~ _ ~ _ ~ prog => FuncDeclExpr(params.map(Identifier(_)), prog)
		  }
  )
  
  lazy val expr : PackratParser[Expr] = (
		  iffOp
		  | qtfExpr
  )
	
  lazy val qtfExpr = (
		  ("forall"|"exists"|"choose") ~ rep1sep(lvalue, ",") ~ "in" ~ expr ~ (":"|"|") ~ expr ^^ {
		 	  case op ~ lvs ~ _ ~ sub ~ _ ~ cond => QtfExpr(lvs, sub, cond, op) 
		  }
  ) 
  
  lazy val iffOp : PackratParser[Expr] = (
		  implOp ~ "iff" ~ implOp ^^ asBinop
		  | implOp
  )
  
  lazy val implOp : PackratParser[Expr] = (
		  implOp ~ "impl" ~ orOp ^^ asBinop
		  | orOp
  )
  
  lazy val orOp : PackratParser[Expr] = (
		  orOp ~ "or" ~ andOp ^^ asBinop
		  | andOp
  )
  
  lazy val andOp : PackratParser[Expr] = (
		  andOp ~ "and" ~ notOp ^^ asBinop
		  | notOp
  )
  
  lazy val notOp = (
	  "not" ~ relOp ^^ asUnop
	  | relOp
  )
  
  lazy val relOp = (
		  addOp ~ ("<"|">"|"<="|">="|"="|"/="|"subset"|"in"|"notin") ~ addOp ^^ asBinop
		  | addOp
  )
    
  lazy val addOp : PackratParser[Expr] = (
		  facOp ~ ("+"|"union"|"-"|"with"|"less") ~ addOp ^^ asBinop
		  | facOp
  )
  
  lazy val facOp : PackratParser[Expr] = (
		  expOp ~ ("*"|"inter"|"/"|"div"|"mod") ~ facOp ^^ asBinop
		  | expOp
  )
  
  lazy val expOp : PackratParser[Expr] = (
		  expOp ~ "**" ~ hashOp ^^ asBinop
		  | hashOp
  )
  
  lazy val hashOp = (
		  "#" ~ unOp ^^ asUnop
		  | unOp
  )
  
  lazy val unOp = (
		  ("-"|"+") ~ coalesceOp ^^ asUnop
		  | coalesceOp
  )
  
  lazy val coalesceOp = (
		  rvalue ~ "?" ~ rvalue ^^ asBinop
		  | rvalue
  )
    
  lazy val rvalue : PackratParser[Expr] = (
		  stringLit ^^ { case s => StringConst(s) }
          | numericLit ^^ { case s => IntegerConst(s) }
          | ("true"|"false") ^^ { case s => BooleanConst(s) }
          | ("OM"|"om") ^^ { case s => UndefinedConst(s) }
          | "(" ~> expr <~ ")"
          | rvalue ~ "(" ~ repsep(expr, ",") ~ ")" ^^ { case f ~ _ ~ args ~ _ => CallExpr(f,args) }
          | "[" ~> repsep(expr, ",") <~ "]" ^^ { case e => TupleExpr(e) }
          | "{" ~> repsep(expr, ",") <~ "}" ^^ { case e => SetExpr(e) }
          | funcDecl
          | ident ^^ { case id => Identifier(id) }
  )
    
  lazy val singleLvalue : PackratParser[Lvalue] = (
		  singleLvalue ~ "(" ~ expr ~ ")" ^^ { case lv ~ _ ~ e ~ _ => DimLvalue(lv,e) }
		  | ident ^^ { case id => IdentifierLvalue(Identifier(id)) }
  )
  
  lazy val multipleLvalue : PackratParser[Lvalue] = (
		  "[" ~> rep1sep(lvalue, ",") <~ "]" ^^ { case lv => MultipleLvalue(lv) }
  )
  
  lazy val lvalue : PackratParser[Lvalue] = (
		  singleLvalue | multipleLvalue
  )
}





