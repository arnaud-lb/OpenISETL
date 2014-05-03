/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler.analysis;

import scala.annotation.tailrec

import openisetl.compiler.parser.DeepVisitorAdapter
import openisetl.compiler.node._
import openisetl.compiler.Sym
import openisetl.compiler.SymTab
import openisetl.compiler.analysis.dataholder.AnDataHolderClient

object SymAn {
	val symTab = 0
	val sym = symTab + 1
	val upvars = sym + 1
}
class SymAn extends AnDataHolderClient {
	
	// function node -> symtab
	
	def setSymTab(fun:FuncDeclExpr, tab:SymTab) = setAnData(fun, (SymAn.symTab -> tab))
	def getSymTab(fun:FuncDeclExpr) = getAnData[SymTab](fun, SymAn.symTab)
	
	// identifier node -> symbol
	
	def setSym(n:Identifier, sym:Sym) = setAnData(n, SymAn.sym -> sym)
	def getSym(n:Identifier) = getAnData[Sym](n, SymAn.sym)
	
	// function node -> upvars
	
	def addUpvar(n:FuncDeclExpr, sym:Sym) : Sym = {
		setAnData(n, SymAn.upvars -> (getUpvars(n) + sym))
		sym
	}
	def setUpvars(n:FuncDeclExpr, v:Set[Sym]) = {
		setAnData(n, SymAn.upvars -> v)
	}
	def getUpvars(n:FuncDeclExpr) : Set[Sym] = {
		getAnData[Set[Sym]](n, SymAn.upvars).getOrElse(Set.empty[Sym])
	}
	
	def run(fun:FuncDeclExpr,
			lookup:List[SymTab] = Nil,
			predefs:Set[Sym] = Set.empty) : Unit = {
		
		val locales = new SymTab
		setSymTab(fun, locales)
		setUpvars(fun, predefs)
		
		queue += new An(fun, locales, locales, locales :: lookup)

		while (!queue.isEmpty) queue.dequeue.run
		
		new PropagUpvars(fun).run
	}
	private var queue = new collection.mutable.Queue[An]
	
	private class An(fun:FuncDeclExpr,
			defaultScope:SymTab, // where to bind variables by default
			localScope:SymTab,   // where to bind variables declared local
			protected var lookupScopes:List[SymTab])
			
			extends DeepVisitorAdapter {
		
		def run() = walkAny(fun.stmts)
		
		@tailrec
		private def lookup(name:String, s:List[SymTab]) : Option[Sym] = s match {
			case Nil => None
			case head :: tail => head.get(name) match {
				case opt @ Some(_) => opt
				case _ => lookup(name, tail)
			}
		}
		
		def lookup(name:String) : Option[Sym] = lookup(name, lookupScopes)
		
		override def visitLocalStmt(s:LocalStmt) = {
			s.locals.foreach((id:Identifier) => {
				setSym(id, localScope.add(id.id))
			})
		}
		
		override def visitForStmt(s:ForStmt) = {
			
			walkAny(s.sub)
			
			val newScope = localScope.createNested
			val an = new An(fun, newScope, localScope, 
					 newScope :: lookupScopes)
			an.walkAny(s.lvs) // bind lvalues to a "block scope"
			
			val saved = lookupScopes
			lookupScopes ::= newScope
			walkAny(s.stmts)
			lookupScopes = saved
		}
		
		override def visitFuncDeclExpr(e:FuncDeclExpr) = {
			val newLocales = new SymTab
			setSymTab(e, newLocales)
			e.params.foreach((id:Identifier) => {
					setSym(id, newLocales.add(id.id, Sym.param))
			})
			
			queue += new An(e, defaultScope, newLocales, 
					newLocales :: lookupScopes)
		}
		
		override def visitIdentifier(id:Identifier) = {
			var sym : Sym = lookup(id.id) match {
				case None => {
					var flags = 0
					val sym = if (id.id.startsWith("#local#"/*FIXME*/)) {
						localScope.add(id.id)
					} else {
						defaultScope.add(id.id)
					}
					if (!sym.scope.isLocalTo(localScope)) {
						addUpvar(fun, sym.maybeCreateUpvar)
					} else {
						sym
					}
				}
				case Some(sym) => {
					if (!sym.scope.isLocalTo(localScope)) {
						addUpvar(fun, sym.maybeCreateUpvar)
					} else {
						sym
					}
				}
			}
			setSym(id, sym)
		}
	}
	
	private class PropagUpvars(val fun:FuncDeclExpr)
			extends DeepVisitorAdapter {
		
		private val symTab = getSymTab(fun).get
		private var upvars = Set.empty[Sym]
		
		def run : Set[Sym] = {
			walkStmts(fun.stmts)
			val mergedUpvars = getUpvars(fun) ++ upvars
			setUpvars(fun, mergedUpvars)
			mergedUpvars
		}
		
		override def visitFuncDeclExpr(e:FuncDeclExpr) {
			val an = new PropagUpvars(e)
			upvars ++= an.run filterNot (_.scope isLocalTo symTab)
		}
	}
}
