/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */


object test {

	import java.io.{File,Reader,FileReader}
	import openisetl.compiler.parser.OpenisetlParser
	import openisetl.compiler.analysis._
	import openisetl.compiler.transform._
	import openisetl.compiler.node._
	import openisetl.compiler.CodeGen
	import openisetl.compiler.{SymTab,Sym}
	import openisetl.runtime.func._
	import openisetl.runtime._val.{BaseVal,BaseValRef,FuncVal}
	import openisetl.runtime.ExecuteException
  import scala.language.implicitConversions
	
	implicit def val2valref(v:BaseVal) = new BaseValRef(v)
	
	val predefs = Map[String,BaseValRef] (
			"print" -> PrintFunc,
			"set_create" -> SetCreateFunc,
			"tuple_create" -> TupleCreateFunc,
			"__count" -> CountFunc
	)
	
	def main(args:Array[String]) : Unit = {
		
		val parser = new OpenisetlParser;
		val result = parser.parse(new File(args(0)))
		var tree = result match {
			case parser.Success(tree, _) => tree
			case e: parser.NoSuccess => {
				Console.err.println(e)
				sys.exit(100)
			}
		}
		var fun = FuncDeclExpr(List(), tree)
		
		val sugarTr = new SugarTransform
		val accessModeAn = new AccessModeAn
		val symAn = new SymAn
		
		val predefTab = new SymTab
		var predefSet = predefs.foldLeft(Set.empty[Sym]) { case (s, (name, _)) =>
				s + predefTab.add(name).maybeCreateUpvar
		}
		
		fun = sugarTr.visitFuncDeclExpr(fun)
		// new PrinterVisitor().walkAny(fun)
		accessModeAn.visitFuncDeclExpr(fun)
		symAn.run(fun, List(predefTab), predefSet)
		
		val codegen = CodeGen.create(fun, accessModeAn, symAn)
		val clazz = codegen.run
		val code = FuncVal.create(clazz, predefs)
		
//				
//		System.err.println(clazz.getDeclaredFields.length)
//		clazz.getDeclaredFields.foreach { case field =>
//		System.err.println(field.getName())
//			import openisetl.runtime._val._
//			System.err.println(field.getAnnotation(classOf[Upvar]).name)
//			field.set(code, new BaseValRef)
//		}
		
		try {
			println(code.call)
		} catch {
			case e:ExecuteException => {
				System.err.println(e.getMessage)
				e.printStackTrace
			}
		}
	}
	
}
