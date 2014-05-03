/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler

import java.io.PrintWriter
import collection.mutable.ArrayBuffer
import parser.DeepVisitorAdapter
import node._
import analysis._
import openisetl.runtime._val._
import openisetl.runtime.annotations.{Upvar,Function}
import org.objectweb.asm.{Type,ClassVisitor,ClassWriter,FieldVisitor,Opcodes}
import org.objectweb.asm.util.{CheckClassAdapter,TraceClassVisitor}
import org.objectweb.asm.commons.{Method,GeneratorAdapter}
import scala.language.implicitConversions /*FIXME*/

object CodeGen {
	
	val upvarType = Type.getType(classOf[BaseValRef])
	val upvarDesc = upvarType.getDescriptor
	val varType = Type.getType(classOf[BaseVal])
	private var uniq = 0
	
	def create(fun:FuncDeclExpr, accessModeAn:AccessModeAn, symAn:SymAn) = {
		uniq += 1
		
		val classType = Type.getObjectType("openisetl/runtime/func/UserFunc$" + uniq)
		val writer = new ClassWriter(ClassWriter.COMPUTE_MAXS | ClassWriter.COMPUTE_FRAMES)
		var clazz : ClassVisitor = writer
		clazz = new CheckClassAdapter(clazz)
		// clazz = new TraceClassVisitor(clazz, new PrintWriter(System.err,true))
		
		clazz.visit(Opcodes.V1_6, Opcodes.ACC_PUBLIC, 
				classType.getInternalName, null, 
				Type.getType(classOf[FuncVal]).getInternalName, Array.empty)
		
		val ann = clazz.visitAnnotation(
				Type.getType(classOf[Function]).getDescriptor, true)
		fun.params.foldLeft(ann.visitArray("parameters")) { case (a, Identifier(name)) =>
			a.visit(null, name)
			a
		}.visitEnd()
		ann.visitEnd()
		
		val main = mkGen(mkMainMethod(fun), clazz)
		new CodeGen(fun, writer, clazz, classType, main, accessModeAn, symAn)
	}
	
	private def mkGen(meth:Method, writer:ClassVisitor, acc:Int = Opcodes.ACC_PUBLIC) = {
		new GeneratorAdapter(acc, meth, meth.getDescriptor, Array.empty, writer)
		
	}
	private def mkMainMethod(fun:FuncDeclExpr) = {
		Method.getMethod(BaseVal.callMethod(fun.params.length))
	}
		
	def arHandler(op:ArOp) = op match {
		case AddOp() => BaseVal.add
		case SubOp() => BaseVal.sub
		case MulOp() => BaseVal.mul
		case DivOp() => BaseVal.div
		case ModOp() => BaseVal.mod
		case WithOp() => BaseVal._with
		case LessOp() => BaseVal.less
		case ExpOp() => BaseVal.exp
		case CoalesceOp() => throw new Error()
	}
	def arUnUnHandler(op:ArUnOp) = op match {
		case SizeOp() => BaseVal.size
		case InvOp() => BaseVal.inv
	}
	def relUnHandler(op:RelUnOp) = op match {
		case NotOp() => BaseVal.not
	}
}
class CodeGen private (
				val fun : FuncDeclExpr,
				val _writer : ClassWriter,
				val clazz : ClassVisitor, 
				val selfType : Type, 
				val main : GeneratorAdapter,
				val accessModeAn : AccessModeAn,
				val symAn : SymAn) 
		extends DeepVisitorAdapter {
	
	implicit def toType[T](c:Class[T]) = {
		Type.getType(c)
	}
	implicit def toMethod(m:java.lang.reflect.Method) = {
		Method.getMethod(m)
	}
	implicit def toMethod[T](m:java.lang.reflect.Constructor[T]) = {
		Method.getMethod(m)
	}
	
	var upvars = Map.empty[Sym, String]
	var literals = Map.empty[(Class[T] forSome { type T <: BaseVal }, Any), String]
	
	val clinit = CodeGen.mkGen(
			new Method("<clinit>", Type.VOID_TYPE, Array[Type]()), 
			clazz)
	clinit.visitCode()

	def run : Class[FuncVal] = {
		
		System.err.println("<run>")
		
		symAn.getUpvars(fun).foldLeft(0) { (n:Int, sym:Sym) =>
			val name = "$upvar$" + sym.name + "$" + n
			upvars += (sym -> name)
			
			val fv = clazz.visitField(Opcodes.ACC_PRIVATE | Opcodes.ACC_FINAL, name, 
					CodeGen.upvarDesc, null, null)
			val ann = fv.visitAnnotation(Type.getType(classOf[Upvar]).getDescriptor, true)
			ann.visit("name", sym.name)
			ann.visitEnd()
			fv.visitEnd()
			
			n + 1
		}
		
		createInitClosureMeth()
		createInit1Meth()
		createFuncNameMeth()
		createParamCountMeth()
		createGetUpvarMeth()
		
		main.visitCode()

		val symTab = symAn.getSymTab(fun).get
		
		val nlocals = symTab.deepSize - fun.params.size
		if (nlocals > 0) {
			main.getStatic(classOf[BaseValRef], "undefined", classOf[UndefinedVal])
			for (i <- 1 until nlocals) main.dup()
		}
			
		symTab.deepFoldLeft(0) { case (n, (name, sym)) =>
			sym match {
				case Sym(_, _, Sym.Param(), None) =>
					sym.physAddr = n
					n + 1
				case Sym(_, _, Sym.Param(), Some(upvar)) =>
					sym.kind = Sym.local
					sym.physAddr = main.newLocal(CodeGen.varType)
					
					main.newInstance(classOf[BaseValRef])
					main.dup() // jvm does not like dup2 here
					main.invokeConstructor(classOf[BaseValRef],
							classOf[BaseValRef].getConstructor())
					main.storeLocal(sym.physAddr)
					main.loadLocal(sym.physAddr)
					main.loadArg(n)
					main.putField(CodeGen.upvarType , "ref", CodeGen.varType)
					
					n + 1

				case Sym(_, _, Sym.Local(), None) =>
					sym.physAddr = main.newLocal(CodeGen.varType)
					main.storeLocal(sym.physAddr)
					n

				case Sym(_, _, Sym.Local(), Some(upvar)) =>
					sym.physAddr = main.newLocal(CodeGen.varType)
					main.newInstance(classOf[BaseValRef])
					main.dup() // jvm does not like dup2 here
					main.invokeConstructor(classOf[BaseValRef],
							classOf[BaseValRef].getConstructor())
					main.storeLocal(sym.physAddr)
					main.loadLocal(sym.physAddr)
					main.swap()
					main.putField(CodeGen.upvarType , "ref", CodeGen.varType)
					n

				case Sym(_, _, Sym.Upvar(), _) => throw new Error()
			}
		}
		
		walkStmts(fun.stmts)
		
		main.getStatic(classOf[BaseValRef], "undefined", classOf[UndefinedVal])
		main.returnValue()
		finalizeMeth(main)
		
		clinit.returnValue()
		finalizeMeth(clinit)
		
		clazz.visitEnd()
		
		System.err.println("</run>")

		UserClassLoader.load(selfType.getClassName, _writer.toByteArray)
			.asInstanceOf[Class[FuncVal]]
	}
	
	def createFuncNameMeth() {
		val funcName = CodeGen.mkGen(FuncVal.funcName, clazz)
		
		funcName.visitCode()
		funcName.push("anon")
		funcName.returnValue()
		finalizeMeth(funcName)
	}
	
	def createParamCountMeth() {
		val paramCount = CodeGen.mkGen(FuncVal.paramCount, clazz)
		
		paramCount.visitCode()
		if (fun.params.size > BaseVal.maxParams) {
			paramCount.push(-1)
		} else {
			paramCount.push(fun.params.size)
		}
		paramCount.returnValue()
		finalizeMeth(paramCount)
	}
	
	def createInitClosureMeth() = {
		val types = ArrayBuffer.empty[Type].padTo(upvars.size, 
				CodeGen.upvarType).toArray
		val m = CodeGen.mkGen(new Method("<init>", Type.VOID_TYPE, 
				types), clazz)
		m.visitCode()
		m.loadThis()
		m.invokeConstructor(classOf[FuncVal], FuncVal.init0)

		upvars.foldLeft(0) { case (n, (sym, field)) =>
			m.loadThis()
			m.loadArg(n)
			m.putField(selfType, field, CodeGen.upvarType)
			n + 1
		}

		m.returnValue()
		finalizeMeth(m)
	}
	
	def createInit1Meth() {
		val m = CodeGen.mkGen(FuncVal.init1, clazz)
		m.visitCode()
		m.loadThis()
		m.invokeConstructor(classOf[FuncVal], FuncVal.init0)

		upvars.foreach { case (sym, field) =>
			m.loadThis()
			m.dup()
			m.push(sym.name)
			m.loadArg(0)
			m.invokeVirtual(classOf[FuncVal], FuncVal.getUpvarFrom)
			m.putField(selfType, field, CodeGen.upvarType)
		}

		m.returnValue()
		finalizeMeth(m)
	}
	
	def createGetUpvarMeth() {
		val m = CodeGen.mkGen(FuncVal.getUpvar, clazz)
		m.visitCode()
		
		upvars.foreach { case (sym, field) =>
			val nextL = m.newLabel
			
			m.push(sym.name)
			m.loadArg(0)
			m.invokeVirtual(classOf[Object], 
					classOf[Object].getMethod("equals", classOf[Object]))
			m.visitJumpInsn(Opcodes.IFEQ, nextL)
			m.loadThis()
			m.getField(selfType, upvars.get(sym).get,
					CodeGen.upvarType)
			m.returnValue()

			m.mark(nextL)
		}
		
		m.loadThis()
		m.loadArg(0)
		invokeSuper(m, classOf[FuncVal], FuncVal.getUpvar)
		m.returnValue()
		
		finalizeMeth(m)
	}
	
	def pushNull(g:GeneratorAdapter) {
		g.visitInsn(Opcodes.ACONST_NULL)
	}
	
	def invokeSuper(g:GeneratorAdapter, sup:Type, m:Method) {
		g.visitMethodInsn(Opcodes.INVOKESPECIAL, 
				sup.getInternalName, m.getName, m.getDescriptor) 
	}
	
	def finalizeMeth(m:GeneratorAdapter) {
		m.visitMaxs(20,20)
		m.visitEnd()
	}
	
	def loadUndefined() {
		main.getStatic(classOf[BaseValRef], "undefined", classOf[UndefinedVal])
	}
	
	def loadTrue() {
		main.getStatic(classOf[BaseValRef], "_true", classOf[BooleanVal])
	}
	
	def loadFalse() {
		main.getStatic(classOf[BaseValRef], "_false", classOf[BooleanVal])
	}
	
	def literalFieldFor[T <: BaseVal](c:Class[T], v:Any)(init:(String) => Unit) : String = {
		literals.get((c, v)) match {
			case Some(name) => name
			case None =>
				val name = "$literal$$" + literals.size
				literals += ((c,v) -> name)
				
				clazz.visitField(Opcodes.ACC_PRIVATE | 
						Opcodes.ACC_STATIC | Opcodes.ACC_FINAL,
						name, Type.getType(c).getDescriptor, null, null).visitEnd()

				init(name)
				
				name
		}
	}
	
	def loadInteger(i:Int) {
		val field = literalFieldFor(classOf[IntegerVal], i) { name =>
			clinit.newInstance(classOf[IntegerVal])
			clinit.dup()
			clinit.push(i)
			clinit.invokeConstructor(classOf[IntegerVal], 
					classOf[IntegerVal].getConstructor(classOf[Int]))
			clinit.putStatic(selfType, name, classOf[IntegerVal])
		}
		main.getStatic(selfType, field, classOf[IntegerVal])
	}
	
	def loadString(s:String) {
		val field = literalFieldFor(classOf[StringVal], s) { name =>
			clinit.newInstance(classOf[StringVal])
			clinit.dup()
			clinit.push(s)
			clinit.invokeConstructor(classOf[StringVal], 
					classOf[StringVal].getConstructor(classOf[String]))
			clinit.putStatic(selfType, name, classOf[StringVal])
		}
		main.getStatic(selfType, field, classOf[StringVal])	
	}
	
	override def outUndefinedConst(e:UndefinedConst) {
		loadUndefined()
	}
	
	override def outBooleanConst(e:BooleanConst) = e.text match {
		case "true" => loadTrue()
		case "false" => loadFalse()
	}
	
	override def outIntegerConst(e:IntegerConst) {
		loadInteger(Integer.parseInt(e.text))
	}
	
	override def outStringConst(e:StringConst) {
		loadString(e.text)
	}
	
	override def visitBinopExpr(e:BinopExpr) {
		e.op match {
			case op @ ArOp() =>
				
				walkAny(e.lhs)
				walkAny(e.rhs)
				
				main.invokeVirtual(CodeGen.varType, CodeGen.arHandler(op))

			case op @ (GtOp() | LtOp() | GeOp() | LeOp() | EqOp() | NeOp() | InOp() | SubsetOp()) =>
				
				walkAny(e.lhs)
				walkAny(e.rhs)
				
				val (cmpop:Int, cmphandler:java.lang.reflect.Method) = op match {
					case GtOp() => (Opcodes.IFLE, BaseVal.rel)
					case LtOp() => (Opcodes.IFGE, BaseVal.rel)
					case GeOp() => (Opcodes.IFLT, BaseVal.rel)
					case LeOp() => (Opcodes.IFGT, BaseVal.rel)
					case EqOp() => (Opcodes.IFEQ, BaseVal.eq)
					case NeOp() => (Opcodes.IFNE, BaseVal.eq)
					case InOp() => (Opcodes.IFEQ, BaseVal.in)
					case SubsetOp() => (Opcodes.IFEQ, BaseVal.subset)
					case _ => throw new Error()
				}

				val trueLabel = main.newLabel
				val falseLabel = main.newLabel
				val retLabel = main.newLabel

				main.invokeVirtual(CodeGen.varType, cmphandler)

				main.visitJumpInsn(cmpop, falseLabel)
				main.goTo(trueLabel)
				
				// true
				main.mark(trueLabel)
				loadTrue()
				main.goTo(retLabel)
				
				// false
				main.mark(falseLabel)
				loadFalse()
				main.goTo(retLabel)
				
				// ret
				main.mark(retLabel)

			case op @ AndOp() =>
				
				val falseLabel = main.newLabel
				val retLabel = main.newLabel

				walkAny(e.lhs)
				loadTrue()
				main.ifCmp(classOf[BooleanVal], GeneratorAdapter.NE, falseLabel)

				// true
				walkAny(e.rhs)
				main.goTo(retLabel)
				
				// false
				main.mark(falseLabel)
				loadFalse()

				// ret
				main.mark(retLabel)

			case op @ OrOp() =>
				
				val falseLabel = main.newLabel
				val retLabel = main.newLabel

				walkAny(e.lhs)
				loadTrue()
				main.ifCmp(classOf[BooleanVal], GeneratorAdapter.NE, falseLabel)

				// true
				loadTrue()
				main.goTo(retLabel)
				
				// false
				main.mark(falseLabel)
				walkAny(e.rhs)

				// ret
				main.mark(retLabel)

			case (ImplOp() | NotinOp()) => throw new Error()
		}
	}
	
	override def visitIfStmt(s:IfStmt) {
		
		val trueLabel = main.newLabel
		val falseLabel = main.newLabel
		val retLabel = main.newLabel
		
		walkAny(s.cond)
		loadTrue()
		main.ifCmp(classOf[BooleanVal], GeneratorAdapter.NE, falseLabel)
		main.goTo(trueLabel)
		
		// true
		main.mark(trueLabel)
		walkAny(s.stmts)
		main.goTo(retLabel)
		
		// false
		main.mark(falseLabel)
		walkAny(s.fb)
		main.goTo(retLabel)
		
		main.mark(retLabel)
	}
	
	override def visitWhileStmt(s:WhileStmt) {
		
		val condLabel = main.newLabel
		val retLabel = main.newLabel
		
		main.mark(condLabel)
		walkAny(s.cond)
		loadTrue()
		main.ifCmp(classOf[BooleanVal], GeneratorAdapter.NE, retLabel)

		walkAny(s.stmts)
		main.goTo(condLabel)
		
		main.mark(retLabel)
	}

	override def visitForStmt(e:ForStmt) {

		val iterType = Type.getType(classOf[Iterator[BaseVal]])
		val iterHasNextMeth = classOf[Iterator[BaseVal]].getMethod("hasNext")
		val iterNextMeth = classOf[Iterator[BaseVal]].getMethod("next")

		walkAny(e.sub)

		def visitLv(lv:Node, lvs:List[Node]) {

			val nextLabel = main.newLabel
			val retLabel = main.newLabel
			val cleanLabel = main.newLabel

			main.dup()
			main.invokeVirtual(CodeGen.varType, BaseVal.iter)

			main.mark(nextLabel)

			main.dup()
			main.invokeInterface(iterType, iterHasNextMeth)
			main.ifZCmp(GeneratorAdapter.EQ, retLabel)

			main.dup()
			main.invokeInterface(iterType, iterNextMeth)
			main.checkCast(CodeGen.varType)

			preAssignAndSwap.walkAny(lv)
			doAssign.walkAny(lv)

			lvs match {
				case lv_ :: lvs_ =>
					main.swap()
					visitLv(lv_, lvs_)
					main.swap()
				case List() => walkAny(e.stmts)
			}

			main.goTo(nextLabel)

			main.mark(cleanLabel)
			main.pop() // iter

			main.mark(retLabel)
			main.pop() // sub
		}

		visitLv(e.lvs.head, e.lvs.tail)
	}

	override def visitCallExpr(e:CallExpr) {
		
		walkAny(e.fun)
		
		if (e.args.size > BaseVal.maxParams) {
			main.newArray(CodeGen.varType)
			for (i <- 0 until e.args.size)
				main.dup()
			e.args.foldLeft(0) { (n:Int, arg:Expr) =>
				main.push(n)
				walkAny(arg)
				main.arrayStore(CodeGen.varType)
				n + 1
			}
		} else {
			walkAny(e.args)
		}
		
		val m = BaseVal.callMethod(e.args.size)
		main.invokeVirtual(CodeGen.varType, m)

		/*
		main.dup
		main.invokeVirtual(classOf[Object],
				classOf[Object].getMethod("toString"))
		main.getStatic(classOf[System],
				"out", classOf[java.io.PrintStream])
		main.swap
		main.invokeVirtual(classOf[java.io.PrintStream],
				classOf[java.io.PrintStream].getMethod("println",classOf[String]))
		*/
	}
	
	
	override def visitFuncDeclExpr(e:FuncDeclExpr) {
		
		val cg = CodeGen.create(e, accessModeAn, symAn)
		val c = cg.run
		
		main.newInstance(c)
		main.dup()
		symAn.getUpvars(e).foreach { sym =>
			if (sym.upvar.get.scope isLocalTo symAn.getSymTab(fun).get) {
				loadVarRef(main, sym.upvar.get)
			} else {
				loadVarRef(main, sym)
			}
		}
		main.invokeConstructor(c, 
				initMethod(c, symAn.getUpvars(e).size))
	}
	
	def wtfSyms(sym:Sym) = sym match {
		case Sym(_, _, Sym.Upvar(), Some(upvar)) =>
			"closed variable in a closure; " +
			"stored in a field of the closure's instance; " +
			"upvar refers to the real, Local/Param Sym"

		case Sym(_, _, Sym.Local(), Some(upvar)) =>
			"a local variable which is closed in some closure; " +
			"upvar refers to the Upvar Sym"

		case Sym(_, _, Sym.Local(), None) =>
			"a local variable"

		case Sym(_, _, Sym.Param(), None) =>
			"an argument variable"

		case _ => "invalid Sym"
	}

	private def preStoreVar(m:GeneratorAdapter, sym:Sym, swap:Boolean = false) = sym match {
		case Sym(_, _, Sym.Upvar(), Some(upvar)) =>
			m.loadThis()
			m.getField(selfType, upvars.get(sym).get, CodeGen.upvarType)
			if (swap) {
				m.swap()
			}
		case Sym(_, _, Sym.Local(), Some(upvar)) =>
			m.loadLocal(sym.physAddr)
			if (swap) {
				m.swap()
			}
		case Sym(_, _, _, _) =>
	}

	private def storeVar(m:GeneratorAdapter, sym:Sym) = sym match {
		case Sym(_, _, Sym.Upvar(), Some(_)) | Sym(_, _, Sym.Local(), Some(_)) =>
			m.putField(CodeGen.upvarType, "ref", CodeGen.varType)
		case Sym(_, _, Sym.Param(), None) =>
			m.storeArg(sym.physAddr)
		case Sym(_, _, Sym.Local(), None) =>
			m.storeLocal(sym.physAddr)
		case _ => throw new Error()
	}
	
	private def loadVar(m:GeneratorAdapter, sym:Sym) = sym match {
		case Sym(_, _, Sym.Upvar(), Some(upvar)) =>
			m.loadThis()
			m.getField(selfType, upvars.get(sym).get, CodeGen.upvarType)
			m.getField(CodeGen.upvarType, "ref", CodeGen.varType)
		case Sym(_, _, Sym.Local(), Some(upvar)) =>
			m.loadLocal(sym.physAddr)
			m.getField(CodeGen.upvarType, "ref", CodeGen.varType)
		case Sym(_, _, Sym.Param(), None) =>
			m.loadArg(sym.physAddr)
		case Sym(_, _, Sym.Local(), None) =>
			m.loadLocal(sym.physAddr)
		case _ => throw new Error()
	}
	
	private def loadVarRef(m:GeneratorAdapter, sym:Sym) = sym match {
		case Sym(_, _, Sym.Upvar(), Some(upvar)) =>
			m.loadThis()
			m.getField(selfType, upvars.get(sym).get, CodeGen.upvarType)
		case Sym(_, _, Sym.Local(), Some(upvar)) =>
			m.loadLocal(sym.physAddr)
		case _ => throw new Error()
	}

	override def visitIdentifier(e:Identifier) {
		val sym = symAn.getSym(e).get
		accessModeAn.getAccessMode(e) match {
			case Some(AccessModeAn.Load()) => loadVar(main, sym)
			case _ => 
		}
	}
	
	private object preAssign extends DeepVisitorAdapter {
		override def visitIdentifierLvalue(l:IdentifierLvalue) {
			val sym = symAn.getSym(l.id).get
			preStoreVar(main, sym)
		}
	}

	private object preAssignAndSwap extends DeepVisitorAdapter {
		override def visitIdentifierLvalue(l:IdentifierLvalue) {
			val sym = symAn.getSym(l.id).get
			preStoreVar(main, sym, true)
		}
	}

	private object doAssign extends DeepVisitorAdapter {
		override def visitIdentifierLvalue(l:IdentifierLvalue) {
			val sym = symAn.getSym(l.id).get
			storeVar(main, sym)
		}
	}
	
	override def visitAssignStmt(s:AssignStmt) {
		preAssign.walkAny(s.lv)
		super.walkAny(s.expr)
		doAssign.walkAny(s.lv)
	}
	
	override def outNullExprStmt(s:NullExprStmt) {
		main.pop()
	}
	
	override def visitReturnStmt(s:ReturnStmt) {
		s.expr match {
			case Some(expr) => walkAny(expr) 
			case _ => loadUndefined()
		}
		main.returnValue()
	}
	
	override def outStmtsStmt(s:StmtsStmt) = Unit
	override def visitLocalStmt(s:LocalStmt) = Unit
	
	override def defaultOut(n:Node) {
		if(true) throw new Error("defaultOut: " + n)
	}
	
	def initMethod[T](c:Class[T], n:Int) = {
		val getConstr = c.getClass 
		                        .getMethod("getConstructor", 
		                        		classOf[Array[Class[_]]])
		val params = Array[Class[_]]().padTo(n, classOf[BaseValRef])
		getConstr.invoke(c, params).asInstanceOf[java.lang.reflect.Constructor[T]]
	}
}