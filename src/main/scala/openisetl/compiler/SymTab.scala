/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler

object Sym {
	sealed abstract class Kind
	case class Upvar private[Sym]() extends Kind
	case class Param private[Sym]() extends Kind
	case class Local private[Sym]() extends Kind
	
	val upvar = Upvar()
	val param = Param()
	val local = Local()
	
	def unapply(sym:Sym) = sym match {
		case null => None
		case _ => Some((sym.name,sym.scope,sym.kind,sym.upvar))
	}
}

class Sym(val name:String, 
			val scope:SymTab,
			private var _kind:Sym.Kind = Sym.local,
			private var _upvar : Option[Sym] = None,
			var physAddr:Int = -1) {
	
	/**
	 * If kind is Local or Param, this is a virtual Sym refering to this
	 * If kind is Upvar, this is the Sym this upvar closes
	 */
	def upvar = _upvar
	
	def kind = _kind
	def kind_=(newkind:Sym.Local) {
		(_kind -> newkind) match {
			case (Sym.Param(), Sym.Local()) => _kind = newkind
			case _ => throw new Error();
		}
	}
	
	def copy(name:String=name,scope:SymTab=scope,
			kind:Sym.Kind=kind,upvar:Option[Sym]=upvar,
			physAddr:Int=physAddr) = {
		new Sym(name,scope,kind,upvar,physAddr)
	}
	
	def real = {
		if (kind == Sym.upvar) _upvar
		else this
	}
	
	def maybeCreateUpvar() = {
		if (kind == Sym.upvar) throw new Error()
		upvar match {
			case Some(u) => u
			case _ => {
				val u = copy(kind=Sym.upvar, upvar=Some(this))
				this._upvar = Some(u)
				u
			}
		}
	}
	
	override def toString = {
		"Sym(" + name + ", _, " + kind + ", " + (upvar != None) + ", " + physAddr + ")" 
	}
}

class SymTab {
	
	import scala.collection.mutable.LinkedHashSet

	var syms : Map[String,Sym] = Map.empty
	private val nested = LinkedHashSet.empty[SymTab]
	
	private var temps = 0
	
	def createNested = {
		val n = new SymTab
		nested += n
		n
	}
	
	def get(name : String) = syms.get(name)
	
	def add(name: String) : Sym = _add(name, Sym.local)
	
	def add(name: String, kind: Sym.Local) : Sym = 
		_add(name, kind)
		
	def add(name: String, kind: Sym.Param) : Sym = 
		_add(name, kind)
		
	protected def _add(name : String, kind: Sym.Kind) : Sym = {
		if (kind == Sym.upvar) throw new Error();
		val sym = new Sym(name, this, kind)
		syms += (name -> sym)
		sym
	}
	
	def addTmp = add("#tmp#" + (temps += 1), Sym.local)
	
	def contains(other:SymTab) : Boolean = {
		nested.contains(other) || nested.exists(_.contains(other))
	}
	
	def isLocalTo(other:SymTab) : Boolean = {
		this == other || other.contains(this) 
	}
	
	def deepForeach[B](f:((String,Sym)) => B) : Unit = {
		syms.foreach(f)
		nested.foreach(_.deepForeach(f))
	}
	
	def deepFoldLeft[A,B](z:A)(f:(A,(String,Sym)) => A) : A = {
		val newz = syms.foldLeft(z)(f)
		nested.foldLeft(newz)((z:A,tab:SymTab) => tab.deepFoldLeft(z)(f))
	}
	
	def deepFilter(f:((String,Sym)) => Boolean) : Map[String,Sym] = {
		deepFoldLeft(Map.empty[String,Sym]) { (m,e:(String,Sym)) =>
			if (f(e)) m + e
			else m
		}
	}
	
	def deepSize : Int = {
		syms.size + nested.foldLeft(0)(_+_.deepSize)
	}
}
