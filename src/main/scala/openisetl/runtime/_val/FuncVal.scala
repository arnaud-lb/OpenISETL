/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.runtime._val

import java.lang.reflect.Field
import openisetl.runtime.annotations.{Upvar,Function}

object FuncVal {
	
	def create[T <: FuncVal](c:Class[T], 
			map:Map[String,BaseValRef]) : T = {
		
		c.getConstructor(classOf[Map[String,BaseValRef]])
			.newInstance(map)
	}
	
	def getParameters(c:Class[_]) : Seq[String] = {
		val ann = c.getAnnotation(classOf[Function])
		if (ann == null) {
			Nil
		} else {
			ann.parameters
		}
	}
	
	def getUpvars(c:Class[_]) : Seq[String] = {
		c.getDeclaredFields.foldLeft(List.empty[String]) { case (m, field) =>
			field.getAnnotation(classOf[Upvar]) match {
				case null => m
				case ann @ _ => ann.name :: m
			}
		}
	}
	
	// method types for code gen
	
	val init0 = classOf[FuncVal].getConstructor()
	val init1 = classOf[FuncVal].getConstructor(classOf[Map[String,BaseValRef]])
	val getUpvars = classOf[FuncVal].getMethod("getUpvars")
	val getUpvar = classOf[FuncVal].getMethod("getUpvar", classOf[String])
	val getUpvarFrom = classOf[FuncVal].getMethod("getUpvarFrom",
						classOf[String], classOf[Map[String,BaseValRef]])
	val funcName = classOf[FuncVal].getMethod("funcName")
	val paramCount = classOf[FuncVal].getMethod("paramCount")
}

/* must not be case class
 * so that two instances of a same FuncVal do not equal */
abstract class FuncVal extends GenericBaseVal {
	
	override def typeName = getUpvars.size match {
		case 0 => "Function"
		case _ => "Closure"
	}
	override def toString = {
		var res = new StringBuilder() ++ typeName ++ " " ++ funcName ++ "(" ++
				FuncVal.getParameters(getClass).mkString(", ")
				
		val upvars = getUpvars
		System.err.println(upvars)
		if (upvars.size > 0) {
			res ++= " with closed vars " ++ upvars.map({ case (name, ref) =>
				(name, ref.ref)
			}).mkString(", ")
		}
		res ++= ")"
		res.mkString
	}

	def funcName : String
	override def paramCount : Int
	
	def getUpvars : Map[String,BaseValRef] = {
		getClass.getDeclaredFields.foldLeft(Map.empty[String,BaseValRef]) { case (m, field) =>
			field.getAnnotation(classOf[Upvar]) match {
				case null => m
				case ann @ _ => m + (ann.name -> getUpvar(ann.name))
			}
		}
	}
	
	def getUpvar(name:String) : BaseValRef = {
		if (true) throw new NoSuchElementException("var " + name + " does not exists");
		null
	}
	
	protected def getUpvarFrom(name:String, from:Map[String,BaseValRef]) : BaseValRef = {
		from.get(name) match {
			case Some(v) => v
			case _ => throw new Error("var " + name + " not found in given map")
		}
	}
	
	def this(map:Map[String,BaseValRef]) {
		this()
	}
}