/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler

object UserClassLoader extends java.lang.ClassLoader {
                              
    def load(name:String, b:Array[Byte]) = {
		super.defineClass(name, b, 0, b.length)
	}
}