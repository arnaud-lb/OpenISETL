/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler.analysis.dataholder

trait AnDataHolder {
  val map = collection.mutable.Map.empty[Any, Map[Any,Any]]
  def getAnData(h:Any) = map.get(h)
  def setAnData(h:Any, d:Map[Any,Any]) : Unit = map += (h -> d)
}
