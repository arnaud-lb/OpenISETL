/*
 * This file is part of openisetl
 *
 * Copyright (c) Arnaud Le Blanc
 */

package openisetl.compiler.analysis.dataholder

trait AnDataHolderClient {
  def getAnData[T](h:AnDataHolder, k:Any) : Option[T] = {
    h.getAnData(this) match {
      case Some(d) => d.get(k) match {
        case Some(v) => Some(v.asInstanceOf[T])
        case _ => None
      }
      case _ => None
    }
  }
  def setAnData(h:AnDataHolder, kv:(Any, Any)) = {
    val anData : Map[Any,Any] = h.getAnData(this) match {
      case Some(d) => d
      case _ => Map.empty
    }
    h.setAnData(this, anData + kv)
  }
}