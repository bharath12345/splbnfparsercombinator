package com.glassbeam.scalar.core.colops

import com.glassbeam.scalar.model.LookupKey

/** *
  * This is the cache Implementation
  */
object ColLookUpCache {

  private var cache = Map[String, Map[LookupKey, String]]()

  def loadItemsToCache = {
    val lookUpValues = com.glassbeam.scalar.model.ColLookupTableDao.loadLookupValue
    cache = lookUpValues
  }

  def lookupValueOfKey(emps: String, key: String, position: Int = 0): Option[String] = {
    val empsMap = cache(emps)
    empsMap.get(LookupKey(key, position)).map(x => x)
  }

}
