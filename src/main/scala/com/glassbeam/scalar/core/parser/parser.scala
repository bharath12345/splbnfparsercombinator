package com.glassbeam.scalar.core

package object parser {

  object Ops extends Enumeration {
    type Ops = Value
    val COLFILL, COLDROP, COLJOIN, COLREP, COLSPLIT, COLHIERARCHY, COLCOPY, COLMAP, COLCALC, COLBOUND, COLBRANCH, COLFILE,
    CONSTRAIN, ROWSPLIT, COLPUSH, COLASSERT, COLPRINT, ADD_ROW_NUMBER, COLCASE, COLWHEN, COLELSE, COLEND, COLLOOKUPBYNAME,
    COLLOOKUPBYPOSITION, COLCOUNT, ROWDROP, Invalid = Value
    // NOTE: Don't add any values without also adding to NRargs
  }

  object Funcs extends Enumeration {
    type Funcs = Value
    val ADJYEAR, INT, MD5, LENGTH, MINUS, CONCAT, PLUS, TIMES, DIVIDEBY, TIME2QTR, TIME2MONTH, TIME2WEEK, TIME2DAY,
    TIME2HOUR, TIME24HOUR, TIME210MIN, UC, LC, RANDINT, XTOPOWY, HEX2DEC, GMTIME, LOCALTIME, ZEROPAD, STR2MMYY, STR2TIME,
    STR2SUM, SDF2EPOCH, EPOCH2SDF, DATEDIFF , TIME2MIN, TIME230DAY, HEX2BIN, BIN2HEX, DISCARDOLDTIME, InvalidFunc = Value
  }

  // Add an enumerated object
  object CASES extends Enumeration {
    type CASE = Value
    val NOCASE, // NO case in effect
    CASEWHEN, // CASE, looking for WHEN or ELSE
    CASETHEN, // CASE, executing (until WHEN, ELSE)
    CASEEND = Value // CASE, looking for END
  }

}
