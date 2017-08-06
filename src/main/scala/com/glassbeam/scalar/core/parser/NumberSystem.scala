package com.glassbeam.scalar.core.parser

import java.lang.{Long => LONG}

/**
  * Created by shiva on 8/9/16.
  */

// TODO: Support for BIGInts
trait NumberSystem {

  def binaryToDecimal(x: String) = LONG.parseLong(x, 2)

  def decimalToHex(x: Long) = LONG.toHexString(x)

  def binaryToHex(x: String) = decimalToHex(binaryToDecimal(x))

  def hexToBinary(x: String) = LONG.toBinaryString(LONG.parseLong(x, 16))

}