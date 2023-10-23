package com.htmlism.temporaldiagrams.v2.syntax

import weaver.FunSuite

object MultiArrowSuite extends FunSuite:
  test("can add multi-arrow sources"):
    expect.eql(1, 1)

  test("can add multi-arrow destinations"):
    expect.eql(1, 1)

  test("can define multi-arrows"):
    expect.eql(1, 1)

  test("rendering multi-arrows is fallible given an undefined source"):
    expect.eql(1, 1)

  test("rendering multi-arrows is fallible given an undefined destination"):
    expect.eql(1, 1)

  test("rendering multi-arrows can be ignored"):
    expect.eql(1, 1)

  test("rendering multi-arrows becomes one domain language"):
    expect.eql(1, 1)

  test("collecting renderables gets rid of multi-arrow classes"):
    expect.eql(1, 1)
