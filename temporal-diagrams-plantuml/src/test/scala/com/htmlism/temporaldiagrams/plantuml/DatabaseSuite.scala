package com.htmlism.temporaldiagrams.plantuml

import cats.data.*
import cats.syntax.all.*
import weaver.*

import com.htmlism.temporaldiagrams.DiagramEncoder

object DatabaseSuite extends FunSuite:
  test("A database has an name"):
    expect.eql(
      Chain("database asdf"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Database("asdf", None, None, Set.empty)
      )
    )

  test("A database has an optional override alias"):
    expect.eql(
      Chain("database foo as bar"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Database("foo", "bar".some, None, Set.empty)
      )
    )

  test("A database has an optional stereotype"):
    expect.eql(
      Chain("database foo << bar >>"),
      DiagramEncoder[PlantUml].encode(
        PlantUml.Database("foo", None, "bar".some, Set.empty)
      )
    )

  // TODO test nesting
