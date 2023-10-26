package com.htmlism.temporaldiagrams.demo.v2

import scala.util.chaining.*

import cats.*
import cats.data.*
import cats.effect.*
import cats.syntax.all.*

import com.htmlism.temporaldiagrams.demo.FilePrinter
import com.htmlism.temporaldiagrams.plantuml.PlantUml
import com.htmlism.temporaldiagrams.v2.Renderable
import com.htmlism.temporaldiagrams.v2.syntax.*

object WriteOuroborosDiagram extends WriteOuroborosDiagram[IO](FilePrinter[IO]) with IOApp.Simple:
  def variant(showMermaid: Boolean) =
    Kleisli.fromFunction[Id, OuroborosDsl.Config.Variant][Chain[Renderable[Chain[PlantUml]]]]:
      case OuroborosDsl.Config.Variant(namespace, oEncoder) =>
        def w(s: String) =
          if namespace.isEmpty then s
          else s"$s ($namespace)"

        val (((configWrap, highlightEncoder), diagramEncoder), last) =
          oEncoder match
            case None =>
              None -> None -> None -> None

            case Some(OuroborosDsl.Config.Encoder.ConfigToBusinessDsl) =>
              Some("FunctionEncoder") -> Some("FunctionEncoder") -> None -> None

            case Some(OuroborosDsl.Config.Encoder.Highlight) =>
              None -> Some("HighlightEncoder") -> Some("HighlightEncoder") -> None

            case Some(OuroborosDsl.Config.Encoder.Diagram) =>
              None -> None -> Some("DiagramEncoder") -> Some("DiagramEncoder")

        val mermaid =
          if showMermaid then
            Chain[Renderable[Chain[PlantUml]]](
              OuroborosDsl.Type(w("TemporalDiagrams.Mermaid"), diagramEncoder).r,
              OuroborosDsl.Output(w("Mermaid"), None).r,
              OuroborosDsl.Link(w("UserDsl"), w("TemporalDiagrams.Mermaid")).r,
              OuroborosDsl.Link(w("TemporalDiagrams.Mermaid"), w("Mermaid")).r,
            )
          else
            Chain.empty

        Chain[Renderable[Chain[PlantUml]]](
          OuroborosDsl.Type(w("UserDsl.Config"), configWrap).r,
          OuroborosDsl.Type(w("UserDsl"), highlightEncoder).r,
          OuroborosDsl.Type(w("TemporalDiagrams.PlantUml"), diagramEncoder).r,
          OuroborosDsl.Output(w("PlantUml"), last).r,
          OuroborosDsl.Link(w("UserDsl.Config"), w("UserDsl")).r,
          OuroborosDsl.Link(w("UserDsl"), w("TemporalDiagrams.PlantUml")).r,
          OuroborosDsl.Link(w("TemporalDiagrams.PlantUml"), w("PlantUml")).r
        ) ++ mermaid

  val cfgs =
    List[OuroborosDsl.Config](
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", None)
        ),
        showMermaid = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.ConfigToBusinessDsl.some)
        ),
        showMermaid = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.Diagram.some)
        ),
        showMermaid = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.Highlight.some)
        ),
        showMermaid = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", None),
          OuroborosDsl.Config.Variant("variant", None)
        ),
        showMermaid = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.ConfigToBusinessDsl.some),
          OuroborosDsl.Config.Variant("variant", OuroborosDsl.Config.Encoder.ConfigToBusinessDsl.some)
        ),
        showMermaid = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.Diagram.some),
          OuroborosDsl.Config.Variant("variant", OuroborosDsl.Config.Encoder.Diagram.some)
        ),
        showMermaid = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.Highlight.some),
          OuroborosDsl.Config.Variant("variant", OuroborosDsl.Config.Encoder.Highlight.some)
        ),
        showMermaid = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", None),
          OuroborosDsl.Config.Variant("variant", None)
        ),
        showMermaid = true
      )
    )

class WriteOuroborosDiagram[F[_]: Applicative](out: FilePrinter[F]):
  def run: F[Unit] =
    WriteOuroborosDiagram
      .cfgs
      .zipWithIndex
      .traverse:
        case (cfg, n) =>
          val renders: Chain[Renderable[Chain[PlantUml]]] =
            cfg
              .variants
              .toChain
              .flatMap: v =>
                WriteOuroborosDiagram
                  .variant(cfg.showMermaid)
                  .run(v)
                  .extract

          printNormalDiagram(renders, n) *> printHighlightDiagrams(renders, n)
      .void

  private def printNormalDiagram(renders: Chain[Renderable[Chain[PlantUml]]], n: Int) =
    val str =
      renders
        .pipe(Renderable.renderMany[Chain[PlantUml]])
        .pipe(PlantUml.renderHorizontally)
        .mkString_("\n")

    out.print(s"ouroboros-$n.puml")(str)

  private def printHighlightDiagrams(renders: Chain[Renderable[Chain[PlantUml]]], n: Int) =
    val tags =
      Renderable
        .allTags(renders)
        .toList

    tags.traverse_ { t =>
      val str =
        renders
          .pipe(Renderable.renderManyWithTag[Chain[PlantUml]](_, t))
          .pipe(PlantUml.renderHorizontally)
          .mkString_("\n")

      out.print(s"ouroboros-$n-$t.puml")(str)
    }
