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
  def variant(showMermaid: Boolean, showForeign: Boolean) =
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
              Some("Config => YourDsl") -> Some("Config => YourDsl") -> None -> None

            case Some(OuroborosDsl.Config.Encoder.Highlight) =>
              None -> Some("HighlightEncoder[YourDsl, PlantUml]") -> Some(
                "HighlightEncoder[YourDsl, PlantUml]"
              ) -> None

            case Some(OuroborosDsl.Config.Encoder.Diagram) =>
              None -> None -> Some("DiagramEncoder[PlantUml]") -> Some("DiagramEncoder[PlantUml]")

        val mermaid =
          if showMermaid then
            Chain[Renderable[Chain[PlantUml]]](
              OuroborosDsl.Type(w("TemporalDiagrams.Mermaid"), diagramEncoder).tag("mermaid"),
              OuroborosDsl.Output(w("Mermaid"), None).tag("mermaid"),
              OuroborosDsl.Link(w("YourDsl"), w("TemporalDiagrams.Mermaid")).tag("mermaid"),
              OuroborosDsl.Link(w("TemporalDiagrams.Mermaid"), w("Mermaid")).tag("mermaid")
            )
          else Chain.empty

        val foreign =
          if showForeign then
            Chain[Renderable[Chain[PlantUml]]](
              OuroborosDsl
                .Type(w("ForeignDsl"), highlightEncoder)
                .tag("foreign", "one-render"),
              OuroborosDsl.Link(w("ForeignDsl"), w("TemporalDiagrams.PlantUml")).r
            )
          else Chain.empty

        Chain[Renderable[Chain[PlantUml]]](
          OuroborosDsl
            .Type(w("YourDsl.Config"), configWrap)
            .tag("mermaid", "function"),
          OuroborosDsl
            .Type(w("YourDsl"), highlightEncoder)
            .tag("mermaid", "function", "highlight", "one-render"),
          OuroborosDsl
            .Type(w("TemporalDiagrams.PlantUml"), diagramEncoder)
            .tag("highlight", "diagram", "foreign", "one-render"),
          OuroborosDsl
            .Output(w("PlantUml"), last)
            .tag("diagram", "foreign", "one-render"),
          OuroborosDsl.Link(w("YourDsl.Config"), w("YourDsl")).r,
          OuroborosDsl.Link(w("YourDsl"), w("TemporalDiagrams.PlantUml")).r,
          OuroborosDsl.Link(w("TemporalDiagrams.PlantUml"), w("PlantUml")).r
        ) ++ mermaid ++ foreign

  val cfgs =
    List[OuroborosDsl.Config](
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", None)
        ),
        showMermaid = false,
        showForeign = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.ConfigToBusinessDsl.some)
        ),
        showMermaid = false,
        showForeign = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.Diagram.some)
        ),
        showMermaid = false,
        showForeign = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.Highlight.some)
        ),
        showMermaid = false,
        showForeign = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", None),
          OuroborosDsl.Config.Variant("variant", None)
        ),
        showMermaid = false,
        showForeign = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.ConfigToBusinessDsl.some),
          OuroborosDsl.Config.Variant("variant", OuroborosDsl.Config.Encoder.ConfigToBusinessDsl.some)
        ),
        showMermaid = false,
        showForeign = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.Diagram.some),
          OuroborosDsl.Config.Variant("variant", OuroborosDsl.Config.Encoder.Diagram.some)
        ),
        showMermaid = false,
        showForeign = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", OuroborosDsl.Config.Encoder.Highlight.some),
          OuroborosDsl.Config.Variant("variant", OuroborosDsl.Config.Encoder.Highlight.some)
        ),
        showMermaid = false,
        showForeign = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", None),
          OuroborosDsl.Config.Variant("variant", None)
        ),
        showMermaid = true,
        showForeign = false
      ),
      OuroborosDsl.Config(
        NonEmptyChain.of(
          OuroborosDsl.Config.Variant("", None)
        ),
        showMermaid = false,
        showForeign = true
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
                  .variant(cfg.showMermaid, cfg.showForeign)
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
