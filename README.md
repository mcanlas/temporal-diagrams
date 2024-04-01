# temporal-diagrams
A Scala DSL for generating diagram variants

## In action

Easily generate diagrams

```mermaid
---
title: Component diagram
---
flowchart
  reader-service:::Service

  writer-lambda:::Lambda

  classDef Lambda fill:#bc4f4f

  classDef Service fill:#586ba4

  subgraph persistence [Persistence]
    database[(database)]:::Database

    classDef Service fill:#586ba4
  end

  reader-service --> database

  writer-lambda -- writes to --> database
```

Or variants of them

```mermaid
---
title: Component diagram
---
flowchart
  reader-service:::Service

  writer-lambda:::Lambda

  classDef Lambda fill:#bc4f4f

  classDef Service fill:#586ba4

  subgraph persistence [Persistence]
    database[(database)]:::Database

    replica-1[(replica-1)]:::Database

    replica-2[(replica-2)]:::Database

    classDef Service fill:#586ba4
  end

  reader-service --> replica-1

  reader-service --> replica-2

  writer-lambda -- writes to --> database
```

Even to different diagram languages

![](docs/demo-plantuml.png)

With complex targeting

![](docs/demo-plantuml-multiarrows.png)

Or highlighting certain areas

![](docs/demo-plantuml-highlighting.png)

## Features

- Encode to one or more target diagram languages
- Write in type safe DSLs for target diagram languages
- Mix multiple domain languages in one diagram
- Highlight certain areas of a diagram with highlighting

## Supported target languages

- :white_check_mark: [Mermaid](docs/mermaid.md)
- :white_check_mark: [PlantUML](docs/plantuml.md)

## See also

- [Architecture](/docs/architecture.md)

## Unmanaged JAR

Copy the URL from [the PlantUML download page](https://plantuml.com/download)

```bash
(cd YOUR_SBT_SUB_PROJECT/lib && curl -LO https://github.com/plantuml/plantuml/releases/download/v1.2023.13/plantuml-mit-1.2023.13.jar)
```

`-L` means follow redirects and `-O` means save the payload

## Generating very large diagrams

[Inject the override via environment variable](https://plantuml.com/faq)

```
PLANTUML_LIMIT_SIZE=8192 sbt run
```

## Elsewhere

- https://crashedmind.github.io/PlantUMLHitchhikersGuide/layout/layout.html
