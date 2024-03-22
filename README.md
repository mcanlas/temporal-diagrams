# temporal-diagrams
A Scala DSL for generating diagram variants

## Features (v2)

- Supports PlantUML
- Supports Mermaid
- Uses `cats` style encoding, derivation, and combinators

## Viewing PlantUML

### With Google Chrome

Install the extension [PlantUML Viewer](https://chrome.google.com/webstore/detail/plantuml-viewer/legbfeljfbjgfifnkmpoajgpgejojooj?hl=en)

When viewing on GitHub, be sure to access the "raw" version of the file (as plain text, outside of the GitHub UI)

### With IntelliJ

Install the plugin [PlantUML Integration](https://plugins.jetbrains.com/plugin/7017-plantuml-integration)

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

- [Mermaid](http://mermaid.js.org/)
- [PlantUML](https://plantuml.com/)
- https://crashedmind.github.io/PlantUMLHitchhikersGuide/layout/layout.html
