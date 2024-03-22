# Mermaid support

[Mermaid](http://mermaid.js.org/) is a DSL for different types of diagrams.

Notably, its rendering engine is based in JavaScript and is supported in GitHub's UI.

## Supported diagram types

- :white_check_mark: [Flowchart](https://mermaid.js.org/syntax/flowchart.html#styling-and-classes)
  - :white_check_mark: Diagram direction 
  - :white_check_mark: Node shapes
  - :white_check_mark: Links between nodes
    - :white_check_mark: Multi-source links
    - :white_check_mark: Chained links
    - :white_check_mark: Arrow styling
  - :white_check_mark: Subgraphs
  - :white_check_mark: Link styling
  - :white_check_mark: Node styling by ID

## Viewing Mermaid

### With GitHub

Since 2022, [GitHub can automatically render Mermaid code blocks](https://github.blog/2022-02-14-include-diagrams-markdown-files-mermaid/).

```mermaid
  graph TD;
      A-->B;
      A-->C;
      B-->D;
      C-->D;
```

### With IntelliJ

Install the plugin [Mermaid](https://plugins.jetbrains.com/plugin/20146-mermaid)

Installing this plugin will enable:

- The rendering of `*.mmd` and `*.mermaid` files
- The rendering of Mermaid diagrams in Markdown documents (with the Markdown plug-in)
