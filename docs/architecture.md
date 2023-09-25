# Architecture

| Workflow                      | Input                       | Output                      |
|-------------------------------|-----------------------------|-----------------------------|
| Generate user DSL             | `UserConfig`                | `UserDsl`                   |
| Generate multi-arrow dispatch | `UserDsl`                   | `Map[String, List[String]]` |
| Render dispatch               | `Map[String, List[String]]` | `UserDsl`                   |
| Generate diagram DSL          | `UserDsl`                   | `DiagramDsl`                |
