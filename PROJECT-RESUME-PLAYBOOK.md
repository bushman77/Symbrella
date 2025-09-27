
# Architecture Diagram (Mermaid) — Fixed for GitHub

> Paste this block directly in your README. It uses Mermaid syntax that GitHub renders reliably (Mermaid 10.x).  
> Changes: 1) `flowchart TD` 2) subgraph IDs without spaces 3) explicit node IDs 4) safe edge labels.

```mermaid
flowchart TD
  UI[Phoenix UI (LiveView)]
  BrainSup[Brain Supervisor]
  CorePipe[Core Pipeline]
  DB[(Postgres)]

  UI -->|HTTP / WebSocket| BrainSup

  subgraph Brain_App [Brain App]
    BrainSup --> LIFG[Brain.LIFG (Stage‑1 WSD)]
    BrainSup --> Cell[Brain.Cell (GenServer)]
    Cell -->|Ecto| DB
  end

  BrainSup -->|API calls| CorePipe

  subgraph Core_App [Core App]
    CorePipe --> Tok[Token / Tokenizer]
    CorePipe --> Sem[SemanticInput]
    CorePipe --> Lex[Lexicon / Senses]
    CorePipe --> Recall[Recall / Plan]
  end
```
