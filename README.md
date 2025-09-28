# black hole (BHAEB)


## Project Goal

This project aims to build a simple **finite-state machine (FSM)** to model the lifecycle of black holes. While the current implementation is basic and conceptual, the long-term vision is to evolve this into a more ambitious and formal language capable of interpreting astrophysical phenomena through the lens of computer science.

## Structure project
```
├── datasets-test # Data science experiments
├── docs # images and notes
├── flake.nix
├── README.md
├── research # haskell FSM
└── stream-producer # golang data science and streaming
```

## Resource
### DataSets
- [Filter Data](https://eventhorizontelescope.org/for-astronomers/data)


## Schema
```mermaid
graph LR
    A[Haskell Parser<br/>Basic Calculations] --> B[Kafka Bus1]
    B --> C[Golang Service<br/>Reads & Assembles<br/>Parquet Files]
    C --> D[Kafka Bus2]
    D --> E[Jupyter Notebooks<br/>Astronomical Work<br/>Images & Simulations]

    subgraph "Data Processing"
        A
        C
    end

    subgraph "Message Buses"
        B
        D
    end

    subgraph "Analysis & Visualization"
        E
        F[Future Extensions<br/>Configurable]
    end

    E -.-> F

    style A fill:#e1f5fe
    style C fill:#e8f5e8
    style B fill:#fff3e0
    style D fill:#fff3e0
    style E fill:#f3e5f5
    style F fill:#f5f5f5
```
