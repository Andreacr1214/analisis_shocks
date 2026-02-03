# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**shockcomercial** is an R package containing a Shiny web application for analyzing commercial shock vulnerabilities in Spain's import supply chains. Users select a product (TARIC code), simulate a trade disruption from a specific provider country, and explore substitution scenarios.

The application is written entirely in Spanish (UI labels, variable names, comments).

## Build & Development Commands

```bash
# Install package dependencies
Rscript -e 'devtools::install_deps()'

# Build the package
R CMD build .

# Check the package (lint + tests)
R CMD check .

# Load package in development mode (from R console)
devtools::load_all()

# Run the Shiny app (from R console, after load_all)
vulnerabilidad_comercial_app()

# Regenerate NAMESPACE and docs from roxygen2 comments
devtools::document()
```

The app runs on port 3838 by default (configurable via `PORT` environment variable).

## Architecture

### Module Pipeline

The app follows Shiny's modular pattern with a strict data-flow pipeline:

```
contexto_producto → configuracion_shock → sustitucion_proveedores
```

Each module returns reactive data consumed by the next:

1. **contexto** (`R/contexto.R`) — User selects a TARIC product code. Queries PostgreSQL for import data (2019–present), computes 12-month rolling totals, market shares, HHI, and year-over-year variation by provider country. Returns: `producto`, `nivel`, `datos`, `periodo`, `total_general`.

2. **shock_config** (`R/shock_config.R`) — User picks a provider country and shock magnitude (0–100% reduction). Calculates pre/post HHI, absolute loss, and new market leader. Returns: `shock_configurado()`.

3. **sustitucion** (`R/sustitucion.R`) — Most complex module (~690 lines). Implements 5 redistribution scenarios: None, Proportional, Capacity-limited (iterative algorithm), Friendly (EU/OECD filter), and Proximity (geographic + commercial scoring). Optionally queries the UN Comtrade API for export analysis of top alternative providers.

4. **impacto** (`R/impacto.R`) — Direct impact display. Currently a placeholder/stub awaiting integration with shock data.

### Main Entry Point

`R/app.R` defines `vulnerabilidad_comercial_app()` which builds the `bslib::page_navbar` UI, creates the PostgreSQL connection via `comerciotools::crear_conexion_pg()`, and wires the three modules together with `tryCatch` error handling.

## External Dependencies

- **PostgreSQL** with `datacomex` schema — all trade data is queried via the `comerciotools` custom package (`crear_conexion_pg()`, `cargar_pg_datacomex()`)
- **UN Comtrade API** — used in the substitution module's export analysis feature; requires `COMTRADE_TOKEN` environment variable
- **comerciotools** — custom (non-CRAN) R package for Spanish trade data access

## Key Metrics

- **HHI (Herfindahl-Hirschman Index):** `sum(market_share_pct^2)`, range 0–10,000, measures market concentration
- **Shock impact:** `valor_perdido = valor_12m * (magnitud / 100)`
- **Capacity-limited substitution:** iterative distribution (max 20 rounds) where each country's absorption is capped at `current_value * limit_pct`

## Code Conventions

- 2-space indentation (configured in `.Rproj`)
- Spanish naming throughout: variables (`valor_12m`, `cuota_pct`), functions (`calcular_proximidad`), UI text
- Roxygen2 for documentation (RoxygenNote 7.2.3)
- Shiny module pattern: each module exports `*_ui(id)` and `*_server(id, ...)` functions
- Country approach tab ("Enfoque: País") is planned but not yet implemented
