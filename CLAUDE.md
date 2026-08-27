# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project

Analysis of the relationship between video gaming and self-concept development and if parental relationship acts as a moderator in the relationship using data from the Longitudinal Study of Australian Children (LSAC). This is a reproducible research pipeline, not a package or app — the deliverable is the analysis, produced by running the `targets` pipeline.

## Architecture

The whole project is a [`targets`](https://docs.ropensci.org/targets/) pipeline:

- `_targets.R` — pipeline definition. The final `list(...)` enumerates targets; `tar_source()` auto-sources everything in `R/`; `tar_option_set()` configures packages and a `crew` local controller for parallel execution across workers.
- `R/` — helper functions only. Do not put pipeline steps here; define them as `tar_target()`s in `_targets.R` and keep the logic in functions under `R/`. Every `.R` file in `R/` is sourced automatically. Prefer one function per file, named after the function, with reasonable exceptions where it makes sense to group helpers together or with the main function.
- `_targets/` (gitignored) — the pipeline's cache/store. `targets` skips up-to-date targets by comparing dependencies, so rerunning only recomputes what changed.

Data flow: raw LSAC data is bind-mounted read-only at `/data` (host path set in `.devcontainer/devcontainer.json`). Intermediate files go in `temp/`. Both `data/` and `temp/` are gitignored.

Use similar projects from the same developer as a reference for how to structure the pipeline. For example, these are some projects which have used LSAC data and `targets` pipelines:

- [bodyimage_parenting](https://github.com/Motivation-and-Behaviour/bodyimage_parenting/tree/initial-analysis)
- [screentime/biomarkers](https://github.com/Motivation-and-Behaviour/screentime_biomarkers/tree/ijbnpa_revisions)
- [scrolling_through_emotions](https://github.com/Motivation-and-Behaviour/scrolling_through_emotions/tree/initial_analysis)
- [sport_autonomy](https://github.com/Motivation-and-Behaviour/sport_autonomy/tree/initial_analysis)

## Analytical decisions

Analytical decisions are the user's to make. When an analytical choice is unclear or has defensible alternatives (e.g., inclusion criteria, how to handle outliers or missing data, model specification, how to operationalise a moderator), ask rather than deciding unilaterally — make a recommendation with reasoning, but let the user decide. Record decisions in ANALYSIS_PLAN.md (and as comments in `_targets.R` where they affect code).

## Reporting

All analysis outputs (descriptives tables, plots, model results) go into a Quarto report at `doc/report.qmd`, rendered to docx via a `tar_quarto()` target — the same way bodyimage_parenting does it (use [its report](https://github.com/Motivation-and-Behaviour/bodyimage_parenting/blob/initial-analysis/doc/report.qmd) as the structural example: docx format with TOC and numbered sections, `echo: false`, `tar_read(..., store = here::here("_targets"))`, plain-language Methods bullets, interpretation callouts after each result, supplementary material in appendix sections referenced with `@sec-supp-*`). The report is built iteratively: each analysis step adds its outputs to the report as it is completed, rather than writing the report at the end — the rendered docx is the artifact reviewed at each step's checkpoint. Don't limit line length by characters - instead, each sentence should be a new line for easy diffing.

## Commands

Run inside R (from the project root):

```r
targets::tar_make()          # run the full pipeline
targets::tar_make("name")    # run/refresh a single target
targets::tar_visnetwork()    # visualise the dependency graph
targets::tar_read("name")    # inspect a target's cached output
targets::tar_outdated()      # list targets that would rerun
```

Formatting (from a shell) — [Air](https://github.com/posit-dev/air), config in `air.toml` (2-space indent, 80-col):

```sh
air format .          # format all R files
air format --check .  # check only; this is what CI enforces
```

CI (`.github/workflows/ci.yml`) runs `air format --check .` only — there is no test suite. Formatting must pass or the build fails.

## Environment & packages

Development happens in a Dev Container (`ghcr.io/tarensanders/r-base:latest`), via VS Code Dev Containers or Codespaces. R packages are added by uncommenting and editing the `r-packages` feature in `.devcontainer/devcontainer.json` (comma-separated), then rebuilding the container — not via `install.packages()`. Packages a pipeline uses must also be listed in the `packages =` argument of `tar_option_set()` in `_targets.R`.

`renv/` is gitignored; `renv.lock` is tracked and generated at project end with `capsule::create()`.
