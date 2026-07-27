# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Environment

This project runs inside a Dev Container using the image `ghcr.io/tarensanders/r-base:latest`. Open in VS Code with the Dev Containers extension, or via GitHub Codespaces. Data is bind-mounted at `/data` from a host path configured in `devcontainer.json`.

## Package Management

Packages are declared in `devcontainer.json` under the `r-packages` feature (`"packages": "pkg1,pkg2,..."`). Rebuild the container to pick up new packages. At project completion, run `capsule::create()` to snapshot the lockfile for reproducibility — no `capsule::install()` step is needed during active development.

The `renv/` directory is gitignored; `renv.lock` is tracked.

## Workflow

Pipelines are built with [`targets`](https://docs.ropensci.org/targets/). The `_targets/` store is gitignored. The pipeline is defined in `_targets.R`; helper functions live in `R/` and are sourced automatically.

```r
targets::tar_make()          # run the full pipeline
targets::tar_make("name")    # run a single target
targets::tar_visnetwork()    # visualise the dependency graph
targets::tar_read("name")    # inspect a target's output
```

## Formatting

Formatting uses [Air](https://github.com/posit-dev/air). Air is pre-installed in the devcontainer; format-on-save is enabled automatically. Config lives in `air.toml`.

```sh
air format .          # format all R files
air format --check .  # check without modifying (used in CI)
```

## Project Conventions

- `data/` and `temp/` are gitignored — keep raw and intermediate data out of git.
- `.Renviron` is gitignored — store secrets and environment-specific paths there.
- `scratch.R` is gitignored — use it freely for throwaway exploration.
- The pipeline is defined in `_targets.R`; analysis helpers live in `R/`.
- `air.toml` sets project-wide formatting rules.
