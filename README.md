# Project Name

> One-line description of what this project does.

## Environment

This project runs inside a Dev Container using the image
`ghcr.io/tarensanders/r-base:latest`. Open in VS Code with the Dev Containers
extension, or via GitHub Codespaces.

Raw data is bind-mounted at `/data` from a host path. To change the host path,
edit the `source` value of the data mount in `.devcontainer/devcontainer.json`.

## Package Management

R packages are declared in `.devcontainer/devcontainer.json` under the
`r-packages` feature:

```jsonc
"ghcr.io/rocker-org/devcontainer-features/r-packages:1": {
  "packages": "targets,dplyr,ggplot2" // comma-separated
}
```

Rebuild the container after adding packages. When the project is complete,
snapshot the environment for reproducibility:

```r
capsule::create()  # record current packages to renv.lock
```

The `renv/` directory is gitignored; `renv.lock` is tracked.

## Workflow

Pipelines are built with [`targets`](https://docs.ropensci.org/targets/).
The `_targets/` store is gitignored.

```r
targets::tar_make()          # run the full pipeline
targets::tar_make("name")    # run a single target
targets::tar_visnetwork()    # visualise the dependency graph
targets::tar_read("name")    # inspect a target's output
```

The pipeline is defined in `_targets.R`. Helper functions live in `R/` and are
sourced automatically by the pipeline.

## Formatting

Formatting uses [Air](https://github.com/posit-dev/air). Air is pre-installed
in the devcontainer and format-on-save is enabled automatically. From a terminal:

```sh
air format .          # format all R files
air format --check .  # check without modifying (used in CI)
```

Config lives in `air.toml`.

## Project Structure

```
_targets.R      pipeline definition
R/              helper functions (sourced by _targets.R)
air.toml        Air formatter config
renv.lock       package lockfile (tracked; generated at project end)
data/           raw data — gitignored
temp/           intermediate files — gitignored
```

## Using This Template

This is a GitHub template repository. To start a new project:

1. Click **Use this template** on GitHub.
2. Rename `"your-project-name"` in `.devcontainer/devcontainer.json`.
3. Update the data mount path in `.devcontainer/devcontainer.json` to point at
   your local data directory.
4. Open in VS Code with the Dev Containers extension and rebuild the container.
