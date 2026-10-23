# netc14

<!-- badges: start -->
[![Static Badge](https://img.shields.io/badge/Quarto-Paper-74AADB?style=social&logo=Quarto)](https://quarto.org)
[![DOI](https://zenodo.org/badge/1319428718.svg)](https://doi.org/10.5281/zenodo.21743118)
[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)
<!-- badges: end -->

This repository contains code and figures for our paper:

> Kenneth B. Vernon
> [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0003-0098-5092),
> Brian F. Codding
> [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0001-7977-8568),
> and Simon Brewer
> [![](https://orcid.org/sites/default/files/images/orcid_16x16.png)](https://orcid.org/0000-0002-6810-1911)
> NetC14: A Gaussian mixture neural network for calibrating and summarizing radiocarbon dates. 

**Preprint**: [manuscript.pdf](/manuscript/manuscript.pdf)

**Supplement**:

-  [analysis.html](/analysis/analysis.html)
-  [tuning.html](/analysis/tuning.html)
-  [timing.html](/analysis/timing.html)

## Contents

-  [\_extensions](/_extensions) Quarto extension for compiling manuscript
-  [analysis](/analysis) code for preparing data and conducting analysis
-  [data](/data) all data required for analysis
-  [figures](/figures) all figures included in the paper
-  [manuscript](/manuscript) the pre-print
-  [R](/R) R functions used in analysis

Root files for project configuration and workflow:  

-  [workflow.R](workflow.R) runs the full analysis and renders manuscript
-  [justfile](justfile) useful recipes for interactive workflow in the terminal
-  [\_quarto.yml](_quarto.yml) Quarto project configuration
-  [panache.toml](panache.toml) panache configuration for formatting markdown
-  [renv.lock](renv.lock) R package versions for `renv::restore()`

## 💾 Data availability

All data for the analysis are fully available for replication. Most of the data 
are simulated, but there are two real datasets required for the analysis:

-  `data/intcal20.csv` stores the radiocarbon calibration curve for the Northern
   Hemisphere.
-  `data/radiocarbon.csv` stores four radiocarbon datasets used as real world
   examples.

Those four actual radiocarbon datasets include

-  **Raths:** dates on medieval Irish farmsteads, originally from (Kerr and 
   McCormick 2014).
-  **Iron:** dates from Iron Age Ireland, originally from (Armit et al 2014).
-  **Paleo:** dates from North America, originally from (Buchanan et al 2008).
-  **Tikal:** dates on the Mayan city of Tikal, originally from the MesoRAD
   dataset (Hoggarth and Ebert 2020).

The first three (Raths, Iron, and Paleo) are used in the Heaton paper, the last
in Price. For reasons of space, only Iron and Paleo currently appear in the 
manuscript.

## 📈 Replicate analysis

First install dependencies:

```r
# install dependencies with renv
install.packages("renv")
renv::restore()

# use this if you do not want to install renv
pkgs <- readLines("dependencies.txt")
install.packages(pkgs)
```

> [!NOTE]
> If you are installing the torch package for the first time, you will need to
> call `library(torch)` before running the workflow. It will walk you through
> additional installation requirements.

Now, provided you have `data/intcal20.csv` and `data/radiocarbon.csv`, the 
entire analysis can be run and the manuscript can be compiled by calling:

```r
source("workflow.R")
```

If you prefer the terminal and have the just command-runner installed, you can 
also run the whole workflow with

```sh
just run workflow.R
```

> [!NOTE]
> `workflow.R` first checks that the default `gamma` used in `netc()` matches
> the best result found in `tuning.qmd`. If they don't, the whole workflow will
> abort with an error requesting that the default be updated in `R/netc14.R`. If 
> you want to re-run the tuning loop, too, use `quarto_render("analysis/tuning.qmd")` 
> in R or `just analyze tuning` in the terminal.

## 🤖 AI statement

Claude Code was used extensively in this project, but not to write the paper. AI 
was used in five main ways: 

1. to develop plotting functions with base R graphics (inspired by the tinyplot 
   package), 
2. to write roxygen documentation for all functions used in analysis or in 
   visualizing results, 
3. to write an adaptive dual averaging Hamiltonian Monte Carlo sampler in 
   TorchScript (based on algorithm 5 in Hoffman and Gelman 2014), 
4. to write print methods for all S3 classes defined in the analysis, and 
5. to make updates to code when one change in a specific file required changes 
   to multiple files. 

We shoveled any AI slop (meaning we simplified unnecessary code complexity and 
removed insanely long and mostly superfluous comments), and because we are 
basically rounding Neptune at this point, we also use Jarl to lint everything, 
applying formatting with Air, all with the goal of making the code easier to 
read. 

```
> jarl check R/
── Summary ──────────────────────────────────────
All checks passed!
```

The context document used to guide Claude's behavior can be found in 
[`CLAUDE.md`](CLAUDE.md).

The core methodological innovations described in the analysis and manuscript are 
the responsibility of the authors.

## License

**Text and figures:** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

**Code:** [MIT](LICENSE.md)

**Data:** [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)