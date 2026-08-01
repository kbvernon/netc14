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
> NetC14: A Gaussian Mixture Neural Network for Calibrating and Summarizing Radiocarbon Dates. 

**Preprint**: [manuscript.pdf](/manuscript/manuscript.pdf)

**Supplement**:\
[01-analysis.html](https://kbvernon.github.io/netc14/R/01-analysis.html)\
[02-tuning.html](https://kbvernon.github.io/netc14/R/02-tuning.html)\
[03-timing.html](https://kbvernon.github.io/netc14/R/03-timing.html)

## Contents

📂 [\_extensions](/_extensions) has Quarto extension for compiling manuscript\
📂 [data](/data) contains all data required for analysis\
  ⊢ 💾 intcal20.csv\
  ⊢ 💾 radiocarbon.csv\
  ⊢ output records\
📂 [figures](/figures) contains all figures included in the paper\
📂 [manuscript](/manuscript) contains the pre-print\
  ⊢ 📄 [bibliography.bib](/manuscript/bibliography.bib)\
  ⊢ 📄 [manuscript.qmd](/manuscript/manuscript.qmd)\
  ⊢ 📄 [manuscript.pdf](/manuscript/manuscript.pdf)\
📂 [R](/R) code for preparing data and conducting analysis, including\
  ⊢ 📄 [00-download-c14-data.R](/R/00-download-c14-data.R) downloads actual 
radiocarbon samples used in the Heaton and Price papers.\
  ⊢ 📄 [00-download-intcal20.R](/R/00-download-intcal20.R) downloads the IntCal20
radiocarbon calibration curve.\
  ⊢ 📄 [00-evaluation-summary.qmd](/R/00-evaluation-summary.qmd) is for generating the 
markdown list-table reported in the results\
  ⊢ 📄 [01-analysis.qmd](/R/01-analysis.qmd) is the primary analysis\
  ⊢ 📄 [02-tuning.qmd](/R/02-tuning.qmd) is for tuning hyperparameters in netc14\
  ⊢ 📄 [03-timing.qmd](/R/03-timing.qmd) is crude benchmarks and\
  ⊢ Many R scripts for defining R functions used in analysis.

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

Provided you have `data/intcal20.csv` and `data/radiocarbon.csv`, all R 
dependencies can be installed, the entire analysis can be run, and the 
manuscript can be compiled like this:

```r
# install dependencies with renv
install.packages("renv")
renv::restore()

# use this if you do not want to install renv
pkgs <- readLines("dependencies.txt")
install.packages(pkgs)

# compile quarto documents
library(quarto)
quarto_render("R/01-analysis.qmd")
quarto_render("R/02-tuning.qmd")
quarto_render("R/03-timing.qmd")
quarto_render("manuscript/manuscript.qmd")
```

Note: if you are installing the torch package for the first time, you will need 
to call `library(torch)` before rendering the quarto documents. It will walk you
through additional installation requirements. Also, the file numbering is not 
the order of the workflow, but of priority - 01-analysis is the most important 
document for understanding the methods described in the manuscript.

## 🤖 AI statement

Claude Code was used extensively in this project, but not to write the paper. AI 
was used in five main ways: 

1. to develop plotting functions with base R graphics (inspired by the tinyplot 
   package), 
2. to write roxygen documentation for all functions used in analysis or in 
   visualizing results, 
3. to write an adaptive dual averaging Hamiltonian Monte Carlo sampler in a 
   torch module (based on algorithm 5 in Hoffman and Gelman 2014), 
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