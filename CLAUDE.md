# CLAUDE

Read @README.md for repo overview.

## Research overview

This scientific research project implements a Gaussian Mixture Neural Network 
(GMNN) called NetC14 for calibrating and summarizing radiocarbon dates. We use
simulations based on the generative model described in (Heaton 2021) to evaluate 
the ability of NetC14 to reconstruct the true probability density (summarizing
radiocarbon dates) and to estimate true ages (calibrating radiocarbon dates). 
Comparisons are also made to other algorithms, including 

-   Summed Probability Density (SPD), 
-   Composite Kernel Density Estimation (CKDE), 
-   Bayesian Gaussian Mixture Model (BGMM), and 
-   Dirichlet Process Mixture Model (DPMM).

Finally, models are compared against real world data, where the true density and 
ages are unknown.

## Structure

-   `_extensions/` - a Quarto extension for the manuscript
-   `.misc/` - random stuff not important enough to hold in a main directory
-   `.quarto/` - the mess of temp files that Quarto generates
-   `data/` - all data required for or derived from the analysis
-   `figures/` - all generated figures
-   `manuscript/` - files related to compiling `manuscript.qmd`
-   `R/` - all R scripts and Quarto documents required for compiling 
    `01-analysis.qmd`, `02-tuning.qmd`, and `03-timing.qmd`.

## Project requirements

-   A key constraint on the project is to minimize the number of R package 
    dependencies, relying as far as possible on software that ships with the R 
    installation. 
-   The project relies heavily on literate programming with Quarto.
-   We use the R package `torch` to implement the BGMM and NetC14.
-   All figures should be saved as SVG using the `svglite` package.
-   Complete simulation tests and evaluations conducted in `R/01-analysis.qmd`.

## Important rules for Claude

-   **YOU MUST NEVER EDIT FILES IN THE manuscript/ FOLDER.** Your primary role 
    is to help me write R code for testing radiocarbon models and visualizing 
    results.
-   **DO NOT VALIDATE CODE.** I will do that manually using simulation tests in 
    `01-analysis.qmd`.
