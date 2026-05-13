# funcml Tutorial

This repository contains a self-contained Quarto tutorial for
[`funcml`](https://github.com/ielbadisy/funcml), a formula-first machine
learning framework for R.

Read the rendered tutorial online:
<https://ielbadisy.github.io/funcml-tutorial/>

The tutorial introduces the package's six main workflow verbs:

- `fit()` for model fitting
- `evaluate()` for resampling-based assessment
- `tune()` for hyperparameter search
- `compare_learners()` for learner benchmarking
- `interpret()` for model-agnostic interpretation
- `estimate()` for plug-in g-computation

It also covers the learner registry, performance metrics, interpretation
methods, causal estimands, and a complete end-to-end workflow using the Pima
diabetes dataset.

## Repository Contents

- `funcml-tutorial.qmd`: Quarto source for the tutorial.
- `funcml-tutorial.html`: Rendered HTML version of the tutorial.
- `index.html`: GitHub Pages entry point that opens the rendered tutorial.
- `references.bib`: Bibliography used by the Quarto document.

## Requirements

To render the tutorial locally, install:

- R
- Quarto
- the `funcml` package
- the R packages used by the tutorial examples

Install `funcml` from GitHub:

```r
remotes::install_github("ielbadisy/funcml")
```

The document also uses `ggplot2` directly and exercises many optional learner
backends through `funcml`, including tree, ensemble, kernel, neural network,
Bayesian, and interpretation packages.

## Render

Render the HTML document with:

```bash
quarto render funcml-tutorial.qmd --to html
```

Render the PDF document with:

```bash
quarto render funcml-tutorial.qmd --to pdf
```

The checked-in HTML file is generated from the Quarto source and embeds its
resources for easier sharing.

## Tutorial Outline

The tutorial is organized around the full applied machine learning workflow:

1. Installation and example datasets
2. Resampling strategies
3. Learner registry and model families
4. Performance metrics
5. Evaluation, tuning, and learner comparison
6. Model-agnostic interpretation
7. Plug-in g-computation
8. End-to-end analysis

## License

The tutorial accompanies the GPL-3 licensed `funcml` package. See the upstream
package repository for package licensing and source code details.
