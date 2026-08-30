# canhrQsort

**Use it here: [rdazadda-canhrqsort.share.connect.posit.cloud](https://rdazadda-canhrqsort.share.connect.posit.cloud)**

A dashboard for Q-sort analysis, built at the Center for Alaska
Native Health Research (CANHR), University of Alaska Fairbanks. Upload a
study's
sorts, run the frequentist or the Bayesian analysis, read the results on
screen, and download the figures and the full results as Excel files.

## Run it locally

From a local copy of this repository:

```r
install.packages(".", repos = NULL, type = "source")
library(canhrQsort)
run_qsort_app()
```

Running locally needs R 4.1 or newer and the
[bayesqm](https://rdazadda.github.io/bayesqm/) package, the Bayesian
engine behind the dashboard.

## The pages

**Overview.** Upload a CSV or Excel file, or load a bundled sample study.
The page validates the sorts, shows the forced distribution, and presents
the data as a table or as one Q-sort pyramid per participant.

**Frequentist.** Two approaches on one page. Priorities ranks statements
by mean placement; add demographics and it compares subgroups with the
appropriate tests and draws how the top statements shift between groups.
Factors is the classical analysis in four steps: extract (retention
evidence and a scree plot against parallel analysis), flag (rotated
loadings, bipolar handling, manual rotation), interpret (factor arrays,
distinguishing and consensus statements, crib sheets), and export
(bootstrap stability and the results file).

**Bayesian.** Fits the bayesqm model to the same data. The page suggests
the number of factors or takes your choice, reports whether sampling
converged, and gives credible intervals for loadings, flag probabilities,
posterior factor arrays, and distinguishing statements at a chosen error
level.

**Visualization.** All figures from both analyses on one page. Each
figure has its own export button, and one button downloads the full set.

## Data formats

CSV and Excel, plus the common Q platforms: PQMethod, HTMLQ, FlashQ,
Ken-Q Analysis, and KADE exports. Statement texts and participant IDs are
read from the file where present.

## Downloads

Each analysis exports its complete results as an Excel file, one sheet
per table. Every figure saves as a PNG, one at a time or all together
as a ZIP.

## Methods

The frequentist pipeline follows the standard Q literature (Brown 1980;
Watts and Stenner 2012) and matches qmethod's numbers where they overlap.
Bootstrap intervals follow Zabala and Pascual (2016). The Bayesian model
is documented with the bayesqm package.

## License

MIT. See [LICENSE.md](LICENSE.md).
