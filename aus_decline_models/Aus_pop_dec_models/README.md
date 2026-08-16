# AusDecline v5: integrated master workflow

This project fits and compares three `nimbleCarbon` temporal models using the same Australian radiocarbon dataset:

1. single proportional slope;
2. abrupt change point with early and late slopes;
3. smooth logistic transition.

## Fastest route: one master command

Open `run_master_report.R` in RStudio and click **Source**, or run:

```r
source("run_master_report.R")
```

The report is written to:

```text
output/master_report/AusDecline_master_report.html
```

It contains the executive summary, complete methods, global settings, data audit, calibration and binning description, three model definitions, priors, MCMC details, WAIC comparison, convergence diagnostics, posterior curves, plain-English results, archaeological interpretation, limitations and reproducibility record.

## Render the complete suite

```r
source("run_everything.R")
```

This creates the master report plus the three standalone model reports.

## Standalone modular reports

```r
source("run_single_slope.R")
source("run_change_point.R")
source("run_logistic_transition.R")
```

Each standalone report explains one model in plain English and technical detail. It does not declare that model superior, because model preference requires fitting competing models to the same data.

## Global controls

Edit `config.yml` to change:

- analysis window;
- cal AD or cal BP display;
- calibration curve and normalisation;
- site-bin width;
- test, medium or long run mode;
- MCMC chains, iterations, burn-in and thinning;
- processor count;
- enabled models;
- figure output settings.

Start with `run_mode: "test"`. Use `medium` after the complete workflow succeeds and `long` only for final publication analyses.

## Important interpretation

The workflow compares temporal shapes in the archaeological radiocarbon record. It does not directly estimate a census population or prove that European colonisation caused an inferred decline. Historical interpretation requires sensitivity analyses and independent evidence.


## Version 5.1 fix
The master report prior expression is rendered as mathematics rather than being misread as inline R code.
