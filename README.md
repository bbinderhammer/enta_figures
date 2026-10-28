# ENTA figures — R scripts

This repository contains R scripts for generating figures from the published ENTA
(European National Transfer Accounts) data.

## Data

The scripts download the ENTA data from Zenodo on first run:

- Concept DOI: https://doi.org/10.5281/zenodo.20360412
- Record: https://zenodo.org/records/22550976

No microdata or local datasets are required.

## Scripts

| Script | Description |
|---|---|
| `ENTA_labourinc_consumption_by_age.R` | Labour income (YL) and consumption (C) by age, with annotated surplus/deficit periods |
| `ENTA_gov_expenditure_by_function_and_age.R` | Social benefits received per capita by age, stacked by type (pensions, health, education, family, unemployment, other), with taxes and social contributions paid as a line |
| `ENTA_gov_expenditure_by_function_aggr.R` | Public expenditure on pensions, healthcare, education, family and unemployment by year (1995-latest), in real euro per capita, percent of GDP or billion euro; same colours as the plot by age |

More scripts will be added for other indicators. Each script includes its own
documentation in the header comments.

## Requirements

- R >= 4.0
- RStudio (optional but recommended)

The scripts install missing packages automatically on first run.

## How to run

1. Download or clone this repository.
2. Open `enta_figures.Rproj` in RStudio. This sets the working directory to
   the repository folder, so no path needs to be set.
3. Open a script, change the settings at the top (country, year, ...) and
   click **Source**.

```
enta_figures/
  enta_figures.Rproj   open this first
  data/                the ENTA data are downloaded here on the first run
  figures/             the graphs are saved here as PNG, named like the
                       script without "by"/"and", plus the settings, e.g.
                       ENTA_gov_expenditure_function_age_AT_2023.png
  ENTA_*.R             the scripts
```

The graph is saved only if the folder `figures/` exists in the working
directory; otherwise it is only shown. To save it elsewhere, set `outdir` at
the top of the script to another existing folder. Downloaded data and
figures are not tracked by git (see `.gitignore`).

## License

This code is released under the MIT License (see LICENSE).

The ENTA data is licensed under CC BY 4.0 (see Zenodo record 22550976).
