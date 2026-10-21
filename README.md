# ENTA figures — R scripts

This repository contains R scripts for generating figures from the published ENTA
(Network of National Time Use Accounts) data.

## Data

The scripts download the ENTA data from Zenodo on first run:

- Concept DOI: https://doi.org/10.5281/zenodo.20360412
- Record: https://zenodo.org/records/22550976

No microdata, or local datasets are required.

## Scripts

| Script | Description |
|---|---|
| `ENTA_labourinc_consumption_by_age.R` | Labour income (YL) and consumption (C) by age, with annotated surplus/deficit periods |

More scripts will be added for other indicators. Each script includes its own
documentation in the header comments.

## Requirements

- R >= 4.0
- RStudio (optional but recommended)

The scripts install missing packages automatically on first run.

## License

This code is released under the MIT License (see LICENSE).

The ENTA data is licensed under CC BY 4.0 (see Zenodo record 22550976).
