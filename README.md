# PURPOSe: Predicting Utilization of Resources in Psychiatry Outpatient Services

## Project Overview
This project is starting as my final project for BMIN503/EPID600 course in my Master of Biomedical Informatics (MBMI), focuses on optimizing resource utilization in the Outpatient Psychiatry Clinic at Penn Medicine. By leveraging a decade's worth of clinic data, this project develops predictive models that aim to enhance scheduling efficiency and care allocation, ultimately improving patient outcomes and provider satisfaction. The analysis integrates datasets from Epic Analytics, implementing advanced data preprocessing, feature engineering, and modeling techniques.

## Repository Structure

- `datasets/`: Directory containing anonymized datasets used in the analysis, along processed intermediate datasets.
- `scripts/`: R scripts and helper functions used for data preprocessing, analysis, and model building.
- `images/`: Graphical resources used in the documentation and reports.
- `outputs/`: Outputs from the scripts including processed data, tables, and figures.
- `README.md`: This file, providing an overview and instructions on how to navigate the repository.

## Installation

To run the scripts and analyses contained in this repository, you will need R and several R packages installed. You can install the necessary packages using the following R command:

```R

install.packages(c("readr", "dplyr", "lubridate", "stringr", "tibble", "ggplot2", "caret", "pROC", "doParallel", "car", "data.table", "tidyr", "mice", "stringdist", "hms", "moments"))
```

## Usage

To reproduce the analysis:

1. Clone this repository to your local machine.
2. Open the R project file in the root directory.
3. Open .qmd file and run code chunks in order (can be ran independently after data aggregation).

## Contributing

Contributions to this project are welcome. Please refer to CONTRIBUTING.md for detailed information on how to contribute.

## Acknowledgments

- Blanca Himes for being so flexible, supportive and comprehensive in her teaching!

- All contributors who participate in the development and refinement of this project.
