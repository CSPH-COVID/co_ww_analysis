# Colorado Wastewater Analysis

This repository contains the code to reproduce the analysis and figures in the manuscript 

Note: this branch requires use of restricted access data. Please contact the authors for access to the following file:

- `1_SARS-CoV-2_Wastewater_Data_2023-04-24.csv`

------------------------------------------------------------------------

## Directory Structure

The project is organized into several subdirectories:

-   `functions` contains scripts with functions written to process and analyze the data

-   `code` contains scripts to download and analyze the wastewater data

-   `input` contains static data that cannot be downloaded from an external source

-   `cache` is where downloaded wastewater datasets and forecasts are stored

-   `outputs` contains all figures and tables generated from the scripts

------------------------------------------------------------------------

## Instructions

1. Run the script `project_init.R` in the root directory to install packages and set up folders.

2. Place the file `1_SARS-CoV-2_Wastewater_Data_2023-04-24.csv` into the input directory. 

3. Run the scripts in the code directory in numeric order.


## Thoughts

- Transfer priors when utilities don't have many observations (new or large gaps) or when lab methods change