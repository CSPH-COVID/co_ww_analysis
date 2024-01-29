# Colorado Wastewater Analysis

This project contains the code to download publicly available wastewater data from Colorado utilities, estimate a state space model, and characterize the trend.

Note: this branch requires use of restricted access data in the following files:

- `1_SARS-CoV-2_Wastewater_Data_ 2023-04-24 .csv`

- `SARS-CoV-2_Dashboard_Data_ 2023-04-24.csv`
------------------------------------------------------------------------

## Directory Structure

The script `project_init.R` loads packages and functions used throughout the analysis. This script should be run at the beginning of each session.

The project is organized into several subdirectories:

-   `functions` contains scripts with functions written to process and analyze the data

-   `code` contains scripts to download and analyze the wastewater data

-   `input` contains static data that cannot be downloaded from an external source

-   `cache` is where downloaded wastewater datasets and forecasts are stored

-   `outputs` contains all figures and tables generated from the scripts

------------------------------------------------------------------------

## Problem statement

Measurement of wastewater concentration of SARS-CoV-2 are highly variable across both time and space. Wastewater concentrations are generally measured every 3-4 days around municipalities in Colorado. Reasons for variation in the wastewater concentration measurements include:

-   Changes in sampling methods across time. While sampling machines are often used, so-called grab samples are collected when the automated samplers fail.

-   Flow rates may vary for any number of reasons. These flow rates may influence how well mixed samples are.

-   Wastewater infrastructure may vary across municipalities.

## Objective

The objective of the project is to develop a method to distinguish a trend in the wastewater concentration measurements from the noise in the data.

## Two-Step Approach

We develop a two-step trend classification method that first uses a state space regression model to distill the signal from the noise in the data, then classify the trend using linear regression as recommended by the CDC. The following section describes the two steps of the model in further detail.

### Step 1: distill signal from noise

The state space model is designed to model a time series that is governed by an dynamic process that may not be perfectly observable over time. In our case, the underlying dynamic process is the prevalence of SARS-CoV-2 in the population. However, we only observe a noisy measure of this prevalence via wastewater concentration. The model estimates parameters that describe the evolution of the unobserved prevalence over time. We then use the model to generate the unobserved prevalence series effectively distilling the prevalence signal from the noise in the wastewater concentration data. We implement the model using the Bayesian Structural Time Series package [bsts](https://cran.r-project.org/web/packages/bsts/bsts.pdf).

### Step 2: classifying the trend

The second step is classifying the trend of the estimated prevalence signal. We follow the CDC recommendation and estimate a linear regression line using all observations that fall within the past 14 days of each observation. Specifically, for each utility, we estimate the following regression on date $t$,

$$
trend_{\tau} = \beta time_{\tau} + \epsilon_{\tau},
$$

where $\tau \in [t-14,t]$. For example, the trend on October 14, 2022 is classified based on all measurements recorded between October 1 and 14. The coefficient on $time$, $\beta$, represents the slope of the time trend. On each day, we test the null hypothesis that $\beta = 0$.  If we reject the null hypothesis, we consider the trend increasing ($\beta>0$) or decreasing ($\beta<0$).  If we fail to reject the null hypothesis, we cannot classify the trend confidently and consider the trend plateau. 


## Thoughts

- Transfer priors when utilities don't have many observations (new or large gaps) or when lab methods change