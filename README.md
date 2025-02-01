# Bayesian Statistics Final Project: Fish Market Data Analysis

**Authors**: Jayden Cruz & Christopher Garcia  
**Course**: Bayesian Statistics  
**Grade**: 95 (A) - Passed the course with an A  
**Project Type**: Final Project   

## Overview

In this project, we analyze the Fish Market Data set, which provides detailed information about various fish species and their physical characteristics. The main goal is to predict the **Weight** of fish based on other continuous variables such as **Length2** and **Height** using Bayesian methods. We explore different models using **Directed Acyclic Graphs (DAGs)**, **Quadratic Approximation (quap)**, and **Markov Chain Monte Carlo (MCMC)** to assess the relationships between predictors and the outcome.

## Dataset Information

The **Fish Market Dataset** includes the following variables:

- **Species**: The species of the fish (categorical).
- **Weight**: The weight of the fish in grams (continuous).
- **Length1** (Vertical), **Length2** (Diagonal), **Length3** (Cross-Length): Various measurements of length in centimeters (continuous).
- **Height**: The height of the fish as a percentage of its cross-sectional length (continuous).
- **Width**: The width of the fish as a percentage of its cross-sectional length (continuous).

Dataset source: [Fish Market Dataset on Kaggle](https://www.kaggle.com/datasets/vipullrathod/fish-market/data).

## Goals and Approach

The project aims to:

1. **Develop DAG models**:
   - Model Weight using Length2 (X1).
   - Model Weight using Height (X2).
   - Model Weight using both Length2 and Height.

2. **Use Quadratic Approximation (quap)**:
   - Create models using informative priors based on the dataset’s characteristics.
   - Simulate priors to assess their suitability.

3. **Run MCMC simulations**:
   - Analyze the models using MCMC techniques to assess convergence and robustness.
   - Compare models using **WAIC** (Widely Applicable Information Criterion).

4. **Visualize and Compare Models**:
   - Visualize predictions using counterfactual plots.
   - Compare the models' predictive accuracy.

### Loading the Dataset

```r
library(rethinking)
data <- read.csv("Fish.csv")
d <- data
d <- na.omit(d)
