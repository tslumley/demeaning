# Re-create tables 2 and 3 from "De-meaning Simulation Studies" by Lumley et al.

Here, we provide code to reproduce tables 2 and 3 from the manuscript. These present results from synthetic datasets. We use code to generate data and run estimators from this [GitHub repository](https://github.com/pamelashaw/missing-confounders-methods). Please refer to the documentation [here](https://github.com/PamelaShaw/Missing-Confounders-Methods/tree/main/sims/README.md) for further information about these functions. We have copied functions from the repository into the current repository to facilitate easier reproduction.  

## Generating data
The first step is to create the datasets. We create 2500 independent datasets of size 12,000 and 17,000 by running the following code in a Windows command prompt (`cmd`) terminal; if you use a different terminal (e.g., bash), you can create a similar script to run in your environment. We generate the data using these calls:

`{cmd} gen_one_y_m_scenario_n.bat 1.15 3.1 12000`
`{cmd} gen_one_y_m_scenario_n.bat 1.15 3.1 17000`

This code further calls `04_main.R`, `00_utils.R`, `01_generate_data.R`, `03_estimate.R`, and `02_methods*.R`.

## Running estimators

The second step is to run the estimators. We will run inverse probability weighting (`ipw`), generalized raking (`gr`), multiple imputation via chained equations (`mice`), and targeted maximum likelihood estimation (`tmle`). We have a separate call for each sample size:

`{cmd} run_one_y_m_scenario_n_some_ests.bat 1.15 3.1 12000 gr ipw mice tmle-mto`
`{cmd} run_one_y_m_scenario_n_some_ests.bat 1.15 3.1 17000 gr ipw mice tmle-mto`

## Creating the tables

Finally, we create the tables. This is accomplished using `create_synthetic_results_table.R`, which uses the code in `00_utils.R` and `winsorize.R`. The code can be run either interactively (e.g., in RStudio) or using `Rscript`.
