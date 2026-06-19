# create table for synthetic-data simulation results

# load required functions and packages -----------------------------------------
library("readr")
library("dplyr")
library("tidyr")
library("knitr")
library("kableExtra")
library("here")

source(here::here("00_utils.R"))
source(here::here("winsorize.R"))

# read in results files --------------------------------------------------------
results_dir <- here::here("..", "results")
output_dir <- here::here("..", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}
# M 3.1, Y 1.15: 
this_mscenario <- "3.1"
this_yscenario <- "1.15"
ns <- c("12000", "17000")
files_pattern <- paste0(".*_n", paste0("(", paste0(ns, collapse = "|"), ")"), ".*.csv")
all_results_files <- list.files(paste0(results_dir, "/m", this_mscenario, "_y", this_yscenario), 
                                pattern = files_pattern)
all_results <- do.call(rbind.data.frame, lapply(as.list(all_results_files), function(x) {
  results <- read_csv(paste0(results_dir, "/m", this_mscenario, "_y", this_yscenario, "/", x))
  # add on NAs in place of null values
  if (nrow(results) != 2500 * 4) {
    all_mcids <- 1:2500
    unique_mcids <- unique(results$mc_id)
    missing_mcids <- setdiff(all_mcids, unique_mcids)
    na_rows <- tibble::tibble(
      "yscenario" = results$yscenario[1], "mscenario" = results$mscenario[1], "xscenario" = results$xscenario[1],
      "seed" = results$seed[1], "n" = results$n[1], "procedure" = results$procedure[1],
      "mc_id" = rep(missing_mcids, each = 4), "estimand" = rep(unique(results$estimand), length(missing_mcids)),
      "est" = NA, "SE" = NA
    )
    results <- bind_rows(results, na_rows) %>% 
      arrange(mc_id)
  }
  results
})) %>% 
  mutate(estimand = case_when(
    estimand == "canonical_parameter" ~ "clogOR",
    estimand == "logOR" ~ "mlogOR",
    estimand == "logRR" ~ "mlogRR",
    estimand == "RD" ~ "mRD",
    estimand == "OR" ~ "mOR",
    estimand == "RR" ~ "mRR",
    estimand == "cOR" ~ "clogOR"
  )) 

all_true_vals <- readRDS(paste0(results_dir, "/", "true_values_oracle.rds")) %>% 
  # filter(yscenario == this_yscenario, mscenario == this_mscenario) %>% 
  filter(yscenario == this_yscenario) %>% 
  group_by(estimand) %>% 
  slice(1) %>% 
  select(-mscenario, -xscenario) %>%  
  mutate(estimand = case_when(
    estimand == "canonical_parameter" ~ "clogOR",
    estimand == "logOR" ~ "mlogOR",
    estimand == "RD" ~ "mRD",
    estimand == "logRR" ~ "mlogRR"
  ))

all_est_procedures <- c("cc_oracle", "cc_population", "cc", "cc_noW", 
                        "IPW", "iptw", "GR_Vanilla", "GR_X", "RRZ",
                        "MICE", "XGB", "RF", 
                        "TMLE", "TMLE-M", "TMLE-MTO", "tmle_m", "tmle_mto",
                        "ipcw-tmle_m", "ipcw-tmle_mto",
                        "ipcw-a-tmle_m", "ipcw-a-tmle_mto", 
                        "r-ipcw-tmle_m", "r-ipcw-tmle_mto",
                        "r-ipcw-a-tmle_m", "r-ipcw-a-tmle_mto")
all_est_procedures_fct <- factor(all_est_procedures,
                                 levels = all_est_procedures,
                                 labels = c("Oracle model", "Population model", "Complete-case", "Confounded model", 
                                            "IPW", "IPTW", "GR", "Raking (X only)", "RRZ",
                                            "MICE", "MIXGB", "MIRF", 
                                            "TMLE-M", "TMLE-M", "TMLE", 
                                            "TMLE-M", "TMLE",
                                            "TMLE-M", "TMLE",
                                            "a-TMLE-M", "a-TMLE", 
                                            "r-TMLE-M", "r-TMLE",
                                            "r-a-TMLE-M", "r-a-TMLE"))
procedure_hash_table <- data.frame(
  "procedure" = all_est_procedures,
  "nice_procedure" = all_est_procedures_fct
)

estimand_log <- c("mOR","mRR")
final_results <- all_results %>% 
  mutate(yscenario = as.factor(yscenario)) %>% 
  mutate(original_est = est,
         est = ifelse(estimand %in% estimand_log, log(est), est)) %>% 
  mutate(estimand = case_when(
    estimand == "mRR" ~ "mlogRR",
    estimand == "mOR" ~ "mlogOR",
    !(estimand %in% c("mRR", "mOR")) ~ estimand
  )) %>% 
  left_join(all_true_vals, by = c("yscenario", "estimand")) %>% 
  left_join(procedure_hash_table, by = "procedure") %>% 
  filter(seed == 1) %>% 
  select(-seed) %>% 
  mutate(est = ifelse(abs(est) > 1e10, NA, est),
         SE = ifelse(abs(original_est) > 1e10, NA, SE))

final_results %>% 
  filter(procedure == "IPW") %>% 
  pull(est) %>% 
  hist()
# so in this case, it's a combination of both NAs (TMLE-MTO) and non-convergence causing large values
# in the Sentinel paper, we defined "large" as abs(est) > log(10)

na_rm <- TRUE
threshold <- log(10)
final_results %>% 
  filter(estimand == "clogOR") %>% 
  group_by(yscenario, n, procedure) %>% 
  summarize(n_na = sum(is.na(est)),
            n_nonconvergence = sum(abs(est) > threshold, na.rm = TRUE)) %>% 
  mutate(prop_na = n_na / 2500 * 100,
         prop_nonconverge = n_nonconvergence / 2500 * 100) %>% 
  print(n = Inf)

ese_mad <- final_results %>% 
  group_by(yscenario, mscenario, xscenario, n, procedure, nice_procedure, estimand) %>% 
  summarize(ESE = sd(est, na.rm = na_rm), 
            MAD = mad(est, na.rm = na_rm), .groups = "drop")

# in this scenario, oracle and population truth are the same. I'm only going to compute wrt oracle
# note that na.rm = TRUE only removes NAs for TMLE, non-conditional parameters, which I'm not reporting anyways
final_results %>% 
  filter(estimand == "clogOR") %>% 
  group_by(yscenario, mscenario, xscenario, n, procedure, nice_procedure, estimand) %>% 
  summarize(mn_est = mean(est, na.rm = TRUE), mdn_est = median(est, na.rm = TRUE), truth = mean(truth))
results_summaries <- final_results %>% 
  filter(estimand == "clogOR") %>% 
  left_join(ese_mad, by = c("yscenario", "mscenario", "xscenario", "n", "procedure", "nice_procedure", "estimand")) %>% 
  mutate(bias_init = est - truth,
         ase_cover_init = ifelse(is.na(est), 0, cover(mu = truth, est = est, SE = SE)),
         ese_cover_init = ifelse(is.na(est), 0, cover(mu = truth, est = est, SE = ESE))) %>% 
  group_by(yscenario, mscenario, xscenario, n, procedure, nice_procedure, estimand) %>% 
  summarize(mean_bias = mean(bias_init, na.rm = na_rm),
            median_bias = median(bias_init, na.rm = na_rm),
            truncated_mean_bias = winsorize(bias_init, alpha = 0.05, na.rm = na_rm),
            trimmed_mean_bias = mean(bias_init, trim = 0.05, na.rm = na_rm),
            ESE = mean(ESE), 
            mean_ASE = mean(SE, na.rm = na_rm), 
            median_ASE = median(SE, na.rm = na_rm),
            truncated_mean_ASE = winsorize(SE, alpha = 0.05, na.rm = na_rm),
            trimmed_mean_ASE = mean(SE, trim = 0.05, na.rm = na_rm),
            MAD = mean(MAD),
            ase_cover = mean(ase_cover_init, na.rm = na_rm),
            ese_cover = mean(ese_cover_init, na.rm = na_rm), 
            num_complete = n() - sum(is.na(est)),
            noncomplete_proportion = 1 - num_complete / 2500,
            # na_proportion = mean(is.na(est)),
            unreasonable_proportion = mean(abs(est) > threshold | is.na(est)),
            .groups = "drop")
# get the coverage over only nice datasets
coverage_nice <- final_results %>% 
  filter(estimand == "clogOR") %>% 
  left_join(ese_mad, by = c("yscenario", "mscenario", "xscenario", "n", "procedure", "nice_procedure", "estimand")) %>% 
  # a filter for datasets that were problematic for any estimator/estimand combination
  group_by(mc_id, estimand) %>% 
  mutate(problematic_id = any(is.na(est))) %>% 
  ungroup() %>% 
  filter(!problematic_id) %>% 
  mutate(ase_cover_init = cover(mu = truth, est = est, SE = SE),
         ese_cover_init = cover(mu = truth, est = est, SE = ESE)) %>% 
  group_by(yscenario, mscenario, xscenario, n, procedure, nice_procedure, estimand) %>% 
  summarize(ase_cover_nice = mean(ase_cover_init, na.rm = na_rm),
            ese_cover_nice = mean(ese_cover_init, na.rm = na_rm),
            .groups = "drop")

nice_summaries <- results_summaries %>%
  left_join(coverage_nice, by = c("yscenario", "mscenario", "xscenario", "n", "procedure", "nice_procedure", "estimand")) %>% 
  filter(nice_procedure %in% c("IPW", "GR", "MICE", "TMLE")) %>% 
  select(n, nice_procedure, estimand, contains("bias"), ESE, MAD, contains("_ASE"), contains("cover"), contains("proportion")) %>% 
  select(-ese_cover, -ese_cover_nice) %>% 
  rename(Est = nice_procedure, `Mn bias` = mean_bias, `Med bias` = median_bias,
         `WMn bias` = truncated_mean_bias, `TMn bias` = trimmed_mean_bias,
         `Mean ASE` = mean_ASE, `Med ASE` = median_ASE, `WMn ASE` = truncated_mean_ASE,
         `TMn ASE` = trimmed_mean_ASE, `CP` = ase_cover, `CP*` = ase_cover_nice,
         `NP` = noncomplete_proportion, `UP` = unreasonable_proportion) %>% 
  mutate(`bias / SE` = abs(`Med bias` / `Med ASE`), .before = CP)

# create the table -------------------------------------------------------------
font_size <- 8.5
all_estimands <- unique(results_summaries$estimand)
nice_estimands <- c("cOR", "mRD", "mOR", "mRR")

abbreviation_txt_prefix <- "Abbreviations: "
estimand_txt <- c("cOR: conditional odds ratio; ",
                  "mRD: marginal risk difference; ",
                  "mOR: marginal odds ratio; ",
                  "mRR: marginal relative risk; ")
abbreviation_txt_suffix <- paste0("Est: Estimator; ", "Mn: Mean; ", 
                                  "Med: Median; ", 
                                  "WMn: Winsorized mean; ", 
                                  "TMn: trimmed mean; ", 
                                  "ESE: empirical standard error; ",
                                  "MAD: median absolute deviation; ",
                                  "ASE: asymptotic standard error; ",
                                  "bias/SE: absolute Med bias / Med ASE; ",
                                  "CP: coverage probability based on the ASE; ",
                                  "CP*: coverage probability based on the ASE, where non-convergent datasets were dropped for all estimators; ",
                                  # "NP: proportion of non-convergent simulations; ",
                                  # "UP: number of simulations returning an unreasonable point estimate, defined as abs(point estimate) $>$ ln(10); ", 
                                  "GR: generalized raking; ",
                                  "IPW: inverse probability weighting; ",
                                  "MICE: multiple imputation via chained equations; ",
                                  # "MIRF: MI using random forests; ",
                                  "TMLE: targeted maximum likelihood estimation. ")

bold_text <- " Bolding indicates columns that we recommend using."

nice_summaries %>% 
  filter(estimand == "clogOR") %>% 
  select(-estimand) %>% 
  select(-NP, -UP) %>% 
  mutate(across(where(is.numeric), .fns = ~ ifelse(abs(.x) > 10, paste0("\\num{", formatC(.x, format = "e", digits = 2), "}"), sprintf("%.2f", .x))))
# as.character(round(.x, 2))

this_outcome_rate <- ifelse(this_yscenario == "1.15", 0.05, 0.12)
this_prop_observed <- ifelse(this_mscenario == "3.1", 0.2, 0.6)
for (i in 1:length(ns)) {
  # this_estimand <- all_estimands[i]
  # this_nice_estimand <- nice_estimands[i]
  this_estimand <- "clogOR"
  this_nice_estimand <- "cOR"
  this_n <- ns[i]
  this_summ <- nice_summaries %>% 
    filter(estimand == this_estimand, n == this_n) %>% 
    select(-estimand, -n)
  num_nonconverge <- this_summ$NP[this_summ$Est == "GR"] * 2500
  nonconvergence_txt <- paste0("Results are based on 2500 Monte-Carlo replications. ",
                               num_nonconverge, 
                               ifelse(num_nonconverge > 1, " values were ", " value was "),
                               "removed when computing GR performance due to non-convergence.")
  one_over_n_eff <- 1 / sqrt(as.numeric(this_n) * this_outcome_rate * this_prop_observed)
  n_eff_text <- paste0(" The inverse square root of the effective sample size is ", round(one_over_n_eff, 3), ".")
  
  this_summ %>% 
    select(-NP, -UP) %>% 
    mutate(across(where(is.numeric), .fns = ~ ifelse(abs(.x) > 10, paste0("\\num{", formatC(.x, format = "e", digits = 2), "}"), sprintf("%.2f", .x)))) %>% 
    rename(`\\textbf{Med bias}` = `Med bias`, 
           `\\textbf{MAD}` = `MAD`,
           `\\textbf{Med ASE}` = `Med ASE`,
           `\\textbf{CP*}` = `CP*`) %>% 
    knitr::kable(digits = 2, format = "latex", escape = FALSE, booktabs = TRUE,
                 align = c("l", rep("r", 13)), linesep = "", caption = paste0(
                   "Estimating the ", this_nice_estimand, " in a rare-outcome, high missing-data setting, n = ", this_n,  ".",
                   "\\label{tab:synthetic_results_", this_nice_estimand, "_n", this_n, "}"
                 )) %>% 
    kableExtra::kable_styling(font_size = font_size, 
                              # latex_options = c("hold_position", "scale_down")) %>% 
                              latex_options = c("hold_position")) %>% 
    kableExtra::column_spec(column = 2:5, width = "0.3in") %>% 
    kableExtra::column_spec(column = 6:14, width = "0.22in") %>% 
    kableExtra::footnote(general = paste0(
      abbreviation_txt_prefix, estimand_txt[i], abbreviation_txt_suffix, nonconvergence_txt,
      bold_text, n_eff_text
    ), general_title = "", threeparttable = TRUE, escape = FALSE) %>% 
    kableExtra::save_kable(file = paste0(output_dir, "/synthetic_simulation_results_", this_nice_estimand, "_n", this_n, ".tex"))
}
