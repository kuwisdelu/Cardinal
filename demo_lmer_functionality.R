#!/usr/bin/env Rscript
# Demonstration script for use_lmer parameter in meansTest
# This script compares nlme::lme vs lme4::lmer for mixed effects models
# and demonstrates the new contrast() function

# Load packages
library(devtools)
load_all("/Users/ethanrogers/OtherProjects/Cardinal")
#library(Cardinal)
library(lmerTest)  # Extends lme4 with Satterthwaite df
library(emmeans)

cat("=== Demonstration of use_lmer parameter in meansTest ===\n\n")

# Create test data
RNGkind("L'Ecuyer-CMRG")
set.seed(1)
cat("RNG and seed set\n")

# Create test data
cat("Creating improved test data...\n")
subj_var <- rnorm(n=6, mean=1.5, sd=1)

# Generate base image once
k <- simulateImage(preset=4, nrun=3, npeaks=10, dim=c(10,10), centroided = TRUE)

# Create run-level random effects (18 runs total: 6 subjects × 3 tissues)
run_effects <- rnorm(n=18, mean=0, sd=0.3)

# Subject pair 1 (subjects 1 and 2)
x <- k
pData(x)$subject <- as.factor(ifelse(pData(x)$condition == "A", 1, 2))
pData(x)$tissue <- as.factor(substr(pData(x)$run, 5, 5))
pData(x)$gamma <- subj_var[pData(x)$subject]
# Add subject-specific + run-specific variation
run_idx <- as.numeric(interaction(pData(x)$subject, pData(x)$tissue, drop=TRUE))
intensity(x) <- intensity(x) + 
  matrix(rep(subj_var[pData(x)$subject],10), ncol = 600) + 
  matrix(rep(run_effects[run_idx],10), ncol = 600) +
  matrix(rnorm(n=600*10, mean=0, sd=0.1), ncol = 600)

# Subject pair 2 (subjects 3 and 4) - different run effects
y <- k
pData(y)$subject <- as.factor(ifelse(pData(y)$condition == "A", 3, 4))
pData(y)$tissue <- as.factor(substr(pData(y)$run, 5, 5))
pData(y)$gamma <- subj_var[pData(y)$subject]
run_idx <- 6 + as.numeric(interaction(pData(y)$subject, pData(y)$tissue, drop=TRUE))
intensity(y) <- intensity(y) + 
  matrix(rep(subj_var[pData(y)$subject],10), ncol = 600) + 
  matrix(rep(run_effects[run_idx],10), ncol = 600) +
  matrix(rnorm(n=600*10, mean=0, sd=0.1), ncol = 600)

# Subject pair 3 (subjects 5 and 6) - different run effects
z <- k
pData(z)$subject <- as.factor(ifelse(pData(z)$condition == "A", 5, 6))
pData(z)$tissue <- as.factor(substr(pData(z)$run, 5, 5))
pData(z)$gamma <- subj_var[pData(z)$subject]
run_idx <- 12 + as.numeric(interaction(pData(z)$subject, pData(z)$tissue, drop=TRUE))
intensity(z) <- intensity(z) + 
  matrix(rep(subj_var[pData(z)$subject],10), ncol = 600) + 
  matrix(rep(run_effects[run_idx],10), ncol = 600) +
  matrix(rnorm(n=600*10, mean=0, sd=0.1), ncol = 600)

x <- cbind(x, y, z)
run(x) <- paste0(run(x),pData(x)$subject)
# Define formulas
fixed_formula <- ~ condition
random_formula <- ~ 1 | subject

cat("Fixed effects formula:", deparse(fixed_formula), "\n")
cat("Random effects formula:", deparse(random_formula), "\n\n")

# Run meansTest with nlme::lme (default)
cat("=== Running meansTest with nlme::lme (use_lmer = FALSE) ===\n")
start_time <- Sys.time()
results_lme <- meansTest(
  x, 
  fixed=~condition, 
  random=~1|subject, 
  samples=run(x),
  use_lmer = FALSE,
  verbose = TRUE
)
time_lme <- Sys.time() - start_time
cat("Time elapsed:", round(time_lme, 3), "seconds\n\n")

# Run meansTest with lme4::lmer
cat("=== Running meansTest with lme4::lmer (use_lmer = TRUE) ===\n")
start_time <- Sys.time()
results_lmer <- meansTest(
  x, 
  fixed=~condition*tissue, 
  random=~1|subject, 
  samples=run(x),
  use_lmer = TRUE,
  verbose = TRUE
)
time_lmer <- Sys.time() - start_time
cat("Time elapsed:", round(time_lmer, 3), "seconds\n\n")

# Compare results
cat("=== Comparison of Results ===\n\n")

cat("Top 10 features by LR statistic (nlme::lme):\n")
print(head(topFeatures(results_lme, n = 10), 10))
cat("\n")

cat("Note: lme4::lmer models do not perform hypothesis tests.\n")
cat("The lmer models are fit with REML=TRUE for parameter estimation.\n\n")

# Check model classes
cat("Model class from nlme::lme:", class(results_lme[[1]])[1], "\n")
cat("Model class from lme4::lmer:", class(results_lmer[[1]])[1], "\n\n")

cat("=== Demonstration of contrast() function ===\n\n")
cat("The contrast() function provides post-hoc contrasts for lmer models\n")
cat("using emmeans::emmeans() and emmeans::contrast()\n\n")

# Example 1: Pairwise contrasts for condition
cat("--- Example 1: Pairwise contrasts for condition ---\n")
contr1 <- Cardinal::contrast(
  results_lmer,
  specs = "condition",
  method = "pairwise",
  emm_adjust = "none",
  verbose = FALSE
)

cat("Contrast object for feature 1:\n")
print(contr1[[1]])
cat("\nTop 3 features by p-value:\n")
stats1 <- mcols(contr1)
pval_col <- grep("\\.pvalue$", names(stats1), value=TRUE)[1]
top3 <- order(stats1[[pval_col]])[1:3]
print(stats1[top3, grep("estimate|pvalue", names(stats1))])
cat("\n\n")

cat("Top 5 features using topFeatures() (default, first non-NA pvalue, ascending):\n")
tf1 <- topFeatures(contr1, n = 5)
print(tf1[, grep("estimate|pvalue|fdr", names(tf1)), drop=FALSE])
cat("\nTop 5 features using topFeatures(sort.by = \"A - B.fdr\"):\n")
tf1_named <- topFeatures(contr1, n = 5, sort.by = "A - B.fdr")
print(tf1_named[, grep("estimate|pvalue|fdr", names(tf1_named)), drop=FALSE])
cat("\nTop 5 features using topFeatures(sort.by = index of 'A - B.fdr'):\n")
idx1 <- which(names(tf1) == "A - B.fdr")
if (length(idx1) == 1) {
  tf1_idx <- topFeatures(contr1, n = 5, sort.by = idx1)
  print(tf1_idx[, grep("estimate|pvalue|fdr", names(tf1_idx)), drop=FALSE])
} else {
  cat("Could not locate column index for 'A - B.fdr' in topFeatures output.\n")
}
cat("\n\n")

# Example 2: Contrasts by tissue (marginalizing over condition)
cat("--- Example 2: Pairwise contrasts for tissue ---\n")
contr2 <- Cardinal::contrast(
  results_lmer,
  specs = "tissue",
  method = "pairwise",
  emm_adjust = "none",
  verbose = FALSE
)

cat("Contrast object for feature 1:\n")
print(contr2[[1]])
cat("\n\n")

cat("Top 5 features for tissue contrasts using topFeatures() (default):\n")
tf2 <- topFeatures(contr2, n = 5)
# Show only estimate/pvalue/fdr columns for brevity
print(tf2[, grep("estimate|pvalue|fdr", names(tf2)), drop=FALSE])
cat("\n\n")

# Example 3: Condition contrasts within each tissue level
cat("--- Example 3: Condition contrasts within each tissue level ---\n")
contr3 <- Cardinal::contrast(
  results_lmer,
  specs = "condition",
  by = "tissue",
  method = "pairwise",
  emm_adjust = "none",
  verbose = FALSE
)

cat("Contrast object for feature 1 (condition within each tissue):\n")
print(contr3[[1]])
cat("\n\n")

# Example 4: Treatment vs control style contrasts
cat("--- Example 4: Treatment vs control (first level as reference) ---\n")
contr4 <- Cardinal::contrast(
  results_lmer,
  specs = "condition",
  method = "trt.vs.ctrl",
  emm_adjust = "none",
  verbose = FALSE
)

cat("Contrast object for feature 1:\n")
print(contr4[[1]])
cat("\n\n")

# Example 5: Multiple comparison adjustments
cat("--- Example 5: Comparing p-value adjustment methods ---\n")
cat("Note: Using 'tissue' (3 levels) to demonstrate adjustment with multiple contrasts\n")
contr_none <- Cardinal::contrast(results_lmer, specs = "tissue", 
                                  method = "pairwise", emm_adjust = "none", verbose = FALSE)
contr_bonf <- Cardinal::contrast(results_lmer, specs = "tissue", 
                                  method = "pairwise", emm_adjust = "bonferroni", verbose = FALSE)
contr_fdr <- Cardinal::contrast(results_lmer, specs = "tissue", 
                                 method = "pairwise", emm_adjust = "fdr", verbose = FALSE)

# Get all p-value columns for tissue contrasts
tissue_pval_cols <- grep("\\.pvalue$", names(mcols(contr_none)), value=TRUE)

cat("\nP-values for feature 1 with different adjustments:\n")
cat("Contrast 1 (tissue1 - tissue2):\n")
cat("  None:       ", mcols(contr_none)[[tissue_pval_cols[1]]][1], "\n")
cat("  Bonferroni: ", mcols(contr_bonf)[[tissue_pval_cols[1]]][1], "\n")
cat("  FDR:        ", mcols(contr_fdr)[[tissue_pval_cols[1]]][1], "\n")
cat("\nContrast 2 (tissue1 - tissue3):\n")
cat("  None:       ", mcols(contr_none)[[tissue_pval_cols[2]]][1], "\n")
cat("  Bonferroni: ", mcols(contr_bonf)[[tissue_pval_cols[2]]][1], "\n")
cat("  FDR:        ", mcols(contr_fdr)[[tissue_pval_cols[2]]][1], "\n")
cat("\nContrast 3 (tissue2 - tissue3):\n")
cat("  None:       ", mcols(contr_none)[[tissue_pval_cols[3]]][1], "\n")
cat("  Bonferroni: ", mcols(contr_bonf)[[tissue_pval_cols[3]]][1], "\n")
cat("  FDR:        ", mcols(contr_fdr)[[tissue_pval_cols[3]]][1], "\n")
cat("\n")

# Example 6: Custom contrast matrix
cat("--- Example 6: Custom contrast matrix ---\n")
# Define custom contrasts as a list
custom_contrasts <- list(
  "A vs B" = c(1, -1)  # Condition A minus Condition B
)
contr6 <- Cardinal::contrast(
  results_lmer,
  specs = "condition",
  method = custom_contrasts,
  emm_adjust = "none",
  verbose = FALSE
)

cat("Custom contrast object for feature 1:\n")
print(contr6[[1]])
cat("\n\n")

# Example 7: Interaction contrasts
cat("--- Example 7: Interaction contrasts (condition:tissue) ---\n")
contr7 <- Cardinal::contrast(
  results_lmer,
  specs = c("condition", "tissue"),
  method = "pairwise",
  emm_adjust = "none",
  verbose = FALSE
)

cat("Interaction contrast object for feature 1 (first few contrasts):\n")
print(head(contr7[[1]], 3))
cat("\n\n")

# Example 8: Consecutive/sequential contrasts
cat("--- Example 8: Consecutive (sequential) contrasts ---\n")
cat("Compares each level to the next (useful for ordered factors)\n")
contr8 <- Cardinal::contrast(
  results_lmer,
  specs = "tissue",
  method = "consec",
  emm_adjust = "none",
  verbose = FALSE
)

cat("Consecutive contrasts for feature 1:\n")
print(contr8[[1]])
cat("\n\n")

# Example 9: Advanced custom contrast matrices
cat("--- Example 9: Advanced custom contrast matrices ---\n")
cat("Multiple custom contrasts with meaningful names\n")

# For condition*tissue model, we have 6 combinations:
# A-tissue1, A-tissue2, A-tissue3, B-tissue1, B-tissue2, B-tissue3
# Order: A1, A2, A3, B1, B2, B3

advanced_contrasts <- list(
  "Main_effect_B_vs_A" = c(-1/3, -1/3, -1/3, 1/3, 1/3, 1/3),  # Average B - Average A
  "Tissue2_vs_1_in_A" = c(-1, 1, 0, 0, 0, 0),                  # A-tissue2 vs A-tissue1
  "Tissue3_vs_1_in_B" = c(0, 0, 0, -1, 0, 1),                  # B-tissue3 vs B-tissue1
  "Interaction_effect" = c(1, -1, 0, -1, 1, 0)                 # (A2-A1) - (B2-B1)
)

contr9 <- Cardinal::contrast(
  results_lmer,
  specs = c("condition", "tissue"),
  method = advanced_contrasts,
  emm_adjust = "none",
  verbose = FALSE
)

cat("Advanced custom contrasts for feature 1:\n")
print(contr9[[1]])
cat("\n")
cat("Column names in mcols:\n")
print(grep("estimate|pvalue", names(mcols(contr9)), value=TRUE))
cat("\n\n")


tf9 <- topFeatures(contr9, sort.by = 4)
tf9


# Example 10: Effect coding (deviation from grand mean)
cat("--- Example 10: Effect coding (eff method) ---\n")
cat("Compares each level to the grand mean\n")
contr10 <- Cardinal::contrast(
  results_lmer,
  specs = "tissue",
  method = "eff",
  emm_adjust = "none",
  verbose = FALSE
)

cat("Effect coding contrasts for feature 1:\n")
print(contr10[[1]])
cat("\n\n")

# Example 11: Formula notation with ~ condition * tissue
cat("--- Example 11: Formula notation (~ condition * tissue) ---\n")
cat("Using formula syntax for specs (equivalent to c('condition', 'tissue'))\n")
contr11 <- Cardinal::contrast(
  results_lmer,
  specs = ~ condition * tissue,
  method = "pairwise",
  emm_adjust = "none",
  verbose = FALSE
)

cat("Formula-based contrasts for feature 1 (first 3):\n")
print(head(contr11[[1]], 3))
cat("\n\n")

# Example 12: Polynomial contrasts for ordered factors
cat("--- Example 12: Polynomial contrasts for tissue (ordered factor) ---\n")
cat("Useful for dose-response or ordered categorical variables\n")
contr12 <- Cardinal::contrast(
  results_lmer,
  specs = "tissue",
  method = "poly",
  emm_adjust = "none",
  verbose = FALSE
)

cat("Polynomial contrasts for feature 1:\n")
print(contr12[[1]])
cat("\n\n")

cat("\n=== Demonstration Complete ===\n")
cat("\nKey takeaways:\n")
cat("1. Set use_lmer = TRUE to use lmerTest::lmer for Satterthwaite df\n")
cat("2. lmerTest::lmer uses REML = TRUE for parameter estimation\n")
cat("3. Hypothesis tests are only performed with nlme::lme (use_lmer = FALSE)\n")
cat("4. lmerTest::lmer is typically faster than nlme::lme\n\n")

cat("5. contrast() function provides flexible post-hoc comparisons:\n")
cat("   - Example 1: Basic pairwise contrasts\n")
cat("   - Example 2: Contrasts for different factors (tissue)\n")
cat("   - Example 3: Contrasts within levels (by = argument)\n")
cat("   - Example 4: Treatment vs control comparisons\n")
cat("   - Example 5: Multiple comparison adjustments (none, bonferroni, fdr, tukey)\n")
cat("     NOTE: Adjustment only differs when testing multiple contrasts (e.g., 3 tissue levels)\n")
cat("           Single contrasts (e.g., 2-level factors) show no difference since p×1=p\n")
cat("   - Example 6: Custom contrast matrices\n")
cat("   - Example 7: Interaction contrasts\n")
cat("   - Example 8: Consecutive (sequential) contrasts\n")
cat("   - Example 9: Advanced custom contrast matrices with meaningful names\n")
cat("   - Example 10: Effect coding (deviation from grand mean)\n")
cat("   - Example 11: Formula notation (~ condition * tissue)\n")
cat("   - Example 12: Polynomial contrasts for ordered factors\n\n")

cat("6. All contrast methods return:\n")
cat("   - ContrastResults (ResultsList subclass) with contrast objects per m/z\n")
cat("   - Wide DataFrame with estimates and p-values in mcols; use topFeatures() to add .fdr columns and rank features\n")
cat("   - Full emmeans contrast objects accessible via [[i]]\n")
