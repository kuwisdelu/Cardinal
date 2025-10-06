# contrast() Function for Cardinal

## Overview

The `contrast()` function provides post-hoc contrast analysis for `MeansTest` objects fitted with `use_lmer = TRUE`. It wraps `emmeans::emmeans()` and `emmeans::contrast()` to compute contrasts across all m/z features.

## Requirements

- Models must be fit with `use_lmer = TRUE` (uses lmerTest::lmer for Satterthwaite df)
- The `emmeans` and `lmerTest` packages must be installed

## Usage

```r
contrast(object, specs, method = "pairwise", adjust = "none", 
         verbose = getCardinalVerbose(), chunkopts = list(),
         BPPARAM = getCardinalBPPARAM(), ...)
```

## Arguments

- **object**: A `MeansTest` object with lmer models
- **specs**: Specification for emmeans (e.g., `"condition"`, `~ condition`, or more complex specs)
- **method**: Contrast method (default: `"pairwise"`)
  - `"pairwise"`: All pairwise comparisons
  - `"trt.vs.ctrl"`: Treatment vs control
  - `"poly"`: Polynomial contrasts
  - Custom contrast matrices (named list)
  - See `?emmeans::contrast` for all options
- **adjust**: P-value adjustment method (default: `"none"`)
  - `"none"`: No adjustment
  - `"bonferroni"`: Bonferroni correction
  - `"tukey"`: Tukey HSD
  - `"fdr"`: False discovery rate
  - See `?emmeans::summary.emmGrid` for all options
- **...**: Additional arguments passed to `emmeans::emmeans()` or `emmeans::contrast()`

## Return Value

A `ResultsList` object containing:
- **Elements**: One emmeans contrast object per m/z feature
- **mcols**: DataFrame with original metadata plus contrast statistics
  - Columns named as: `"[contrast_name].estimate"` and `"[contrast_name].pvalue"`
  - For pairwise: `"A - B.estimate"`, `"A - B.pvalue"`, etc.
  - For custom contrasts: Uses names from the contrast specification

## Examples

### Basic pairwise contrasts

```r
# Fit models with lmer
mt <- meansTest(msi_data, fixed = ~condition, random = ~1|subject, 
                use_lmer = TRUE)

# Compute pairwise contrasts
contr <- contrast(mt, specs = "condition", method = "pairwise")

# View contrast for first m/z
contr[[1]]

# Access statistics
mcols(contr)$"A - B.pvalue"
```

### With multiple comparison adjustment

```r
# Bonferroni adjustment
contr_bonf <- contrast(mt, specs = "condition", method = "pairwise",
                       adjust = "bonferroni")

# Tukey HSD adjustment
contr_tukey <- contrast(mt, specs = "condition", method = "pairwise",
                        adjust = "tukey")
```

### Treatment vs control contrasts

```r
# Compare all treatments to control (first level)
contr_trt <- contrast(mt, specs = "condition", method = "trt.vs.ctrl")
```

### Complex models with interactions

```r
# Fit model with interaction
mt2 <- meansTest(msi_data, fixed = ~condition*tissue, random = ~1|subject,
                 use_lmer = TRUE)

# Contrasts for condition within each tissue level
contr_by <- contrast(mt2, specs = "condition", by = "tissue",
                     method = "pairwise")
```

### Custom contrast matrices

```r
# Define custom contrasts
my_contrasts <- list(
  "Treatment_effect" = c(-1, 1, 0),
  "Control_vs_others" = c(-2, 1, 1)
)

contr_custom <- contrast(mt, specs = "condition", method = my_contrasts)
```

## Notes

1. **Failed models**: Returns `NA` for m/z features where the model failed to fit
2. **Singular fits**: Treated the same as successful fits (no special handling)
3. **Error handling**: If `use_lmer = FALSE`, throws an error with helpful message
4. **Performance**: Uses Cardinal's chunking and parallel processing infrastructure
5. **Column naming**: Follows emmeans naming conventions for contrast names

## See Also

- `?meansTest` - Fit mixed effects models
- `?emmeans::emmeans` - Estimated marginal means
- `?emmeans::contrast` - Contrasts and comparisons
- `demo_lmer_functionality.R` - Complete working example
