# Cardinal

## Mass spectrometry imaging tools

*Cardinal* provides an interface for manipulating mass spectrometry (MS) imaging datasets, simplifying most of the basic programmatic tasks encountered during the statistical analysis of MS imaging data. These include image manipulation and processing of both images and mass spectra, and dynamic plotting of both.

While pre-processing steps including normalization, baseline correction, and peak-picking are provided, the core functionality of the package is statistical analysis. The package includes classification and clustering methods based on nearest shrunken centroids, as well as traditional tools like PCA and PLS.

## User Installation

### Bioconductor Release

*Cardinal* can be installed via the *BiocManager* package.

This is the **recommended** installation method.

```{r install, eval=FALSE}
if (!require("BiocManager", quietly = TRUE))
    install.packages("BiocManager")

BiocManager::install("Cardinal")
```

The same function can be used to update *Cardinal* and other Bioconductor packages.

Once installed, *Cardinal* can be loaded with `library()`:

```{r library, eval=FALSE}
library(Cardinal)
```

### Github Release

*Cardinal* can also be installed via the *remotes* package.

```{r install, eval=FALSE}
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("kuwisdelu/Cardinal", ref=remotes::github_release())
```

Previous releases can be installed by specifying the exact version.

```{r library, eval=FALSE}
remotes::install_github("kuwisdelu/Cardinal@v3.6.2")
```

## Developer Installation

### Bioconductor Devel

The Bioconductor development version of *Cardinal* can also be installed via the *BiocManager* package.

```{r install, eval=FALSE}
BiocManager::install("Cardinal", version="devel")
```

This version is **unstable** and should not be used for critical work. However, it is typically more stable than Github devel.

This version should *typically* pass `R CMD check` without errors.

### Github Devel

The most cutting edge version of *Cardinal* can be installed from Github via the *remotes* package.

```{r install, eval=FALSE}
if (!require("remotes", quietly = TRUE))
    install.packages("remotes")

remotes::install_github("kuwisdelu/Cardinal")
```

This version is **unstable** and only recommended for developers. It should not be used for critical work.


