# Introduction
The R package **tri2basis** is for constructing the bivariate spline basis functions on triangulations, which is drawing attentions from many fields such as spatial statistics, functional data analysis.

# Download and Install
To download the package, type the following at the R command line:

```r
install.packages("devtools")
if(!require("knitr")){
  install.packages("knitr")
}
devtools::install_github("ShirunShen/tri2basis", build_vignettes = TRUE)
```

# Examples
In the vignettes of this package, there are two toy examples that help illustrate how to use this package. Users can find the vignettes in the document **User guides, package vignettes and other documentation** of **tri2basis** package page.


# References and Citation

Zhou, Lan, and Huijun Pan. 2014a. “Principal Component Analysis of Two-Dimensional Functional Data.” Journal Article. Journal of Computational and Graphical Statistics 23 (3): 779–801.

———. 2014b. “Smoothing Noisy Data for Irregular Regions Using Penalized Bivariate Splines on Triangulations.” Journal Article. Computational Statistics 29 (1-2): 263–81.
