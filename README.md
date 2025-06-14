# DDcentutils

R package created by the SCSC Ecosystem Modeling and Data Consortium Team to facilitate the use of the DayCent model and visualization of results. 
This package is under construction and we welcome feedback. 
Please note that the package does not include the DayCent model. Access to the model is currently being managed by the SCSC Ecosystem Modeling and Data Consortium at Colorado State University.

For more information about model access, please visit https://www.soilcarbonsolutionscenter.com/consortium.


# How to Install

You can install this package using devtools:
```{r}
# devtools version 2.4.5 (2022)
devtools::install_github("CSU-Soil-Carbon-Solution-Center/DDcentutils", dependencies = TRUE, upgrade = c("ask"))
```

Or alternatively you can download the compressed package (tar.gz file) and install it locally:
```{r}
install.packages(here::here("DDcentutils_0.0.0.9000.tar.gz"), repos = NULL, source = TRUE)
```
