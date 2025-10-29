# DDcentutils

R package created by the SCSC Ecosystem Modeling and Data Consortium Team to facilitate the use of the DayCent model and visualization of results.
This package is under construction and we welcome feedback.
We created a Discussions forum on GitHub to facilitate Q&A about the package and suggest ideas for further development: <https://github.com/CSU-Soil-Carbon-Solution-Center/DDcentutils/discussions> You can also report bugs by creating an issue on the repository.
For more about creating an issue, please visit: <https://docs.github.com/en/issues/tracking-your-work-with-issues/using-issues/creating-an-issue>

Please note that the package does not include the DayCent model.
Access to the model is currently being managed by the SCSC Ecosystem Modeling and Data Consortium at Colorado State University.
For more information about model access, please visit <https://www.soilcarbonsolutionscenter.com/consortium>.

# How to Install

You can install this package using the install_github function from the devtools package:

```{r}
# devtools version 2.4.5 (2022)
devtools::install_github("CSU-Soil-Carbon-Solution-Center/DDcentutils", dependencies = TRUE, upgrade = c("ask"))
```

Or alternatively you can download the compressed package (tar.gz file) and install it locally (requires Rtools):

```{r}
install.packages(here::here("DDcentutils_0.1.0.tar.gz"), repos = NULL, source = TRUE)
```
