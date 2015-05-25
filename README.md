# comics

cornell package for omics data analysis

### Installation

To install the package, you first need the 
[*devtools*](https://github.com/hadley/devtools) package.

```{r}
install.packages("devtools")
```

Then you can install the *comics* package using

```{r}
library(devtools)
install_bitbucket(
  "graumannlab/comics",
  auth_user = "your bitbucket username", 
  password  = "your bitbucket password"  
)
```