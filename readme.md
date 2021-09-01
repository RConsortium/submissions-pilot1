## Overview

To initiate the project on RStudio Cloud, you need to run the code below 
for start-up scripts. 

```
source("inst/startup.R")
```

## Original Readme file 

`esubdemo` is a demo project to illustrate how to organize analysis scripts 
in an R package folder structure. 

The demo project is following the concepts discussed in:

- [Marwick, B., Boettiger, C., & Mullen, L. (2018). Packaging data analytical work reproducibly using R (and friends). The American Statistician, 72(1), 80-88.](https://peerj.com/preprints/3192/)
- [Wu, P., Palukuru, U. P., Luo, Y., Nepal, S., & Zhang, Y. (2021) Analysis and reporting in regulated clinical trial environment using R. PharmaSUG 2021](https://www.pharmasug.org/proceedings/2021/AD/PharmaSUG-2021-AD-079.pdf)

## Folder Structure

By using R package folder structure and tools, we are able to achieve:

- Consistency 
- Automation
- Reproducibility 
- Compliance

Below are minimal sufficient folders and files leverage R package folder structure.  

- `*.Rproj`: RStudio project file used to open RStudio project.
- `DESCRIPTION`: Metadata for a package including authors, license, dependency etc.
- `vignettes/`: Analysis scripts using Rmarkdown.
- `R/`: Project specific R functions.
- `man/`: Manual of project specific R functions.

There are additional folders and files required in this demo for an analysis project

> people may use different folder name as it is not a standard R package folder.
> These folders and files needs to be added in `.Rbuildignore` to pass
> R package check 

- `adam`: ADaM datasets in `.sas7bdat` format.
  + one may also put it in `inst/exdata` following [R package convention](https://r-pkgs.org/data.html)
  + in reality, we suggest to have real data saved outside of this project. (e.g. in a database)
- `output`: TLFs output 
- `renv.lock` and `renv`: R package management using `renv` package. ([Introduction](https://rstudio.github.io/renv/articles/renv.html)) 
- `_pkgdown.yml`: [pkgdown](https://pkgdown.r-lib.org/articles/pkgdown.html) configuration file
- `.Rprofile`: Project startup file to setup running environment including R version, repository, folder path etc. 
  - We further use `inst/startup.R` and `R/zzz.R` to allow the startup file is executed while running. `devtools::load_all()` and RStudio build panel. 
  

