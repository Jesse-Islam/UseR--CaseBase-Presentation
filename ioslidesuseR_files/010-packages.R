## ---- required-packages ----

getPckg <- function(pckg) install.packages(pckg, repos = "http://cran.r-project.org")

pckg = try(require(knitr))
if(!pckg) {
    cat("Installing 'knitr' from CRAN\n")
    getPckg("knitr")
    require(knitr)
}
pckg = try(require(splines))
if(!pckg) {
  cat("Installing 'splines' from CRAN\n")
  getPckg("splines")
  require(splines)
}

pckg = try(require(timereg))
if(!pckg) {
  cat("Installing 'timereg' from CRAN\n")
  getPckg("timereg")
  require(timereg)
}
pckg = try(require(pacman))
if(!pckg) {
  cat("Installing 'knitr' from CRAN\n")
  getPckg("pacman")
  require(pacman)
}
pckg = try(require(flextable))
if(!pckg) {
  cat("Installing 'flextable' from CRAN\n")
  getPckg("flextable")
  require(flextable)
}
pckg = try(require(casebase))
if(!pckg) {
  cat("Installing 'casebase' through pacman \n")
  pacman::p_install_gh('sahirbhatnagar/casebase')
  require(casebase)
}
pckg = try(require(ISLR))
if(!pckg) {
    cat("Installing 'ISLR' from CRAN\n")
    getPckg("ISLR")
    require(ISLR)
}

pckg = try(require(data.table))
if(!pckg) {
    cat("Installing 'data.table' from CRAN\n")
    getPckg("data.table")
    require(data.table)
}


pckg = try(require(party))
if(!pckg) {
    cat("Installing 'party' from CRAN\n")
    getPckg("party")
    require(party)
}



pckg = try(require(rpart))
if(!pckg) {
    cat("Installing 'rpart' from CRAN\n")
    getPckg("rpart")
    require(rpart)
}

pckg = try(require(rpart.plot))
if(!pckg) {
    cat("Installing 'rpart.plot' from CRAN\n")
    getPckg("rpart.plot")
    require(rpart.plot)
}

pckg = try(require(ggplot2))
if(!pckg) {
    cat("Installing 'ggplot2' from CRAN\n")
    getPckg("ggplot2")
    require(ggplot2)
}

pckg = try(require(knitcitations))
if(!pckg) {
    cat("Installing 'knitcitations' from CRAN\n")
    getPckg("knitcitations")
    require(knitcitations)
}


pckg = try(require(devtools))
if(!pckg) {
    cat("Installing 'devtools' from CRAN\n")
    getPckg("devtools")
    require(devtools)
}

pckg = try(require(tidyverse))
if(!pckg) {
  cat("Installing 'tidyverse' from CRAN\n")
  getPckg("tidyverse")
  require(tidyverse)
}
pckg = try(require(magrittr))
if(!pckg) {
  cat("Installing 'magrittr' from CRAN\n")
  getPckg("magrittr")
  require(magrittr)
}
pckg = try(require(survival))
if(!pckg) {
  cat("Installing 'survival' from CRAN\n")
  getPckg("survival")
  require(survival)
}
pckg = try(require(DT))
if(!pckg) {
    cat("Installing 'DT' from Github\n")
    devtools::install_github('rstudio/DT')
    require(DT)
}

