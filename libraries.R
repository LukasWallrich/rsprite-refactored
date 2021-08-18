#!/usr/bin/env Rscript

required_packages <- c("ggplot2", "gridExtra", "moments", "shiny", "shinycssloaders")
res <- unlist(suppressWarnings(lapply(required_packages, requireNamespace, quietly = TRUE)))
if (!all(res)) {
  warning(paste0(note, "The following packages are required for rSPRITE but
                   cannot be loaded: ", paste0(required_packages[!res], collapse = ", ")),
    call. = FALSE
  )
  choice <- readline(prompt = "Should I try to install these packages? (Y/N)")
  if (tolower(substr(choice, 1, 1)) == "y") {
    utils::install.packages(x[!res])
  } else {
    stop("Cannot proceed without these packages.", call. = FALSE)
  }
}

library(ggplot2)
library(gridExtra)
library(moments)
library(shiny)
library(shinycssloaders)
