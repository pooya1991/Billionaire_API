#!/usr/bin/env Rscript

library(plumber)

options(httr_oob_default = TRUE)
funcs <- list.files("R", full.names = TRUE)
n <- length(funcs)
for (i in 1:n) {
  source(funcs[i])
}
r <- plumb("rest_controller.R")
#r <- plumb("test.R")
r$run(port=3009, host="0.0.0.0")
