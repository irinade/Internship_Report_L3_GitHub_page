#!/usr/bin/Rscript --vanilla

library(knitr)

args <- commandArgs(trailingOnly=TRUE)
name <- tools::file_path_sans_ext(args[1])
purl(args[1], output = paste(name, "R", sep="."), documentation = 2)
