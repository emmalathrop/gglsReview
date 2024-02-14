
library(tidyr)
library(dplyr)
library(ggplot2)
library(tidyverse)

ds <- data.frame(sample1 = rnorm(100),
                 sample2 = rnorm(100, mean = 2),
                 sample3 = rnorm(100, sd=2))

ds <- ds %>%
  pivot_longer(cols = c("sample1", "sample2", "sample3"), names_to = "Sample", values_to = "Values")

now <- Sys.time()
timestamp <- function(time) format(time, "%Y-%B-%d_%H-%M-%S")
outfile_path <- function(infile) {
  paste0(timestamp(now), "_", sub("(.*)([.]csv$)", "\\1_clean\\2", infile))
}

infile <- "dummy_ds.csv"
#write_csv(ds, outfile_path(infile)) #Here's the dummy dataset



#Test assumptions, route to certain tests based on outcome of the testing
#Provide warning if user selects test that violates assumptions
