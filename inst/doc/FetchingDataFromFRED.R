## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(eFRED)
api_key <- "abcdefghijklmnopqrstuvwxyz123456"
set_fred_key(api_key)

## ----include=FALSE------------------------------------------------------------
set_fred_key("04dc6777a721d81414a460093933e0ae")

## -----------------------------------------------------------------------------
df <- fred(y = "GDPC1", "unrate", all=FALSE)
head(df)

## -----------------------------------------------------------------------------
attr(df, "info")

## -----------------------------------------------------------------------------
search_results <- fred_search("unemployment", args = list(limit = 3))
search_results

