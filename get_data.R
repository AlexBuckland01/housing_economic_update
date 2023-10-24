
# load libraries -----------------------------------------------------------

library(tidyverse)
library(zoo)
library(readabs)
library(readxl)
library(googlesheets4)
library(RCurl)

# housing -------------------------------------------------------

#housing CPI


#property price growth by suburb

wa_suburbs <- read.csv('wa_available_keys.csv')

wa_suburbs <- wa_suburbs |> mutate(pricegrowth = runif(n(), min=0, max=1)) #generate example data

wa_suburbs |> write_excel_csv('wa_suburbs.csv')


# rentals ----------------------------------------------------------------

#rent cpi


#rental vacancy rates


# construction -------------------------------------------------------

#value of construction



#building activity

approvals <- read_abs_series('A422572J') |> select(date, value) |> 
  mutate(year_quarter = as.yearqtr(date)) |> group_by(year_quarter) |>
  summarize(approvals = sum(value))

completions <- read_abs_series('A83801561R') |> select(date, value) |> 
  mutate(year_quarter = as.yearqtr(date)) |> group_by(year_quarter) |>
  summarize(completions = sum(value))

commencements <- read_abs_series('A83801560L') |> select(date, value) |> 
  mutate(year_quarter = as.yearqtr(date)) |> group_by(year_quarter) |>
  summarize(commencements = sum(value))

building <- approvals |> left_join(commencements) |> left_join(completions) |>
  mutate(year_quarter = yq(year_quarter)) |> na.omit()

building |> write_excel_csv('building.csv')

# affordability -----------------------------------------------------------

#interest and mortgage rates data

download.file(url = 'https://www.rba.gov.au/statistics/tables/xls/f01hist.xls',
              destfile = 'f01hist.xls', method = 'curl')

cash_rates <- read_xls('f01hist.xls', skip = 10) |> 
  rename(date = 'Series ID',
         cash_rate_target = 'FIRMMCRT') |> select(date, cash_rate_target)

download.file(url = 'https://www.rba.gov.au/statistics/tables/xls/f06hist.xls',
              destfile = 'f06hist.xls', method = 'curl')

lending_rates <- read_xls('f06hist.xls', skip = 10) |> 
  rename(date = 'Series ID',
         fixed_rate_outstanding = 'FLRHOOFA',
         variable_rate_new = 'FLRHOFVA',
         fixed_rate_new = 'FLRHOFFA') |> select(date, fixed_rate_outstanding,
                                                fixed_rate_new,
                                                variable_rate_new)

inf_rate <- read_abs_series('A2325847F') |> select(date, value) |>
  rename(yoy_cpi = value)

rates <- cash_rates |> left_join(lending_rates) |> 
  left_join(inf_rate, join_by(closest(date >= date))) |> 
  mutate('date.y' = NULL) |> na.omit()

rates |> write_excel_csv('rates.csv')
