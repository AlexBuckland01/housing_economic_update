
# load libraries -----------------------------------------------------------

library(tidyverse)
library(zoo)
library(readabs)
library(readxl)
library(RCurl)

# housing -------------------------------------------------------

#housing CPI

perth_dwell <- read_abs_series('A2329922T') |> select(date, value) |> 
  mutate(date = as.yearqtr(date)) |> rename(perth = value)

aus_dwell <- read_abs_series('A2329942A')|> select(date, value) |> 
  mutate(date = as.yearqtr(date)) |> rename(wacc = value)

dwell_cpi <- perth_dwell |> left_join(aus_dwell) |>
  mutate(date = yq(date)) |> na.omit()

dwell_cpi |> write_excel_csv('dwell_cpi.csv')

#property price growth by suburb

wa_suburbs <- read.csv('wa_available_keys.csv')

wa_suburbs <- wa_suburbs |> mutate(pricegrowth = runif(n(), min=0, max=1)) #generate example data

wa_suburbs |> write_excel_csv('wa_suburbs.csv')


# rentals ----------------------------------------------------------------

#rent cpi

perth_rents <- read_abs_series('A2326412L')|> select(date, value) |> 
  mutate(date = as.yearqtr(date)) |> rename(perth = value)

aus_rents <- read_abs_series('A2326432W')|> select(date, value) |> 
  mutate(date = as.yearqtr(date)) |> rename(wacc = value)

rents_cpi <- perth_rents |> left_join(aus_rents) |>
  mutate(date = yq(date)) |> na.omit()

rents_cpi |> write_excel_csv('rents_cpi.csv')

#rental vacancy rates

#wip

# construction -------------------------------------------------------

#value of construction

con_value <- read_abs('8752.0', tables = c('4')) |> 
  filter(series_type == "Seasonally Adjusted") |>
  select(date, series, value)
  
con_value <- con_value |> separate_wider_delim(series, 
                                  delim = " ;", 
                                  names = c('series', 
                                            'measure', 
                                            'state', 
                                            'type', 
                                            'moretype'), too_many = 'drop') |>
  mutate(across(c('series', 
                  'measure', 
                  'state', 
                  'type', 
                  'moretype'), str_squish)) |>
  filter(type == "Total (Type of Work)") |> 
  mutate(series = NULL, 
         measure = NULL,
         type = NULL,
         moretype = NULL) |> 
  na.omit()

con_value |> group_by(state) |> 
  mutate(YoY = (value - lag(value, 4)) / lag(value, 4) * 100) |> 
  select(date, state, YoY) |> tail(28) |>   mutate(date = as.yearqtr(date)) |>
  pivot_wider(names_from = date, values_from = YoY) |> na.omit() |>
  write_excel_csv('con_value.csv')

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

#home repayments composition

download.file(url = 'https://www.rba.gov.au/statistics/tables/xls/e13hist.xlsx',
              destfile = 'e13hist.xlsx', method = 'curl')

repayments <- read_xlsx('e13hist.xlsx', skip = 10) |> 
  rename(date = 'Series ID',
         interest_charged = 'LPHTIC',
         scheduled_payment = 'LPHTSP',
         excess_payment = 'LPHTEX') |> select(date,
                                              interest_charged,
                                              scheduled_payment,
                                              excess_payment)

repayments |> write_excel_csv('repayments.csv')

population <- read_abs_series('A2133251W') |> select(date, value) |> 
  rename(population = 'value')

#home repayments per capita

repayments_pc <- repayments |> 
  left_join(population, join_by(closest(date >= date))) |> 
  mutate('date.y' = NULL) |> na.omit()

repayments_pc <- repayments_pc |> mutate(interest_charged = (interest_charged * 1000000) / (population * 1000),
                        scheduled_payment = (scheduled_payment * 1000000) / (population * 1000),
                        excess_payment = (excess_payment * 1000000) / (population * 1000))

repayments_pc |> write_excel_csv('repayments_pc.csv')

#repayments as a share of income

burden <- read_xlsx('e13hist.xlsx', skip = 10) |> 
  rename(date = 'Series ID',
         interest_charged = 'LPHTICRI',
         scheduled_payment = 'LPHTSPRI',
         excess_payment = 'LPHTEXRI') |> select(date,
                                              interest_charged,
                                              scheduled_payment,
                                              excess_payment) |> na.omit()

burden |> write_excel_csv('burden.csv')
