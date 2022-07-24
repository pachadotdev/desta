library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(usethis)

# download ----

fraw <- "data_raw/"
try(dir.create(fraw))

treaties_url <- "https://www.designoftradeagreements.org/media/filer_public/97/7f/977f7d18-2edb-4d94-ba30-16cf3ea2f824/desta_list_of_treaties_02_01.xlsx"
treaties_xlsx <- paste0(fraw, gsub(".*/", "", treaties_url))
treaties_rda <- "data/treaties.rda"

if (!file.exists(treaties_rda)) {
  try(download.file(treaties_url, treaties_xlsx))
  treaties <- read_xlsx(treaties_xlsx,
                        sheet = "data") %>%
    mutate_if(is.numeric, as.integer)
  use_data(treaties, overwrite = T)
} else {
  load("data/treaties_dyads.rda")
}

treaties_dyads_url <- "https://www.designoftradeagreements.org/media/filer_public/ab/ee/abee41ef-f5e5-44b6-91db-5e8befe48fe5/desta_list_of_treaties_02_01_dyads.xlsx"
treaties_dyads_xlsx <- paste0(fraw, gsub(".*/", "", treaties_dyads_url))
treaties_dyads_rda <- "data/treaties_dyads.rda"

if (!file.exists(treaties_dyads_rda)) {
  try(download.file(treaties_dyads_url, treaties_dyads_xlsx))
  treaties_dyads <- read_xlsx(treaties_dyads_xlsx,
                              sheet = "data") %>%
    mutate_if(is.numeric, as.integer) %>%
    drop_na(country1, country2)
  use_data(treaties_dyads, overwrite = T)
} else {
  load("data/treaties_dyads.rda")
}

withdrawals_dyads_url <- "https://www.designoftradeagreements.org/media/filer_public/77/6c/776c7757-3ae4-4dba-b2ad-b6d0f1f9c634/desta_dyadic_withdrawal_02_01.xlsx"
withdrawals_dyads_xlsx <- paste0(fraw, gsub(".*/", "", withdrawals_dyads_url))
withdrawals_dyads_rda <- "data/withdrawals_dyads.rda"

if (!file.exists(withdrawals_dyads_rda)) {
  try(download.file(withdrawals_dyads_url, withdrawals_dyads_xlsx))
  withdrawals_dyads <- read_xlsx(withdrawals_dyads_xlsx,
                                 sheet = "data") %>%
    mutate_if(is.numeric, as.integer) %>%
    drop_na(country1, country2)
  use_data(withdrawals_dyads, overwrite = T)
} else {
  load("data/withdrawals_dyads.rda")
}

# computed tables ----

treaties <- treaties_dyads %>%
  select(year, country1, country2, entry_type) %>%
  filter(entry_type == "base_treaty") %>%
  mutate(c1 = pmin(country1, country2), c2 = pmax(country1, country2)) %>%
  select(year, c1, c2) %>%
  distinct() %>%
  group_by(c1, c2) %>%
  summarise(year = min(year)) %>%
  mutate(rta = 1)

withdrawals <- withdrawals_dyads %>%
  select(year, country1, country2, entry_type) %>%
  mutate(c1 = pmin(country1, country2), c2 = pmax(country1, country2)) %>%
  ungroup() %>%
  select(year, c1, c2) %>%
  distinct() %>%
  group_by(c1, c2) %>%
  summarise(year = min(year)) %>%
  mutate(rta = -1)

valid_rta <- crossing(
  year = min(treaties$year, withdrawals$year):max(treaties$year, withdrawals$year),
  treaties %>% select(c1, c2)
  ) %>%
  left_join(
    treaties
  ) %>%
  left_join(
    withdrawals %>% rename(wrta = rta)
  )

valid_rta <- valid_rta %>%
  group_by(c1, c2) %>%
  fill(rta, .direction = "down") %>%
  fill(wrta, .direction = "down") %>%
  ungroup()

valid_rta <- valid_rta %>%
  mutate(
    rta = if_else(is.na(rta), 0, rta),
    wrta = if_else(is.na(wrta), 0, wrta)
  ) %>%
  group_by(c1,c2) %>%
  mutate(res = rta + wrta)

valid_rta <- valid_rta %>%
  ungroup() %>%
  select(-c(rta, wrta)) %>%
  rename(rta = res) %>%
  mutate_if(is.numeric, as.integer)

current_treaties_dyads <- valid_rta %>%
  rename(
    country1 = c1,
    country2 = c2,
    at_least_one_valid_treaty = rta
  )

use_data(current_treaties_dyads, overwrite = T)
