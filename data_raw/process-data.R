library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(usethis)

# download ----

fraw <- "data_raw/"
try(dir.create(fraw))

## treaties ----

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

## withdrawals ----

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

## content coding ----

content_coding_url <- "https://www.designoftradeagreements.org/media/filer_public/85/b2/85b2833f-ba14-42e7-8e8e-381dd4e354cd/desta_version_02_01.xlsx"
content_coding_xlsx <- paste0(fraw, gsub(".*/", "", content_coding_url))
content_coding_rda <- "data/content_coding.rda"

if (!file.exists(content_coding_rda)) {
  try(download.file(content_coding_url, content_coding_xlsx))
  content_coding <- read_xlsx(content_coding_xlsx,
                              sheet = "data") %>%
    mutate_if(is.numeric, as.integer)
  use_data(content_coding, overwrite = T)
} else {
  load("data/content_coding.rda")
}

# computed tables ----

## current Free Trade Agreements ----

treaties2 <- treaties_dyads %>%
  select(number, year, country1, country2, entry_type) %>%
  filter(entry_type == "base_treaty") %>%
  mutate(c1 = pmin(country1, country2), c2 = pmax(country1, country2)) %>%
  mutate(rta = 1) %>%
  select(year, c1, c2, rta, number)

# filter for full FTAs!!!

treaties2 <- treaties2 %>%
  inner_join(
    content_coding %>%
      select(number, mar_typedepth) %>%
      filter(mar_typedepth == 2)
  )

treaties2 %>%
  group_by(c1, c2) %>%
  count() %>%
  filter(n > 1)

withdrawals2 <- withdrawals_dyads %>%
  select(year, country1, country2, number) %>%
  mutate(c1 = pmin(country1, country2), c2 = pmax(country1, country2)) %>%
  select(year, c1, c2, number) %>%
  mutate(rta = -1)

in_force_ftas_dyads <- crossing(
  year = min(treaties2$year, withdrawals2$year):max(treaties2$year, withdrawals2$year),
  treaties2 %>% select(c1, c2)
  ) %>%
  left_join(
    treaties2
  ) %>%
  left_join(
    withdrawals2 %>% rename(wrta = rta)
  )

in_force_ftas_dyads <- in_force_ftas_dyads %>%
  group_by(c1, c2) %>%
  fill(rta, .direction = "down") %>%
  fill(wrta, .direction = "down") %>%
  ungroup()

in_force_ftas_dyads <- in_force_ftas_dyads %>%
  mutate(
    rta = if_else(is.na(rta), 0, rta),
    wrta = if_else(is.na(wrta), 0, wrta)
  ) %>%
  group_by(c1,c2) %>%
  mutate(res = rta + wrta)

in_force_ftas_dyads <- in_force_ftas_dyads %>%
  ungroup() %>%
  select(-c(rta, wrta)) %>%
  rename(rta = res) %>%
  mutate_if(is.numeric, as.integer)

in_force_ftas_dyads <- in_force_ftas_dyads %>%
  rename(
    country1 = c1,
    country2 = c2,
    in_force_fta = rta
  )

in_force_ftas_dyads %>%
  filter(in_force_fta < 0)

in_force_ftas_dyads %>%
  filter(!is.na(number)) %>%
  distinct(country1, country2, number) %>%
  group_by(country1, country2) %>%
  count() %>%
  filter(n > 1)

in_force_ftas_dyads <- in_force_ftas_dyads %>%
  select(-mar_typedepth) %>%
  group_by(country1, country2) %>%
  fill(number, .direction = "down")

in_force_ftas_dyads <- in_force_ftas_dyads %>%
  filter(!is.na(number)) %>%
  pivot_wider(names_from = "number", values_from = in_force_fta,
              names_prefix = "n") %>%
  mutate(year = as.character(year))

in_force_ftas_dyads$in_force_fta <- rowSums(Filter(is.integer, in_force_ftas_dyads), na.rm = T)

in_force_ftas_dyads <- in_force_ftas_dyads %>%
  select(year:country2, in_force_fta)

in_force_ftas_dyads %>%
  filter(in_force_fta < 0)

in_force_ftas_dyads %>%
  filter(in_force_fta > 1)

in_force_ftas_dyads <- in_force_ftas_dyads %>%
  mutate(in_force_fta = as.integer(ifelse(in_force_fta > 1, 1, in_force_fta)))

in_force_ftas_dyads <- crossing(
  year = min(in_force_ftas_dyads$year):max(in_force_ftas_dyads$year),
  in_force_ftas_dyads %>%
    select(country1, country2)
) %>%
  left_join(in_force_ftas_dyads %>% mutate(year = as.numeric(year))) %>%
  mutate(in_force_fta = as.integer(if_else(is.na(in_force_fta), 0L, in_force_fta)))

use_data(in_force_ftas_dyads, overwrite = T)
