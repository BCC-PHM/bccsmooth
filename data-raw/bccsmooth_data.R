#======================================================
#build the bccsmooth datasets
#run this script whenever the source data is updated
#======================================================
library(dplyr)
library(sf)

#======================================================
#boundaries: project and repair once
#======================================================
lsoa21_boundaries = read_sf("data-raw/boundaries-lsoa-2021-birmingham.geojson") %>%
  rename(LSOA21CD = lsoa21cd) %>%
  select(LSOA21CD) %>%
  st_transform(27700) %>%
  st_make_valid()

ward21_boundaries = read_sf("data-raw/boundaries-wards-2022-birmingham.geojson") %>%
  rename(WD21CD = Ward_Code, WD21NM = Ward_Name) %>%
  select(WD21CD, WD21NM) %>%
  st_transform(27700) %>%
  st_make_valid()

#======================================================
#population estimates
#======================================================
#------------------------------------------------------
#lsoa 2021: Birmingham only
#------------------------------------------------------
lsoa21pop = readRDS("data-raw/lsoa21pop_2001_2024_bham.rds") %>%
  select(LSOA21CD, year, fin_year, age_band, count, standard_pop)

#------------------------------------------------------
#ward 2021: only the 69 current wards
#------------------------------------------------------
ward21pop = readRDS("data-raw/ward21pop_2001_2024_bham.rds") %>%
  filter(WD21CD %in% ward21_boundaries$WD21CD)

#------------------------------------------------------
#2011 to 2021 lsoa reallocation weights
#------------------------------------------------------
lsoa11_to_lsoa21 = readRDS("data-raw/lsoa11_to_lsoa21_weights.rds") %>%
  select(lsoa11, lsoa21, weight)

#------------------------------------------------------
#checks before shipping
#------------------------------------------------------
stopifnot(n_distinct(lsoa21pop$LSOA21CD) == 659)
stopifnot(nrow(lsoa21_boundaries) == 659)
stopifnot(all(st_is_valid(lsoa21_boundaries)))
stopifnot(all(lsoa21pop$LSOA21CD %in% lsoa21_boundaries$LSOA21CD))
stopifnot(n_distinct(ward21pop$WD21CD) == 69)
stopifnot(nrow(ward21_boundaries) == 69)
stopifnot(all(st_is_valid(ward21_boundaries)))

#------------------------------------------------------
#write
#------------------------------------------------------
usethis::use_data(lsoa21pop, ward21pop, lsoa11_to_lsoa21,
                  lsoa21_boundaries, ward21_boundaries,
                  overwrite = TRUE, compress = "xz")









