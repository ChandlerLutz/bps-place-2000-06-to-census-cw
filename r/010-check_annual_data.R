## r/010-check_annual_data.R

##Clear the workspace
rm(list = ls()) 
suppressWarnings(CLmisc::detach_all_packages())

##Set wd using the here package
setwd(here::here("r/"))

suppressPackageStartupMessages({library(CLmisc); })

##Load the data
dt_all <- read_fst(here::here("data/bps_place_2000_06.fst"), as.data.table = TRUE)

## -- Check for problem `bps_id`s -- ##

dup_state_census_place_year_ids <- dt_all %>%
  .[!is.na(census_place_code) & survey_date <= "2007"] %>% 
  .[, id := paste(state_code, census_place_code, survey_date, sep = "_")] %>%
  .[duplicated(id), .(state_code, census_place_code)] %>%
  .[!duplicated(paste0(state_code, census_place_code))]

potential_prob_bps_ids <- dt_all %>%
  .[survey_date <= "2007"] %>%
  .[, num_census_fips := uniqueN(census_place_code), by = bps_id] %>%
  .[num_census_fips > 1, unique(bps_id)]

dt_bps_ids_with_more_than_one_place_name <- dt_all %>%
  .[survey_date <= "2007" & bps_id %in% potential_prob_bps_ids] %>%
  .[, `:=`(unique_st_obs = uniqueN(state_code), unique_name_obs = uniqueN(place_name)),
    by = bps_id] %>%
  .[unique_st_obs > 1 | unique_name_obs > 1] %>% 
  .[, is_dup_place_name := duplicated(place_name), by = bps_id] %>%
  .[is_dup_place_name == FALSE] %>% 
  .[, name_number := rleid(place_name), by = bps_id] %>% 
  dcast(bps_id ~ name_number, value.var = "place_name")
      
## Manual check to make sure that the `bps_id`s with more than one place name
## are not actually different places
## fwrite(
##   dt_bps_ids_with_more_than_one_place_name,
##   "030-dt_bps_ids_with_more_than_one_place_name.csv"
## )

dt_tst <- dt_all %>%
  .[survey_date <= "2007" & !is.na(census_place_code)] %>% 
  ## Remove places where the bps_id stayed the same but the census_place_code changed
  .[bps_id %notin% potential_prob_bps_ids] %>%
  .[order(bps_id, survey_date)] %>%
  .[paste0(state_code, census_place_code) %notin%
      dup_state_census_place_year_ids[, paste0(state_code, census_place_code)]] %>%
  .[, `:=`(grp_bps_id = .GRP, obs_by_bps_id = .N), by = .(bps_id)] %>%
  .[, `:=`(grp_state_census_place_id = .GRP, obs_by_state_census_place_id = .N),
    by = .(state_code, census_place_code)] %>%
  .[grp_bps_id != grp_state_census_place_id]

if (nrow(dt_tst) > 0) {
  stop("There are non-unique state-census place code pairs outside of those identified in `potential_prob_bps_ids` and `dup_state_census_place_year_ids`.")
}

f_gsub_remove <- function(pattern, x) {
  gsub(pattern = pattern, replacement = "", x = x, ignore.case = TRUE)
}

## Check place names differences again within bps ids for survey_date <= 2007
dt_all_tst_place_names <- dt_all %>%
  .[survey_date <= "2007"] %>%
  .[, place_name_cleaned := place_name] %>%
  .[, place_name_cleaned := f_gsub_remove("@1|@2|@3|@4|@5", place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove(" township$", place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove(" town$", place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove(" part unincorporated area$",
                                          place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove(" unincorporated area$",
                                          place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove(" city$", place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove(" village$", place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove(" borough$", place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove(" charter$", place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove("\'", place_name_cleaned)] %>%
  .[, place_name_cleaned := f_gsub_remove("-", place_name_cleaned)] %>%
  .[, place_name_cleaned := tolower(place_name_cleaned)] %>%
  .[, place_name_cleaned := gsub(" ", "", place_name_cleaned)] %>%
  .[, num_names := uniqueN(place_name_cleaned), by = bps_id] %>%
  .[num_names > 1] %>%
  .[!duplicated(paste0(bps_id, place_name_cleaned)),
    .(bps_id, place_name, place_name_cleaned)]

## No major differences in place names within bps ids
print(dt_all_tst_place_names)

