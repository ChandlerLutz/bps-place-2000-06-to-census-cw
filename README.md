# Building Permits Survey (BPS) 2000--06 Census Crosswalk

## Annual BPS Data
- Create a `bps_id` columns by `paste0(state_code, "_", x6_digit_id)`
- For 2000-06, fill the `census_place_code` variable with `place_code` to reconcile the data before and after 2007: 
```r
dt[is.na(census_place_code) & !is.na(place_code) & survey_date <= "2006",
   census_place_code := place_code]
```
- There is likely a mistake in the Nelson, GA `bps_id`. Correct with the following data.table code: 
```r
dt[survey_date == "2002" & state_code == "13" & place_name == "Nelson"
   & census_place_code == "2000",
   `:=`(bps_id = "13_486000", cnty_fips = "13227", county_code = "057")]
```
- By `bps_id`, backfill the missing variables: `census_place_code`, `fips_place_code`, `fips_mcd_code`:
```r
dt <- dt %>%
  .[survey_date <= "2007", 
    census_place_code_2007 := .SD[survey_date == "2007", census_place_code],
    by = bps_id] %>%
  .[survey_date < "2007" & is.na(census_place_code),
    census_place_code := census_place_code_2007] %>%
  .[survey_date <= "2007",
    fips_place_code_2007 := .SD[survey_date == "2007", fips_place_code], 
    by = bps_id] %>%
  .[survey_date < "2007" & is.na(fips_place_code),
    fips_place_code := fips_place_code_2007] %>%
  .[survey_date <= "2007",
    fips_mcd_code_2007 := .SD[survey_date == "2007", fips_mcd_code],
    by = bps_id] %>%
  .[survey_date < "2007" & is.na(fips_mcd_code),
    fips_mcd_code := fips_mcd_code_2007] %>%
  .[, c("census_place_code_2007", "fips_place_code_2007", "fips_mcd_code_2007") := NULL]

```



