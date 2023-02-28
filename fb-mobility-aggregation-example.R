# Calculate basic statistics for movement vectors
library(sf)
library(tidyverse)
library(here)

rm(list=ls())

in_dir <- "fb-mobility"

start_date <- c("2022-08-13")
end_date <- c("2022-09-27")

date_list <- seq(as.Date(start_date), as.Date(end_date), by="days")
date_list_match <- str_replace_all(date_list,"-","")

# Search for mobility data files
files <- list.files(path = here(in_dir),pattern =
                      paste(date_list_match,collapse ="|"),
                    recursive = TRUE)

if(length(files) == 0){
  print(paste0("No files found under",curr_dir))}else{
  df <- here(in_dir,files) %>%
    map(read_csv) %>%
    reduce(rbind)

  df <- df %>%
    mutate(vector_name = paste(start_polygon_name,
                               end_polygon_name,sep=" - "))

  # Calculate summary statistics for level 1 admin units

  df_sum <- df %>%
    group_by(vector_key) %>%
    summarise(start_lat = first(start_lat),
              start_lon = first(start_lon),
              end_lat = first(end_lat),
              end_lon = first(end_lon),
              length_km = first(length_km),
              start_polygon_id = first(start_polygon_id),
              end_polygon_id = first(end_polygon_id),
              start_polygon_name = first(start_polygon_name),
              end_polygon_name = first(end_polygon_name),
              country = first(country),
              n_baseline = first(n_baseline),
              ndiff_mean = mean(n_difference,na.rm = TRUE),
              ndiff_med = median(n_difference,na.rm = TRUE),
              ndiff_min = min(n_difference,na.rm = TRUE),
              ndiff_max = max(n_difference,na.rm = TRUE),
              ndiff_std = sd(n_difference,na.rm = TRUE),
              abs_ndiff_med = abs(ndiff_med),
              pct_chg_mean = mean(percent_change,na.rm = TRUE),
              pct_chg_med = median(percent_change,na.rm = TRUE),
              pct_chg_min = min(percent_change,na.rm = TRUE),
              pct_chg_max = max(percent_change,na.rm = TRUE),
              pct_chg_std = sd(percent_change,na.rm = TRUE),
              abs_pct_chg_med = abs(pct_chg_med),
              vector_name = paste(start_polygon_name,
                                  end_polygon_name,sep=" - "))

  # df_sum <- df_sum[complete.cases(df_sum), ]

  out_name <- paste0("fb_mobility_admin_summary_",date_list_match[1],"_",
                     date_list_match[length(date_list_match)],".csv")

  out_dir <- "fb-mobility-agg"
  if(!dir.exists(out_dir)){
    dir.create(here(out_dir),recursive = TRUE)
  }

  write.csv(df_sum,here(out_dir,out_name),row.names = FALSE,na = "")
}




