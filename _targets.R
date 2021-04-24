library(targets)
library(tarchetypes)
library(tibble)

# General variables
csl <- "pandoc/csl/apa.csl"
bibstyle <- "bibstyle-apa"

options(tidyverse.quiet = TRUE,
        dplyr.summarise.inform = FALSE)

set.seed(7305)  # From random.org

tar_option_set(packages = c("tidyverse", "here", "fs", "scales", "withr"))

source("R/funs_data-cleaning.R")
source("R/funs_details.R")
# source("R/funs_notebook.R")

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}

# Pipeline
list(
  # Define helper functions
  tar_target(plot_funs, here_rel("R", "graphics.R"), format = "file"),
  tar_target(misc_funs, here_rel("R", "misc.R"), format = "file"),
  
  # Define raw data files
  tar_target(chaudhry_raw_file,
             here_rel("data", "raw_data", "Chaudhry restrictions", "SC_Expanded.dta"),
             format = "file"),
  tar_target(aiddata_raw_file, 
             get_aiddata(
               aiddata_url = "https://github.com/AidData-WM/public_datasets/releases/download/v3.1/AidDataCore_ResearchRelease_Level1_v3.1.zip",
               out_dir = here_rel("data", "raw_data", "AidData"),
               final_name = "AidDataCoreDonorRecipientYearPurpose_ResearchRelease_Level1_v3.1.csv"),
             format = "file"),
  # Current list is at https://webfs.oecd.org/crs-iati-xml/Lookup/DAC-CRS-CODES.xml
  # but the XML structure has changed and it's trickier to identify all the codes
  # systematically now. So instead we use a version from 2016:
  tar_target(dac_purposes_raw_file,
             get_dac_purposes(
               purposes_url = "https://web.archive.org/web/20160819123535/https://www.oecd.org/dac/stats/documentupload/DAC_codeLists.xml",
               out_dir = here_rel("data", "raw_data", "DAC CRS codes")),
             format = "file"),
  tar_target(dac_eligible_raw_file,
             here_rel("data", "manual_data", "oecd_dac_countries.csv")),
  tar_target(usaid_raw_file,
             get_usaid(
               usaid_url = "https://explorer.usaid.gov/prepared/us_foreign_aid_complete.csv",
               out_dir = here_rel("data", "raw_data", "USAID")),
             format = "file"),
  tar_target(dcjw_raw_file, 
             here_rel(here("data", "raw_data", 
                           "DCJW NGO laws", "DCJW_NGO_Laws.xlsx")),
             format = "file"),
  tar_target(vdem_raw_file,
             here_rel("data", "raw_data", "Country_Year_V-Dem_Full+others_R_v10",
                      "V-Dem-CY-Full+Others-v10.rds"),
             format = "file"),
  tar_target(un_pop_raw_file,
             here_rel("data", "raw_data", "UN data",
                      "WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"),
             format = "file"),
  tar_target(un_gdp_constant_raw_file,
             here_rel("data", "raw_data", "UN data",
                      "UNdata_Export_20210118_034054729.csv"),
             format = "file"),
  tar_target(un_gdp_current_raw_file,
             here_rel("data", "raw_data", "UN data",
                      "UNdata_Export_20210118_034311252.csv"),
             format = "file"),
  tar_target(ucdp_raw_file,
             here_rel("data", "raw_data", "UCDP PRIO", "ucdp-prio-acd-191.csv"),
             format = "file"),
  tar_target(disasters_raw_file,
             here_rel("data", "raw_data", "Disasters",
                      "emdat_public_2021_01_16_query_uid-ufBbE2.xlsx"),
             format = "file"),
  tar_target(naturalearth_raw_file,
             here_rel("data", "raw_data", "ne_110m_admin_0_countries",
                      "ne_110m_admin_0_countries.shp"),
             format = "file"),
  tar_target(civicus_raw_file,
             here_rel("data", "raw_data", "Civicus", "civicus_2021-03-19.json"),
             format = "file"),
  
  # Process and clean data
  tar_target(chaudhry_raw, load_chaudhry_raw(chaudhry_raw_file)),
  tar_target(democracies, create_consolidated_democracies()),
  tar_target(skeleton, create_panel_skeleton(democracies, chaudhry_raw)),
  tar_target(regulations, create_regulation_lookup()),
  
  tar_target(aiddata_clean, clean_aiddata(aiddata_raw_file)),
  tar_target(aid_donors, build_aid_donors(aiddata_clean)),
  tar_target(aid_recipients, build_aid_recipients(aiddata_clean, skeleton)),
  tar_target(aid_purposes, build_aid_purposes(aiddata_clean)),
  tar_target(aid_purposes_manual, 
             build_aid_purposes_manual(
               dac_purposes_raw_file, 
               here_rel("data", "manual_data",
                        "purpose_codes_contention_WILL_BE_OVERWRITTEN.csv")),
             format = "file"),
  tar_target(aid_purposes_manual_edited, 
             here_rel("data", "manual_data",
                      "purpose_codes_contention.csv")),
  tar_target(aid_purpose_codes_contentiousness, 
             build_aid_contentiousness(aid_purposes_manual_edited)),
  tar_target(aiddata_final, 
             build_aiddata_final(aiddata = aiddata_clean, 
                                 donors = aid_donors, 
                                 recipients = aid_recipients,
                                 purpose_codes = aid_purpose_codes_contentiousness,
                                 skeleton = skeleton,
                                 dac_eligible_raw = dac_eligible_raw_file)),
  tar_target(donor_level_data, build_donor_aiddata(aiddata_final, skeleton)),
  tar_target(usaid_clean, clean_usaid(usaid_raw_file, skeleton)),
  tar_target(donor_level_data_usaid, fix_inflation_usaid(usaid_clean, skeleton)),
  tar_target(usaid_by_country_total, 
             build_usaid_by_country_total(donor_level_data_usaid)),
  tar_target(usaid_by_country_channel, 
             build_usaid_by_country_channel(donor_level_data_usaid)),
  
  tar_target(dcjw_clean, load_clean_dcjw(dcjw_raw_file, regulations)),
  tar_target(chaudhry_clean, load_clean_chaudhry(chaudhry_raw, regulations)),
  tar_target(vdem_clean, load_clean_vdem(vdem_raw_file)),
  tar_target(autocracies, build_autocracies(vdem_clean, skeleton)),
  tar_target(wdi_clean, load_clean_wdi(skeleton)),
  tar_target(un_pop, load_clean_un_pop(un_pop_raw_file, skeleton, wdi_clean)),
  tar_target(un_gdp, load_clean_un_gdp(un_gdp_constant_raw_file,
                                       un_gdp_current_raw_file, skeleton)),
  tar_target(ucdp_prio_clean, load_clean_ucdp(ucdp_raw_file)),
  tar_target(disasters_summarized, load_clean_disasters(disasters_raw_file, 
                                                        skeleton)),
  
  tar_target(country_aid, 
             build_country_data(skeleton, chaudhry_clean, vdem_clean,
                                ucdp_prio_clean, disasters_summarized,
                                aiddata_final, democracies, un_gdp, un_pop,
                                donor_level_data, usaid_by_country_total,
                                usaid_by_country_channel)),
  tar_target(country_aid_complete, fix_country_data(country_aid)),
  tar_target(panel_with_extra_years, make_final_data(country_aid_complete)),
  tar_target(panel_lagged_extra_years, lag_data(panel_with_extra_years)),
  tar_target(country_aid_no_lags, trim_data(panel_with_extra_years)),
  tar_target(country_aid_final, trim_data(panel_lagged_extra_years)),
  
  tar_target(world_map, load_world_map(naturalearth_raw_file)),
  tar_target(civicus_clean, load_clean_civicus(civicus_raw_file)),
  tar_target(civicus_map_data, create_civicus_map_data(civicus_clean, world_map)),
  
  tar_target(var_details, create_vars_table()),
  tar_target(ngo_index_table, create_ngo_index_table())
)
