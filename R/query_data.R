#' Query data from GOA survey
#'
#' @description
#' Function that connects to AKFIN database (using afscdata package) and pulls catch per-unit-effort (cpue, in numbers and weight),
#' survey strata (strata), haul information (haul), design-based indices (index), species information (species_names),
#' and if desired length frequency (lfreq) and age-length specimen (specimen) data from the GAP_PRODUCTS tables
#'
#' @param species species_codes, i.e., c(10110, 21740)
#' @param pull_comp boolean, if TRUE then pull necessary data to compute age/length composition (default = FALSE)
#'
#' @return a list of necessary data sources, dataframes are also written to csv files within the 'data' folder.
#'
#' @export
#' @examples
#' \dontrun{
#' query_data(species = c(10110, 21740), pull_comp=TRUE)
#' }
#'
query_data <- function(species, pull_comp = FALSE) {

  if(!dir.exists(here::here("data"))) {
    dir.create(here::here('data'))
  }
  # connect to database ----
  # get connected to akfin
  conn = afscdata::connect()

  # cpue data ----

  cat(paste0("pulling cpue...\n"))
  # get gap_products cpue
  # dplyr::tbl(conn, dbplyr::in_schema("GAP_PRODUCTS", "AKFIN_HAUL"))

  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>%
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>%
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cpue')),
                      by = c('HAULJOIN')) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(survey_definition_id == 47,
                      species_code %in% species,
                      year >= 1990) %>%
    dplyr::collect() %>%
    tidytable::select(year,
                  species_code,
                  stratum,
                  hauljoin,
                  survey = survey_definition_id,
                  numcpue = cpue_nokm2,
                  wtcpue = cpue_kgkm2,
                  lat_st = latitude_dd_start,
                  long_st = longitude_dd_start) %>%
    tidytable::as_tidytable() -> cpue

  # get gap_products catch
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>%
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                      by = c('CRUISEJOIN')) %>%
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_catch')),
                      by = c('HAULJOIN')) %>%
    dplyr::rename_with(tolower) %>%
    dplyr::filter(survey_definition_id == 47,
                      species_code %in% species,
                      year >= 1990) %>%
    dplyr::collect() %>%
    tidytable::mutate(numcpue = count / (distance_fished_km * (0.001 * net_width_m)),
                  wtcpue = weight_kg / (distance_fished_km * (0.001 * net_width_m))) %>%
    tidytable::select(year,
                  survey = survey_definition_id,
                  species_code,
                  stratum,
                  hauljoin,
                  lat_st = latitude_dd_start,
                  long_st = longitude_dd_start,
                  numcpue,
                  wtcpue) %>%
    tidytable::as_tidytable() -> cpue_calc

  # filling in 0's like gapindex and write cpue data
  cpue %>%
    tidytable::complete(hauljoin, species_code) %>%
    tidytable::left_join(cpue_calc) %>%
    tidytable::replace_na(list(numcpue = 0, wtcpue = 0)) %>%
    vroom::vroom_write(here::here('data', "cpue.csv"), delim = ',') -> cpue

  # strata data ----

  cat(paste0("pulling strata...\n"))

  # strata with area sizes
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(survey_definition_id == 47,
                  area_type == 'STRATUM') %>%
    dplyr::select(survey = survey_definition_id,
                  design_year,
                  stratum = area_id,
                  area = area_km2) %>%
    dplyr::collect() %>%
    tidytable::as_tidytable() -> st_area

  # subregion level with description (e.g., wgoa, etc)
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_area')) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(survey_definition_id == 47,
                  area_type == 'REGULATORY AREA') %>%
    dplyr::select(area_id,
                  subarea_name = description,
                  design_year) %>%
    dplyr::collect() %>%
    tidytable::as_tidytable() -> subreg

  # strata within subregions
  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_stratum_groups')) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(survey_definition_id == 47) %>%
    dplyr::select(stratum,
                  area_id,
                  design_year) %>%
    dplyr::collect() %>%
    tidytable::as_tidytable() -> st_subreg

  # join all to get strata with area sizes and subregion ids
  st_area %>%
    tidytable::left_join(st_subreg %>%
                           tidytable::left_join(subreg) %>%
                           tidytable::drop_na()) %>%
    vroom::vroom_write(here::here('data', "strata.csv"), delim = ',') -> strata

  # haul data ----
  # note: got this query from zack

  cat(paste0("pulling haul info...\n"))

  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')) %>%
    dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')),
                      by = c('CRUISEJOIN')) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(survey_definition_id == 47) %>%
    dplyr::select(year,
                  survey = survey_definition_id,
                  stratum,
                  hauljoin,
                  lat_st = latitude_dd_start,
                  long_st = longitude_dd_start) %>%
    dplyr::collect() %>%
    tidytable::as_tidytable() %>%
    vroom::vroom_write(here::here('data', "haul.csv"), delim = ',') -> haul

  # computed index data ----

  cat(paste0("pulling index data...\n"))

  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_biomass')) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(survey_definition_id == 47,
                  area_id == 99903,
                  year >= 1990,
                  species_code %in% species) %>%
    dplyr::select(year, area_id, species_code, biomass_mt, biomass_var, population_count, population_var) %>%
    dplyr::collect() %>%
    tidytable::as_tidytable() %>%
    vroom::vroom_write(here::here('data', "index.csv"), delim = ',') -> index

  # species names ----

  cat(paste0("pulling species info...\n"))

  dplyr::tbl(conn, dplyr::sql('gap_products.akfin_taxonomic_classification')) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::filter(species_code %in% species) %>%
    dplyr::select(species_code,
                  species_name,
                  common_name) %>%
    dplyr::collect() %>%
    tidytable::as_tidytable() %>%
    vroom::vroom_write(here::here('data', "species_names.csv"),
                       delim = ',') -> species_names


  if(isTRUE(pull_comp)){
    # length frequency data ----

    cat(paste0("pulling length frequency...\n"))

    dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>%
      dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                        by = c('CRUISEJOIN')) %>%
      dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_length')),
                        by = c('HAULJOIN')) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::filter(survey_definition_id == 47,
                    species_code %in% species,
                    year >= 1990) %>%
      dplyr::select(year,
                    survey = survey_definition_id,
                    species_code,
                    stratum,
                    hauljoin,
                    sex,
                    length = length_mm,
                    frequency,
                    lat_st = latitude_dd_start,
                    long_st = longitude_dd_start) %>%
      dplyr::collect() %>%
      tidytable::as_tidytable() %>%
      vroom::vroom_write(here::here('data', "lfreq.csv"),
                         delim = ',') -> lfreq

    # specimen data ----

    cat(paste0("pulling specimen...\n"))

    dplyr::tbl(conn, dplyr::sql('gap_products.akfin_haul')) %>%
      dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_cruise')),
                        by = c('CRUISEJOIN')) %>%
      dplyr::inner_join(dplyr::tbl(conn, dplyr::sql('gap_products.akfin_specimen')),
                        by = c('HAULJOIN')) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::filter(survey_definition_id == 47,
                    species_code %in% species,
                    year >= 1990) %>%
      dplyr::select(year,
                    survey = survey_definition_id,
                    species_code,
                    stratum,
                    hauljoin,
                    sex,
                    length = length_mm,
                    age,
                    lat_st = latitude_dd_start,
                    long_st = longitude_dd_start) %>%
      dplyr::collect() %>%
      tidytable::as_tidytable() %>%
      vroom::vroom_write(here::here('data', "specimen.csv"),
                         delim = ',') -> specimen
  }

  afscdata::disconnect(conn)
  cat("finished.\n")

  data = list(cpue = cpue,
              strata = strata,
              haul = haul,
              index = index)

  if(isTRUE(pull_comp)){
    data$lfreq = lfreq
    data$specimen = specimen
  }
  saveRDS(data, here::here('data', 'data.rds'))

  data
}


