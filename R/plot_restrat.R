#' Plot restratification results
#'
#' @description
#' Function that plots time-series of orginal indices (with 95% CIs) and restratified indices (for both biomass and numbers)
#' and percent difference between original and restratified. Stores plots in 'plots' folder
#'
#' @param index list of computed indices, obtained by running the get_index() function
#' @param species species code for which plots will be computed (can also be a vector for stock complexes
#' @param stock_name descriptive name for stock, will be included with title and also in plot filename that saved
#'
#' @return plots with time-series and percent difference
#'
#' @export
#'
plot_restrat <- function(index, species, stock_name) {

  # start plots folder
  if(!dir.exists(here::here('figs'))){
    dir.create(here::here('figs'))
  }

  # get data prepped
  index$og_indx_reg %>%
    tidytable::drop_na() %>%
    tidytable::filter(species_code %in% species) %>%
    tidytable::left_join(index$new_indx_reg %>%
                           tidytable::filter(species_code %in% species) %>%
                           tidytable::select(year, region, species_code, biom_new = biomass_mt, popn_new = population_count)) %>%
    tidytable::summarise(biomass_mt = sum(biomass_mt),
                         biomass_var = sum(biomass_var),
                         population_count = sum(population_count),
                         population_var = sum(population_var),
                         biom_new = sum(biom_new),
                         popn_new = sum(popn_new),
                         .by = c(year, region)) %>%
    tidytable::mutate(diff_biom = 100 * (biom_new - biomass_mt) / biomass_mt,
                      diff_popn = 100 * (popn_new - population_count) / population_count,
                      biom_test = tidytable::case_when(biom_new > biomass_mt + 1.96 * sqrt(biomass_var) ~ 1,
                                            biom_new < biomass_mt - 1.96 * sqrt(biomass_var) ~ 1,
                                            .default = 0),
                      popn_test = tidytable::case_when(popn_new > population_count + 1.96 * sqrt(population_var) ~ 1,
                                            popn_new < population_count - 1.96 * sqrt(population_var) ~ 1,
                                            .default = 0),
                      # cause i just like west to the left/top and east to the right/bottom
                      region = factor(region, levels = c("Western GOA", "Central GOA", "Eastern GOA", "GOA"))) -> db

  # biomass timeseries plot
  biomass <- db %>%
    tidytable::mutate(og_uci = biomass_mt + 1.96 * sqrt(biomass_var),
                      og_lci = tidytable::case_when(biomass_mt - 1.96 * sqrt(biomass_var) > 0 ~ biomass_mt - 1.96 * sqrt(biomass_var),
                                         biomass_mt - 1.96 * sqrt(biomass_var) < 0 ~ 0)) %>%
    tidytable::select(year, region, Original = biomass_mt, Restratified = biom_new, uci = og_uci, lci = og_lci) %>%
    tidytable::pivot_longer(., cols = c(Original, Restratified)) %>%
    tidytable::mutate(uci = tidytable::case_when(name == 'Original' ~ uci,
                                      name == 'Restratified' ~ value),
                      lci = tidytable::case_when(name == 'Original' ~ lci,
                                      name == 'Restratified' ~ value)) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = value, colour = name)) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2::geom_point() +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lci, ymax = uci), linewidth = 0.75) +
    ggplot2::facet_wrap(~region, ncol = 1, scales = 'free_y') +
    afscassess::theme_report() +
    ggplot2::labs(x = "Year", y = "Survey biomass (mt)", colour = "Index type", title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma') +
    tickr::scale_x_tickr(data=db, var = year) +
    ggplot2::scale_y_continuous(labels = scales::comma)

  ggplot2::ggsave(filename = here::here('figs', paste0(stock_name, '_biom_ts.png')),
         plot = biomass)

  # numbers timeseries plot
  numbers <- db %>%
    tidytable::mutate(og_uci = population_count + 1.96 * sqrt(population_var),
                      og_lci = tidytable::case_when(population_count - 1.96 * sqrt(population_var) > 0 ~ population_count - 1.96 * sqrt(population_var),
                                         population_count - 1.96 * sqrt(population_var) < 0 ~ 0)) %>%
    tidytable::select(year, region, Original = population_count, Restratified = popn_new, uci = og_uci, lci = og_lci) %>%
    tidytable::pivot_longer(., cols = c(Original, Restratified)) %>%
    tidytable::mutate(uci = tidytable::case_when(name == 'Original' ~ uci,
                                      name == 'Restratified' ~ value),
                      lci = tidytable::case_when(name == 'Original' ~ lci,
                                      name == 'Restratified' ~ value)) %>%
    ggplot2::ggplot(ggplot2::aes(x = year, y = value, colour = name)) +
    ggplot2::geom_line(alpha = 0.5) +
    ggplot2:: geom_point() +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lci, ymax = uci), linewidth = 0.75) +
    ggplot2::facet_wrap(~region, ncol = 1, scales = 'free_y') +
    afscassess::theme_report() +
    ggplot2::labs(x = "Year", y = "Survey numbers", colour = "Index type", title = stock_name) +
    scico::scale_color_scico_d(palette = 'roma') +
    tickr::scale_x_tickr(data=db, var = year) +
    ggplot2::scale_y_continuous(labels = scales::comma)

  ggplot2::ggsave(filename = here::here('figs', paste0(stock_name, '_num_ts.png')),
         plot = numbers)

  # % difference in biomass plot
  db %>%
    tidytable::select(year, region, diff_biom, biom_test) %>%
    tidytable::mutate(perc_diff = scales::percent(diff_biom / 100, accuracy = 1, trim = FALSE),
                      pjust = tidytable::case_when(diff_biom > 0 ~ -0.5,
                                        diff_biom < 0 ~ 1),
                      biom_test = tidytable::case_when(biom_test == 0 ~ "plain",
                                            biom_test == 1 ~ "bold")) -> plot_dat

  biomass_diff <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = year, y = diff_biom, fill = region)) +
    ggplot2::geom_bar(stat = "identity", show.legend=FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = perc_diff, vjust = pjust), size=3) +
    ggplot2::facet_wrap(~region, ncol = 1, scales = 'free_y') +
    ggplot2::ylim(floor(1.2 * min(plot_dat$diff_biom)), ceiling(1.2 * max(plot_dat$diff_biom))) +
    ggplot2::geom_hline(yintercept = 0) +
    scico::scale_fill_scico_d(palette = 'roma') +
    afscassess::theme_report() +
    tickr::scale_x_tickr(data=db, var = year) +
    ggplot2::labs(x = "Year", y = "Percent difference in biomass after restratification", fill = "Region", title = stock_name)

  ggplot2::ggsave(filename = here::here('figs', paste0(stock_name, '_biom_diff.png')),
         plot = biomass_diff)

  # % difference in numbers plot
  db %>%
    tidytable::select(year, region, diff_popn, popn_test) %>%
    tidytable::mutate(perc_diff = scales::percent(diff_popn / 100, accuracy = 1, trim = FALSE),
                      pjust =  tidytable::case_when(diff_popn > 0 ~ -0.5,
                                        diff_popn < 0 ~ 1),
                      biom_test =  tidytable::case_when(popn_test == 0 ~ "plain",
                                            popn_test == 1 ~ "bold")) -> plot_dat

  numbers_diff <- ggplot2::ggplot(plot_dat, ggplot2::aes(x = year, y = diff_popn, fill = region)) +
    ggplot2::geom_bar(stat = "identity", show.legend=FALSE) +
    ggplot2::geom_text(ggplot2::aes(label = perc_diff, vjust = pjust), size=3) +
    ggplot2::facet_wrap(~region, ncol = 1, scales = 'free_y') +
    ggplot2::ylim(floor(1.2 * min(plot_dat$diff_popn)), ceiling(1.2 * max(plot_dat$diff_popn))) +
    ggplot2::geom_hline(yintercept = 0) +
    scico::scale_fill_scico_d(palette = 'roma') +
    afscassess::theme_report() +
    tickr::scale_x_tickr(data=db, var = year) +
    ggplot2::labs(x = "Year", y = "Percent difference in numbers after restratification", fill = "Region", title = stock_name)

  ggplot2::ggsave(filename = here::here('figs', paste0(stock_name, '_num_diff.png')),
         plot = numbers_diff)

  # return plot output
  list(biomass = biomass,
       numbers = numbers,
       biomass_diff = biomass_diff,
       numbers_diff = numbers_diff)

}
