#' Reshape confirmed case data for producing Tableau extracts
#'
#' take case data (from WEDSS or historical data table) and put it in proper
#' shape for metric tables to feed tableau
#'
#' @param case_df Confirmed case data.frame (e.g. produced by \link{pull_histTable})
#' @param weeks Number of weeks of data (default 2)
#'
#' @return a data.frame with the following with one row per county, state, and
#' HERC regions with the following columns
#' \describe{
#'   \item{fips}{FIPS Code and/or region identifier}
#'   \item{geo_name}{Name of geography}
#'   \item{pop_2018}{2018 Population Numbers pulled from WISH}
#'   \item{case_weekly_1}{Total cases for \strong{current} 7 day period}
#'   \item{case_weekly_2}{Total cases for \strong{prior} 7 day period}
#'   \item{week_end_1}{End date for \strong{current} 7 day period}
#'   \item{week_end_2}{End date for \strong{prior} 7 day period}
#' }
#'
#' @export
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#' @importFrom dplyr summarize_at
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#'
#' @examples
#' \dontrun{
#'   hdt <- pull_histTable()
#'   hdt_clean <- shape_case_data(hdt)
#' }
shape_data <- function(case_df, weeks = 2) {
  stopifnot(weeks > 1)
  
  max_date <- max(case_df$post_date)
  
  case_df %>%
    dplyr::group_by(fips, geo_name, pop_2018) %>%
    dplyr::mutate(
      weeknum = rolling_week(date_vector = post_date, end_date = max_date),
      pos = case_daily,
      total = test_daily
    ) %>%
    tidyr::pivot_longer(case_daily:death_daily,
                        names_to = "count_type",
                        values_to = "daily") %>%
    dplyr::mutate(count_type = sub("_daily", "", count_type)) %>%
    dplyr::group_by(fips, geo_type, geo_name, pop_2018, weeknum, count_type) %>%
    dplyr::summarize(
      weekly = as.integer(sum(daily)),
      week_end = max(post_date),
      pos = sum(pos, na.rm = TRUE),
      total = sum(total, na.rm = TRUE),
      pct_pos = round(ifelse(is.na(total) | total == 0, NA,
                             100 * pos / total), 2),
      .groups = "drop_last"
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(weeknum <= weeks) %>%
    dplyr::select(fips, geo_type, geo_name, pop_2018, weeknum, count_type, weekly, week_end, pct_pos) %>%
#    tidyr::pivot_wider(id_cols = c("fips", "geo_type", "geo_name", "pop_2018", "count_type"),
#                       values_from = c("weekly", "week_end", "pct_pos"),
#                       names_from = "weeknum") %>%
    dplyr::mutate(pop_2018 = 
             as.integer(
               ifelse(fips == "55",
                      sum(county_data$pop_2018),
                      pop_2018)))
}
