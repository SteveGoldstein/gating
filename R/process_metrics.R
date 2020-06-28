#' Process the shaped confirmed case data.frame into a Tableau ready format
#'
#' @param clean_df shaped data produced by \code{\link{shape_data}}
#'
#' @return a Tableau ready data.framed with the following columns:
#' \describe{
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#'   \item{}{}
#' }
#'
#' @export
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr ungroup
#' @importFrom dplyr if_else
#' @importFrom rlang .data
#'
#' @examples
#' library(dplyr)
#'
#' output <- pull_histTable() %>%
#'   shape_data() %>%
#'   process_metrics()
process_metrics <- function(clean_df) {
  dplyr::ungroup(clean_df) %>%
    dplyr::mutate(
      Count = weekly_1 + weekly_2,
      # Done uniformly across types. Could adjust by type.
      Burden = score_burden(curr = weekly_1,
                            prev = weekly_2,
                            pop = pop_2018),
      Burden_Class = class_burden(Burden),
      Trajectory = score_trajectory(curr = weekly_1,
                                    prev = weekly_2),
      Trajectory_P = pval_trajectory(curr = weekly_1,
                                     prev = weekly_2),
      Trajectory_Class = class_trajectory(traj = Trajectory,
                                          pval = Trajectory_P),
      Trajectory_FDR = fdr_trajectory(pval = Trajectory_P),
      Composite_Class = confirmed_case_composite(traj_class = Trajectory_Class,
                                                 burd_class = Burden_Class)
      
    ) %>%
    dplyr::mutate(
      Trajectory = signif(Trajectory, 2),
      Trajectory = dplyr::if_else(Trajectory_Class == "No significant change", "N/A",
                                  as.character(Trajectory)),
      Burden = signif(Burden, 2)
    ) %>%
    dplyr::select(
      Date = week_end_1,
      Region_ID = fips,
      Region = geo_name,
      Type = count_type,
      Count,
      Burden,
      Trajectory,
      Burden_Class,
      Trajectory_Class,
      Composite_Class,
      Trajectory_P,
      Trajectory_FDR
    )
}
