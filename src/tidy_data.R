#' @title Summarize site parameter data from the API and field notes data frames.
#' @description A function that summarizes and joins site parameter data from the API with the field notes data frames.
#' @param site_arg A site name.
#' @param parameter_arg A parameter name.
#' @param api_data A dataframe with the munged API data.
#' @param summarize_interval At what time interval the user would like the data set to be aggregated and rounded to. Default is 15 minutes
#' @return A dataframe with summary statistics for a given site parameter data frame

tidy_data <- function(site_arg, parameter_arg, api_data, summarize_interval = "15 minutes") {

  if(grepl("minutes", summarize_interval)){
    summarize_interval <- gsub("minutes", "mins", summarize_interval, ignore.case = TRUE)
  }

  # filtering the data and generating results
  summary <- tryCatch({
    api_data %>%
      # subset to single site-parameter combo:
      dplyr::filter(site == site_arg & parameter == parameter_arg) %>%
      # safety step of removing any erroneous dupes
      dplyr::distinct() %>%
      # across each 15 timestep, get the average value, spread, and count of obs
      dplyr::group_by(DT_round, site, parameter) %>%
      dplyr::summarize(mean = as.numeric(mean(value, na.rm = T)),
                       diff = abs(min(value, na.rm = T) - max(value, na.rm = T)),
                       n_obs = n()) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(DT_round) %>%
      # pad the dataset so that all user-selected interval time stamps are present
      padr::pad(by = "DT_round", interval = summarize_interval) %>%
      # add a DT_join column to join field notes to (make DT_round character string, so no
      # funky DT issues occur during the join):
      dplyr::mutate(DT_join = as.character(DT_round),
                    site = site_arg,
                    parameter = parameter_arg,
                    flag = NA) %>% # add "flag" column for future processing
      # make sure DT_join is still correct:
      dplyr::mutate(DT_round = lubridate::as_datetime(DT_join, tz = "MST")) %>%
      dplyr::distinct(.keep_all = TRUE)

  },

  error = function(err) {
    # error message
    cat("An error occurred with site ", site_arg, " parameter ", parameter_arg, ".\n")
    cat("Error message:", conditionMessage(err), "\n")
    flush.console() # Immediately print the error messages
    NULL  # Return NULL in case of an error
  })

  return(summary)

}
