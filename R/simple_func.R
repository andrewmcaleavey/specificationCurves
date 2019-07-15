
#' Trim a data set based on one permutation of variabls
#'
#' @param ...
#' @param data
#' @param min_ses_num
#' @param max_ses_num
#' @param max_courses
#' @param completers_only
#' @param min_sev_baseline
#' @param min_sev_BDI
#' @param min_sev_BAI
#' @param min_obs
#' @param use_obs
#' @param param_list
#'
#' @return
#' @export
#'
#' @examples
trim_data <- function(...,
                      data,
                      min_ses_num = 0,
                      max_ses_num = 10000,
                      max_courses = 1,
                      completers_only = FALSE,
                      min_sev_baseline = 0,
                      min_sev_BDI = 0,
                      min_sev_BAI = 0,
                      min_obs = 0,
                      use_obs = c("all", "prepost", "post"),
                      param_list = NULL){  # param_list is an optional whole replacement

  if(!is.null(param_list)){  # if param_list supplied
    # re-assign all values to the param_list version
    # this is an option that comes in handy sometimes, not always needed.
    data <- param_list$data
    min_ses_num <- param_list$min_ses_num
    max_ses_num <- param_list$max_ses_num
    max_courses <- param_list$max_courses
    completers_only <- param_list$completers_only
    min_sev_baseline <- param_list$min_sev_baseline
    min_sev_BDI <- param_list$min_sev_BDI
    min_sev_BAI <- param_list$min_sev_BAI
    min_obs <- param_list$min_obs
    use_obs <- param_list$use_obs
  }

  temp <- dplyr::filter(data,
                        n_tx_eval_tot >= min_ses_num,
                        n_tx_eval_tot <= max_ses_num,
                        # need courses indicator still
                        # need completer indicator still
                        # need baseline severity indicators
                        n_meas_obs_tot >= min_obs)

  # if using just pre or post data, trim it here and compute new variables
  if(use_obs == "prepost" | use_obs == "post"){
    temp <- temp %>%
      dplyr::filter(has.measure == 1) %>%
      group_by(new_id) %>%
      slice(c(1, n())) %>%
      mutate(first_BDI = first(BDI),
             first_BAI = first(BAI)) %>%
      dplyr::filter(first_BDI >= min_sev_BDI & first_BAI >= min_sev_BAI) %>%
      slice(n()) %>%
      ungroup()

    temp <- mutate(temp,
                   minimum_sessions = min_ses_num,
                   maximum_sessions = max_ses_num,
                   maximum_courses = max_courses,
                   minimum_BDI_bl = min_sev_BDI,
                   minimum_BAI_bl = min_sev_BAI,
                   minimum_observations = min_obs)
  }
  else if(use_obs == "all"){
    temp <- temp %>%
      dplyr::filter(has.measure == 1) %>%
      group_by(new_id) %>%
      mutate(first_BDI = first(BDI),
             first_BAI = first(BAI)) %>%
      dplyr::filter(first_BDI >= min_sev_BDI & first_BAI >= min_sev_BAI) %>%
      ungroup()

    temp <- mutate(temp,
                   minimum_sessions = min_ses_num,
                   maximum_sessions = max_ses_num,
                   maximum_courses = max_courses,
                   minimum_BDI_bl = min_sev_BDI,
                   minimum_BAI_bl = min_sev_BAI,
                   minimum_observations = min_obs)
  }
  temp
}
