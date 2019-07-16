
#' Trim a data set based on one permutation of variabls
#'
#' @param ... additional variables passed on from other functions
#' @param data data to use
#' @param min_ses_num Minimum number of sessions required
#' @param max_ses_num Maximum number of sessions allowed
#' @param max_courses Maximum number of courses of treatment allowed
#' @param completers_only Logical. Should only completers be used?
#' @param min_sev_baseline Minimum initial severity, outcome not defined.
#' @param min_sev_BDI Minimum initial severity on the BDI.
#' @param min_sev_BAI Minimum initial severity on the BAI.
#' @param min_obs Minimum number of observations per patient.
#' @param use_obs Which observations to use? Options: "prepost", "post", and "all".
#' @param param_list Optional list of parameter values that overwrite existing parameters if included.
#'
#' @return a trimmed dataset
#' @export
#'
#' @examples
#' trim_data(data = sim_data)
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


#' Generate permutations of predictors with varying options
#'
#' @param ... Additional arguments
#' @param options A list of variables in the data set
#' @param target The variable in the data set that will be analyzed for its effect
#'
#' @return A matrix in which each row is a unique permutation of the options for each variable
#' @export
#'
#' @examples
#' make_predictors(options = c("BAI", "first_BDI"), target = "age")
make_predictors <- function(..., options, target){
  # options should just be a list of variables in the data set,
  # The function should return a list of lists - one for each set of predictor variables. Assumes that each predictor can either be included or NOT included.
  # target is required here because it HAS to be included in every model run.

  # trying to use expand.grid().
  to_expand <- list(0)
  for(i in seq_along(options)){
    to_expand[i] <- paste0("c(\"", as.character(options[i]), "\", NA)")
  }
  # this is HORRIBLE CODE - but it works?
  out <- expand.grid(eval(parse(text = paste0("list(", paste(unlist(to_expand), collapse = ', '), ")"))))

  # rename variable names from "Var1" to whatever the varaible is
  for(i in seq_along(options)){
    names(out)[i] <- levels(out[1, i])
  }

  out <- cbind(out, target = rep(target, nrow(out)))
  # the result is a matrix of rows with different variables included in each row. Every row is a unique permutation of the list.
}
