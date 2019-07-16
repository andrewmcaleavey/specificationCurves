
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

#' Isolate one permutation of non-NA predictors from a previously-defined
#' row of a matrix of predictor permutations
#'
#' @param ... Additional arguments passed on from other functions
#' @param x The predictor permutation to use
#'
#' @return A vector with just the non-NA predictors
#' @export
#'
#' @examples
#' isolate_predictors(x = c("age", NA, "first_BDI"))
#'
#' @details
#' This function exists only to remove NA values from lists of predictors
#' If x is entirely NA, returns NULL, which is compatible with lm() and lmer() formula definitions.
isolate_predictors <- function(..., x){
  # x should be a single row of predictors, not including target, target
  x <- x[!is.na(x)]
  if(length(x) > 0){
    return(x)
  }
  else return(NULL)
}

#' Conduct analysis of pre-post data WITHOUT constants.
#' Obsolete, use conduct_prepost_constant() instead.
#'
#' @param ... Additional variables
#' @param data Data to use
#' @param predictors List of predictors to include in formula
#' @param outcome Variable to use as the outcome of the lm() call
#'
#' @return an lm() model
#' @export
#'
#' @examples
#' outcome <- conduct_prepost(data = test_data,
#'   predictors = c("n_tx_eval_tot", "n_noshow_tot", "week_since_first_attend", "ther_id"),
#'   outcome = "BDI")
conduct_prepost <- function(...,
                            data,
                            predictors,
                            outcome){
  form <- as.formula(paste(outcome, paste(predictors, collapse = " + "),
                           sep = " ~ "))
  model <- lm(form, data = data)
}
# note that predictors is a list of names of variables, as strings
# the usage of that is like this:
# outcome <- conduct_prepost(data = test_data, predictors = c("n_tx_eval_tot", "n_noshow_tot", "week_since_first_attend", "ther_id"), outcome = "BDI")
# result is a model, type lm.
# takes a dataset, list of predictors, and the name of the outcome variable.


#' Conduct analysis of pre-post data WITH constants.
#'
#' @param ... Additional variables
#' @param data Data to use
#' @param predictors List of predictors to include in formula
#' @param outcome Variable to use as the outcome of the lm() call
#' @param constants List of variables that will be in EVERY permutation.
#' Entered separately from predictors at present.
#'
#' @return a model of type lm
#' @export
#'
#' @examples
#' outcome <- conduct_prepost_constant(data = test_data,
#'   predictors = c("n_tx_eval_tot", "n_noshow_tot", "week_since_first_attend", "ther_id"),
#'   constants = c("first_BDI"),
#'   outcome = "BDI")
conduct_prepost_constant <- function(...,
                                     data,
                                     predictors,
                                     outcome,
                                     constants = NULL){

  form <- as.formula(paste(outcome, paste(predictors, constants, collapse = " + ", sep = " + "),
                           sep = " ~ "))
  model <- lm(form, data = data)
}

# does the same thing but with lme4::lmer() instead of lm() Note it technically uses lmerTest::lmer(), for the p-value calculation (though I don't really endorse this myself, typically).
#' Conduct analysis using lmerTest::lmer()
#'
#' @param ... Additional variables
#' @param data Data to use
#' @param predictors List of predictors to include in formula
#' @param outcome Variable to use as the outcome of the lm() call
#' @param constants List of variables that will be in EVERY permutation.
#' Entered separately from predictors at present.
#' @param ranef The random effects variables
#' @param ranslope Variables (if any) to be included as slopes.
#'
#' @return A model of type lmerMod
#' @export
#'
#' @examples
#' out <- conduct_lmm_int_constant(data = test_data,
#'   predictors = c("n_tx_eval_tot", "n_noshow_tot", "week_since_first_attend", "ther_id"),
#'   constants = c("first_BDI"),
#'   outcome = "BDI")
#'
#' @details This function only tolerates one grouping variable at present.
#' Or more directly, one parenthetical feature per formula, so it is possible that a
#' specification like \code{(1 | therapist/patient)} could work?
#' It should tolerate multiple slope variables, though.
conduct_lmm_int_constant <- function(...,
                                     data,
                                     predictors,
                                     outcome,
                                     constants = NULL,
                                     ranef = "ther_id",
                                     ranslope = "1"){
  predictors_text <- paste(predictors, collapse = " + ")
  constants_text <- paste(constants, collapse = " + ")
  ranslope_text <- paste(ranslope, collapse = " + ")
  ranef_text <- paste0("(", ranslope_text, " | ", ranef, ")")
  form <- as.formula(paste(outcome, paste(paste(predictors, collapse = " + "),
                                          paste(constants, collapse = " + "),
                                          paste(ranef_text, collapse = " + "),
                                          sep = " + "),
                           sep = " ~ "))
  model <- lmerTest::lmer(form, data = data)
}


