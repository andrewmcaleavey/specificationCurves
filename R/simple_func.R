
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

#' Extract values of interest from lm and lmerMod objects
#'
#' @param ... Additional variables
#' @param model Model to extract from
#' @param target Name of the variable predictor of interest
#'
#' @return a 1x6 data frame with Estimate, SE, t val, and p val, lower, and upper
#' @export
#'
#' @examples
#' extract_target_lm(model = model, target = "age")
extract_target_lm <- function(..., model, target){
  values <- data.frame(as.list(coef(summary(model))[target, ]))
  values$lower <- values[1, 1] - 1.96 * values[1, 2]
  values$upper <- values[1, 1] + 1.96 * values[1, 2]
  if("df" %in% names(values)){
    values <- select(values, -df)
  }
  values
}
# result is a 1x6 dataframe with Estimate, SE, t val, and p val, lower, and upper
# takes a model as the input, computed earlier

#' Run a single data permutation through multiple predictor permutations
#'
#' @param ...
#' @param data
#' @param predictors
#' @param target
#' @param outcome
#' @param constants
#'
#' @return
#' @export
#'
#' @examples
run_data <- function(...,
                     data,
                     predictors,
                     target = predictors$target[1],
                     outcome = "BDI",
                     constants = "first_BDI"){
  # data is the single data.frame to use
  # predictors is the matrix of predictors to use

  out <- data.frame("Estimate" = 0,
                    "Std. Error" = 0,
                    "t value" = 0,
                    "Pr(>|t|)" = 0,
                    "lower" = 0,
                    "upper" = 0)

  for(i in 1:nrow(predictors)){  # for each predictor set
    use_predictors <- isolate_predictors(x = predictors[i, ])
    # use_predictors should be a list of the predictors to include

    mod <- conduct_prepost_constant(data = data,
                                    predictors = use_predictors,
                                    outcome = outcome,
                                    constants = constants)
    # mod is an lm object

    extracts <- extract_target_lm(model = mod, target = target)
    # extracts is a data frame with six outputted values

    out <- rbind(out, extracts)
  }
  out <- data.frame(out[2:nrow(out), ]) # cropping the first empty row.
  out <- cbind(out, N = mod$df.residual)  # adding residual DF to get a sense of the size of the data effectively
  out <- cbind(out,
               predictors,
               covariate_perm = 1:nrow(out),
               outcome_var = outcome,
               minimum_sessions = data$minimum_sessions[1],
               maximum_sessions = data$maximum_sessions[1],
               maximum_courses = data$maximum_courses[1],
               minimum_BDI_bl = data$minimum_BDI_bl[1],
               minimum_BAI_bl = data$minimum_BAI_bl[1],
               minimum_observations = data$minimum_observations[1],
               ran_ther_included = FALSE)  # adding a bunch of variables that are helpful later
  out <- mutate(out,
                highlight = if_else(lower < 0 & upper < 0, "neg",
                                    if_else(lower < 0 & upper > 0, "ns",
                                            "pos")))
}

#' Run a single data frame through multiple predictor permutations with lmer()
#'
#' @param ...
#' @param data
#' @param predictors
#' @param target
#' @param outcome
#' @param constants
#' @param ranef
#' @param ranslope
#'
#' @return
#' @export
#'
#' @examples
run_data_lmm <- function(...,
                         data,
                         predictors,
                         target = predictors$target[1],
                         outcome = "BDI",
                         constants = "first_BDI",
                         ranef = "ther_id",
                         ranslope = "1"){
  # data is the single data.frame to use
  # predictors is the matrix of predictors to use

  out <- data.frame("Estimate" = 0,
                    "Std. Error" = 0,
                    "t value" = 0,
                    "Pr(>|t|)" = 0,
                    "lower" = 0,
                    "upper" = 0)

  for(i in 1:nrow(predictors)){  # for each predictor set
    use_predictors <- isolate_predictors(x = predictors[i, ])
    # use_predictors should be a list of the predictors to include

    mod <- conduct_lmm_int_constant(data = data,
                                    predictors = use_predictors,
                                    outcome = outcome,
                                    constants = constants,
                                    ranef = ranef,
                                    ranslope = ranslope)
    # mod is an lm object

    # only do anything if not singular
    if(!isSingular(mod)){
      extracts <- extract_target_lm(model = mod, target = target)
      # extracts is a data frame with six outputted values
    }
    else{ # if it was singular, create NA values
      extracts <- data.frame("Estimate" = NA,
                             "Std. Error" = NA,
                             "t value" = NA,
                             "Pr(>|t|)" = NA,
                             "lower" = NA,
                             "upper" = NA)
    }

    out <- rbind(out, extracts)
  }
  out <- data.frame(out[2:nrow(out), ]) # cropping the first empty row.
  out <- cbind(out, N = NA)  # adding residual DF to get a sense of the size of the data effectively. ONLY LEAVING IN bc don't want to bother righ tnow.
  out <- cbind(out,
               predictors,
               covariate_perm = 1:nrow(out),
               outcome_var = outcome,
               minimum_sessions = data$minimum_sessions[1],
               maximum_sessions = data$maximum_sessions[1],
               maximum_courses = data$maximum_courses[1],
               minimum_BDI_bl = data$minimum_BDI_bl[1],
               minimum_BAI_bl = data$minimum_BAI_bl[1],
               minimum_observations = data$minimum_observations[1],
               ran_ther_included = TRUE)  # adding a bunch of variables that are helpful later
  out <- mutate(out,
                highlight = if_else(lower < 0 & upper < 0, "neg",
                                    if_else(lower < 0 & upper > 0, "ns",
                                            "pos")))
}

#' Run a complete specification curve analysis
#'
#' @description Given a data frame and the options, trim it in every permutation
#' and run each with every predictor permutation available.
#'
#' @param ... Additional arguments
#' @param data Data set
#' @param min_ses_options Minimum number of sessions attended
#' @param max_ses_options Maximum number of sessions attended
#' @param max_courses_options Maximum number of courses allowed
#' @param min_sev_BDI_options Minimim initial severity of BDI
#' @param min_sev_BAI_options Minimum initial severity of BAI
#' @param min_obs_options Minimum number of observations per patient
#' @param predictors List of predictors to permute
#' @param target The target effect
#' @param outcome The outcome variable
#' @param constants List of variables to including in all permutations
#' @param include_ran_thers Logical: should random therapists be included as an option?
#'
#' @return a \code{tibble} with outcome of all analyses
#' @export
#'
#' @examples
run_multiple_trims <- function(...,
                               data,
                               min_ses_options = NA,
                               max_ses_options = NA,
                               max_courses_options = NA,
                               min_sev_BDI_options = NA,
                               min_sev_BAI_options = NA,
                               min_obs_options = NA,
                               predictors,  # string vector
                               target,
                               outcome = "BDI",
                               constants = "first_BDI",
                               include_ran_thers = TRUE){

  out <- expand.grid(min_ses_options,
                     max_ses_options,
                     max_courses_options,
                     min_sev_BDI_options,
                     min_sev_BAI_options,
                     min_obs_options)
  names(out) <- c("min_ses_options", "max_ses_options", "max_courses_options", "min_sev_BDI_options", "min_sev_BAI_options", "min_obs_options")
  # out here is just a matrix with all combinations of all the variables of interest.

  # define param_list_temp, will be altered on each iteration of next loop
  param_list_temp <- list(data = data,
                          min_ses_num = 0,
                          max_ses_num = 100000,
                          max_courses = 1,
                          completers_only = FALSE,
                          min_sev_baseline = 0,
                          min_sev_BDI = 0,
                          min_sev_BAI = 0,
                          min_obs = 0,
                          use_obs = "prepost")

  # define predictors_temp, which can be modified
  predictors_temp <- make_predictors(options = predictors, target = target)

  for(i in 1:nrow(out)){  # for each permutation of inputs
    param_list_mod <- param_list_temp

    # if first variable is not NA, replace the value in param_list_mod with the corrsponding value from out.
    if(!is.na(out[i, 1])) {
      param_list_mod$min_ses_num <- out[i, 1]
    }
    if(!is.na(out[i, 2])) {
      param_list_mod$max_ses_num <- out[i, 2]
    }
    if(!is.na(out[i, 3])) {
      param_list_mod$max_courses <- out[i, 3]
    }
    if(!is.na(out[i, 4])) {
      param_list_mod$min_sev_BDI <- out[i, 4]
    }
    if(!is.na(out[i, 5])) {
      param_list_mod$min_sev_BAI <- out[i, 5]
    }
    if(!is.na(out[i, 6])) {
      param_list_mod$min_obs <- out[i, 6]
    }
    #need to repeat for all the variables

    out_dat <- trim_data(data = data,
                         param_list = param_list_mod,
                         use_obs = "prepost")
    # param_list_mod is the MODIFED list of parameters, specific to this iteration.

    ##### now ready to run this data set
    out_analysis <- run_data(data = out_dat,
                             predictors = predictors_temp,
                             outcome = outcome,
                             constants = constants)

    # if random therapists should be included (at all, run here)
    if(include_ran_thers){
      out_analysis_lmm <- run_data_lmm(data = out_dat,
                                       predictors = predictors_temp,
                                       outcome = outcome,
                                       constants = constants,
                                       ranef = "ther_id",
                                       ranslope = "1")
      # combining random and nonrandom outputs
      output1 <- rbind(out_analysis, out_analysis_lmm)
    }
    output1 <- out_analysis

    # now combining all the data into one dataset
    if(i ==1){
      output <- output1
    }
    else{
      output <- rbind(output, output1)
    }
  }
  output <- cbind(output, permutation = 1:nrow(output))
  # output is a dataset with the specified variables
}
