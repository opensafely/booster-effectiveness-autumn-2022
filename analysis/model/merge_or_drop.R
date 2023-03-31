## Function to merge or drop covariate levels if <=2 events in either category of expo 
merge_or_drop <- function(
  covariate_name,
  covariate_col,
  outcome_col,
  expo_col,
  events_threshold = 2
) {
  
  if (!(is.logical(covariate_col) | is.factor(covariate_col) | is.numeric(covariate_col))) {
    stop("covariate_col must be logical, factor or numeric")
  }
  
  # if logical convert to factor (specify the levels in case any are all one level)
  if (is.logical(covariate_col)) covariate_col <- factor(covariate_col, levels = c(FALSE, TRUE))
  
  if (is.factor(covariate_col)) {
    
    # define the function for implementing each merging and dropping iteration
    merge_or_drop_iteration <- function(old_col) {
      
      # check how many covariate levels
      # important to droplevels here!
      n_levels <- length(levels(droplevels(old_col)))
      
      if (n_levels <= 2) {
        
        # if covariate has <=2 levels, drop the covariate
        return(NULL)
        
      } else {
        
        # if the covariate has >2 levels, merge the levels
        
        old_levels <- levels(old_col)
        map_levels <- list()
        if (covariate_name == "ethnicity") {
          level_1 <- "White"
          map_levels[[level_1]] <- level_1
          map_levels[["Non-white"]] <- old_levels[which(old_levels != level_1)]
        } else if (covariate_name == "imd_Q5") {
          map_levels[["Quintiles 1-2 (more deprived)"]] <- old_levels[1:2]
          map_levels[["Quintiles 3-5 (less deprived)"]] <- old_levels[3:5]
        } else if (covariate_name == "bmi") {
          level_1 <- "Not obese"
          map_levels[[level_1]] <- level_1
          map_levels[["Obese"]] <- old_levels[which(old_levels != level_1)]
        } else if (covariate_name == "multimorb") {
          level_1 <- "0"
          map_levels[[level_1]] <- level_1
          map_levels[["1+"]] <- old_levels[which(old_levels != level_1)]
        } else if (covariate_name == "time_since_infection") {
          level_1 <- "never"
          map_levels[[level_1]] <- level_1
          map_levels[["31+ days"]] <- old_levels[which(old_levels != level_1)]
        } else if (covariate_name == "prior_test_cat") {
          level_1 <- "0"
          map_levels[[level_1]] <- level_1
          map_levels[["1+"]] <- old_levels[which(old_levels != level_1)]
        } else {
          stop(glue("`map_levels` not defined for var=\"{var}\""))
        }
        
        new_col <- factor(
          if_else(
            old_col %in% map_levels[[1]], 
            names(map_levels)[[1]], 
            names(map_levels)[[2]]
          ),
          levels = names(map_levels)
        )
        
        return(new_col)
        
      }
      
    }
    
    calc_min_events <- function(cov_col) {
      
     df <- tibble(
       covariate = cov_col,
       outcome = outcome_col,
       expo = expo_col
     )
     
     df %>%
       group_by(expo, outcome, covariate) %>%
       count() %>%
       ungroup() %>%
       # expand the df to make sure 0 counts are captured
       complete(expo, outcome, covariate, fill = list(n = 0)) %>%
       summarise(min_events = min(n)) %>%
       pull(min_events)
        
    }
    
    # check how many events per cross tab of category of expo and level of exposure
    min_events <- calc_min_events(covariate_col)
    
    while (min_events <= events_threshold) {
      
      covariate_col <- merge_or_drop_iteration(covariate_col)
      
      if (is.null(covariate_col)) {
        
        return(NULL) # breaks and returns NULL
        
      } else {
        min_events <- calc_min_events(covariate_col) 
      }
      
    }
    
  }
  
  return(tibble(!! sym(covariate_name) := covariate_col))

}
    
  
 