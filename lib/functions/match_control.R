# this function will be updated to allow for different matching strategies

my_matchit <- function(data, exact, caliper) {
  
  matchit_res <- MatchIt::matchit(
      formula = treated ~ 1,
      data = data,
      # these method and distance options don't really do anything because we
      # only want exact + caliper match
      method = "nearest", distance = "glm", 
      replace = FALSE,
      estimand = "ATT",
      exact = exact,
      caliper = caliper, std.caliper=FALSE,
      m.order = "random",
      #verbose = TRUE,
      ratio = 1L # 1:1 match
    )
  
}

safely_matchit <- purrr::safely(my_matchit)
