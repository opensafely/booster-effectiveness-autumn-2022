# # # # # # # # # # # # # # # # # # # # #
# Purpose: Combine km and cox estimates from different outcomes
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

# import libraries
library(tidyverse)
library(glue)
library(here)

# import command-line arguments
args <- commandArgs(trailingOnly=TRUE)
if(length(args)==0){
  effect <- "comparative"
  match_strategy <- "a"
} else {
  effect <- args[[1]]
  match_strategy <- args[[2]]
}

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))

# define output directory
output_dir <- ghere("output", "{effect}_{match_strategy}", "model")
fs::dir_create(output_dir)

# metaparams for all models that have been run for the given effect
model_args <- model_args[model_args$effect == effect,]

# get all subgroups
subgroups <- model_args %>% distinct(subgroup) %>% pull()

# combine and save outputs ----
combine_and_save_contrasts <- function(
    filenames,
    new_filename = NULL,
    metaparams = model_args
    ) {
  
  model_type <- unique(str_extract(filenames, "km|cox"))
  
  metaparams <- metaparams %>%
    filter(str_detect(model, model_type)) %>%
    uncount(length(filenames), .id="filename") %>% 
    mutate(across(filename, ~filenames[.x])) %>%
    filter(str_detect(filename, model))
  
  rounding_info <- unique(str_extract(filenames, "midpoint\\d+"))
  stopifnot(
    "Do not combine files with different rounding" = 
      length(rounding_info) <= 1
  )
  
  if (length(filenames) > 1) {
    stopifnot(
      "Must specify new_filename when length(filenames) > 1" =
        !is.null(new_filename)
      )
    if (!is.na(rounding_info)) {
      new_filename <- str_c(new_filename, rounding_info, sep = "_")
    }
  } else {
    new_filename <- filenames
  }
  
  metaparams %>%
    mutate(
      data = pmap(
        list(effect, model, subgroup, outcome, filename), 
        function(effect, model, subgroup, outcome, filename)  {
          dat <- try(
            read_csv(
              here("output", glue(effect, "_", match_strategy), "model", model, subgroup, outcome, glue(filename, ".csv"))
              )
            )
          if (inherits(dat, "try-error")) {
            dat <- tibble()
          } else {
            dat <- dat %>%
              add_column(
                subgroup_level = as.character(.[[subgroup]]),
                .before=1
              ) 
          }
          return(dat)
        }
        
      )
    ) %>%
    unnest(data) %>%
    select(-any_of(subgroups)) %>%
    mutate(across(
      starts_with(c("surv", "risk", "inc", "cml.rate", "irr", "cmlirr", "sr", "rd", "rr", "cox")),
      ~round(.x, digits = 5)
    )) %>%
    mutate(across( # this bit is a bit of a hack to deal with need to round counts of patients in cox models without rerunning them all - to update cox.R if running again 
      starts_with(c("npat", "nswitch")),
      ~ roundmid_any(.x, to = 6)
    )) %>%
    write_csv(fs::path(output_dir, glue("{new_filename}.csv")))
  
}


# km outputs
combine_and_save_contrasts(
  filenames = glue("km_estimates_midpoint{threshold}")
  ) 
combine_and_save_contrasts(
  filenames = c(
    glue("km_contrasts_cuts_midpoint{threshold}"),
    glue("km_contrasts_overall_midpoint{threshold}")
    ),
  new_filename = "km_contrasts"
  ) 
# cox outputs
combine_and_save_contrasts(
  filenames = c(
    "cox_unadj_contrasts_cuts",
    "cox_adj_contrasts_cuts",
    "cox_unadj_contrasts_overall",
    "cox_adj_contrasts_overall"
    ),
  new_filename = "cox_contrasts"
  )

## plot overall estimates for inspection ----

plot_estimates <- function(.data, estimate, estimate.ll, estimate.ul, logscale=FALSE, xttl, name){
  
  colour_labs <- c("Unadjusted", "Adjusted", "empty")
  colour_palette <- RColorBrewer::brewer.pal(n=length(colour_labs), name="Dark2")
  names(colour_palette) <- colour_labs
  
  subgroup_levels_labels <- unlist(unname(recoder[subgroups]))
  
  plot_data <- .data %>% 
    mutate(
      subgroup = fct_recoderelevel(subgroup, recoder$subgroups),
      outcome = fct_recoderelevel(outcome,  recoder$outcome),
      subgroup_level = fct_recoderelevel(subgroup_level, subgroup_levels_labels),
      yvar = as.character(if_else(
        subgroup %in% "Main",
        as.character(subgroup),
        as.character(subgroup_level)
      )), 
      Estimate = case_when( # used for the legend of plots - be careful of the difference between estimate (rd, rr, or hr) and Estimate (unadjusted or adjusted)
        model == "km"        ~ "Unadjusted", 
        model == "cox_unadj" ~ "Unadjusted", 
        model == "cox_adj"   ~ "Adjusted" 
        ),
      .before = 1
    ) %>%
    mutate(outcome = fct_relabel(outcome, str_wrap, width=10)) 

  if(logscale == TRUE){
    # transform to log scle 
    xscaletrans = scales::transform_log()
    xscale_expand = expansion(mult=c(0,0.1))
    vline = 1
    
    # edit xscale range for estimates on log scale 
    xrng <- c(0.0625, 0.125, 0.25, 0.5, 1, 2, 4, 8, 16, 32)
    xmn  <- plot_data %>% select({{estimate.ll}}) %>% min(na.rm = TRUE)
    xmx  <- plot_data %>% select({{estimate.ul}}) %>% max(na.rm = TRUE)
    xrng <- xrng[which(xrng > xmn & xrng < xmx)]     
  }
  else {
    xscaletrans = scales::transform_identity()
    xscale_expand = expansion(mult=c(0,0.01))
    vline = 0
    
    # edit xscale range for estimates on log scale 
    xrng <- c(-0.1, -0.05, 0, 0.05, 0.1)  
    xmn  <- plot_data %>% select({{estimate.ll}}) %>% min(na.rm = TRUE)
    xmx  <- plot_data %>% select({{estimate.ul}}) %>% max(na.rm = TRUE)
    xrng <- xrng[which(xrng > xmn & xrng < xmx)]     
  }  
    
  ylevels <- plot_data %>%
    distinct(yvar, subgroup, subgroup_level) %>%
    arrange(subgroup, subgroup_level) %>%
    pull(yvar)
  
  plot_temp <- plot_data %>%
    mutate(outcome = fct_relabel(outcome, str_wrap, width=10))  %>%
    mutate(across(yvar, factor, levels = ylevels)) %>%
    ggplot(aes(y=yvar, colour = Estimate)) +
    geom_vline(aes(xintercept=vline), linetype="dotted", colour="darkgrey")+
    geom_point(aes(x={{estimate}}), position=position_dodge(width=-0.3), alpha=0.7)+
    geom_linerange(aes(xmin={{estimate.ll}}, xmax={{estimate.ul}}), position=position_dodge(width=-0.3))+
    facet_grid(rows=vars(yvar), cols=vars(outcome), scales="free", space="free_y", switch="y")+
    scale_x_continuous(expand=xscale_expand, transform=xscaletrans, breaks = xrng, labels = scales::label_number(drop0trailing=TRUE))+
    xlab(xttl)+
    scale_color_manual(values=colour_palette)+
    labs(y=NULL)+
    theme_bw()+
    ggtitle(paste("Effect = ", str_to_title(effect), sep = ""))+
    theme(
      legend.position="bottom",
      axis.text.x.top=element_text(hjust=0),
      
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_blank(),
      strip.placement="outside",
      #strip.text.y.left = element_text(angle=0),
      strip.text.y.left = element_blank(),
      
      panel.border = element_blank(),
      panel.spacing = unit(0.3, "lines"),
    )
  
  ggsave(
    filename=file.path(output_dir, glue("overall_plot_rounded_{name}.png")),
    plot_temp,
    width=30, height=20, units="cm"
  )
  
  plot_temp
  
}

# read data and create plots ----
# km
#km_contrasts_overall <- 
#  read_csv(fs::path(output_dir, glue("km_contrasts_midpoint{threshold}.csv"))) %>%
#  filter(str_detect(filename, "overall"))
#km_contrasts_overall %>% plot_estimates(rd, rd.ll, rd.ul,logscale=FALSE, xttl="Risk Difference", "km_rd")
#km_contrasts_overall %>% plot_estimates(rr, rr.ll, rr.ul,logscale=TRUE, xttl="Risk Ratio", "km_rr")

# cox
#cox_contrasts_rounded <- read_csv(fs::path(output_dir, glue("cox_contrasts.csv"))) %>%
#  filter(str_detect(filename, "overall")) %>%
#  filter(str_detect(term, "^treated")) 
#cox_contrasts_rounded %>% plot_estimates(coxhr, coxhr.ll, coxhr.ul,logscale=TRUE, xttl="Hazard Ratio", "cox_hr")

