# # # # # # # # # # # # # # # # # # # # #
# Purpose: Test for subgroup heterogeneity and plot HR estimates 
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

# import libraries
library(tidyverse)
library(glue)
library(here)
library(gridExtra)
library(meta)

# load functions and parameters
source(here("analysis", "design.R"))
source(here("lib", "functions", "utility.R"))
source(here("lib", "functions", "survival.R"))
source(here("analysis", "process", "process_functions.R"))


# Load results and store objects
output_dir_inc <- ghere("output", "postrelease", "incremental_a", "model")
output_dir_com <- ghere("output", "postrelease", "comparative_a", "model")

HR_incremental_a <- read_csv(fs::path(output_dir_inc, glue("HR_restimate_output.csv"))) 
HR_comparative_a <- read_csv(fs::path(output_dir_com, glue("HR_restimate_output.csv"))) 

# Calculate Q statistic and I-squared values

meta_results <- data.frame(comparison = character(), 
                           outcome = character(), 
                           subgroup = character(),
                           df = numeric(),
                           Q = numeric(), 
                           p_Q = numeric(), 
                           I2 = numeric(), 
                           I2_lower = numeric(), 
                           I2_upper = numeric())

for(comp in c("incremental", "comparative") ){
  for(outc in c("covidadmitted", "coviddeath", "noncoviddeath", "fracture")){ 
    for(subg in c("agegroup_match","cv")){
    
      #comp <- "comparative"
      #outc <- "covidadmitted"
      
      df_name <- paste0("HR_", comp, "_a")
      tmp_df <- get(df_name)
        
      tmp <- tmp_df %>%
          filter(outcome == outc) %>%
          filter(subgroup == subg) %>% 
          mutate(
            loghr = log(mantel_cox_hr),
          )
      
      meta_tmp <- metagen(data = tmp, 
                          TE = loghr, 
                          seTE = se_log_hr, 
                          studlab = subgroup_level, 
                          method.I2 = "Q")
      
      summary(meta_tmp)
      
      meta_results[nrow(meta_results)+1,] <- c(comp, outc, subg, 
                                               meta_tmp$df.Q, meta_tmp$Q, meta_tmp$pval.Q, 
                                               meta_tmp$I2, meta_tmp$lower.I2, meta_tmp$upper.I2) 
      rm(df_name, tmp_df, tmp, meta_tmp)   
      
    }    
  }
} 

meta_results_fig <- meta_results %>% 
  mutate(
    p_str = case_when(
      as.numeric(p_Q) < 0.001 ~ "Between subgroup heterogeneity p < 0.001",
      TRUE ~ paste0("Between subgroup heterogeneity p = ", sprintf("%5.3f", as.numeric(p_Q)))
    ),
  ) %>% 
  select(comparison, outcome, subgroup, p_str) 


# create function to plot estimates
plot_estimates <- function(.data, effect, metadat, estimate, estimate.ll, estimate.ul, logscale=FALSE, xttl){
  
  colour_labs <- c("Main","Age","Clinical vulnerability","empty")
  colour_palette <- RColorBrewer::brewer.pal(n=length(colour_labs), name="Dark2")
  names(colour_palette) <- colour_labs
  
  subgroup_levels_labels <- unlist(unname(recoder[subgroups]))

  metadat <- metadat %>% 
    filter(comparison == {{effect}}) %>%
    select(outcome, subgroup, p_str) 
  
  plot_data <-   .data %>% 
    left_join(metadat, by=c("subgroup", "outcome")) %>%
    mutate(
      subgroup = fct_recoderelevel(subgroup, recoder$subgroups),
      outcome = fct_recoderelevel(outcome,  recoder$outcome),
      subgroup_level = fct_recoderelevel(subgroup_level, subgroup_levels_labels),
      yvar = case_when(
        subgroup == "Main" ~ 8, 
        subgroup == "Age" & subgroup_level == "50-64 years" ~ 6 , 
        subgroup == "Age" & subgroup_level == "65-74 years" ~ 5 , 
        subgroup == "Age" & subgroup_level == "75+ years" ~ 4 , 
        subgroup == "Clinical vulnerability" & subgroup_level == "Not clinically vulnerable" ~ 2,
        subgroup == "Clinical vulnerability" & subgroup_level == "Clinically vulnerable" ~ 1 
      ), 
      yvar_p = yvar + 0.5,
      p_fig = case_when(
        yvar == 2 ~ p_str,
        yvar == 6 ~ p_str,
        TRUE ~ ""
      ),
      .before = 1
    ) %>%
    mutate(outcome = fct_relabel(outcome, str_wrap, width=10)) %>% 
    arrange(outcome, yvar)
  
  # transform to log sacle 
  xscaletrans = scales::transform_log()
  xscale_expand = expansion(mult=c(0,0.1))
  vline = 1
  xrng <- c(0.25,0.35, 0.5, 0.7, 1, 1.4, 2, 2.9, 4)
  xlims <- c(0.2, 4.2)
    
 
  ylabs <- c("Clinically vulnerable", 
             "Not clinically vulnerable",  
             "75+ years", 
             "65-74 years", 
             "50-64 years", 
             "Overall" 
  )

  if(effect == "incremental"){
    text_x = 0.315
  } else if(effect == "comparative"){
    text_x = 0.730
  }
  
  plot_temp <- plot_data %>%
    mutate(outcome = fct_relabel(outcome, str_wrap, width=10))  %>%
    ggplot(aes(y=yvar)) +
    geom_vline(aes(xintercept=vline), linetype="dashed", colour="darkgrey")+
    geom_point(aes(x={{estimate}}, colour = subgroup), alpha=0.7)+
    geom_linerange(aes(xmin={{estimate.ll}}, xmax={{estimate.ul}}, colour = subgroup))+
    geom_text(aes(label = p_fig, x = text_x, y = yvar_p, ), alpha=0.7, size = 3)+    
    facet_grid(rows=vars(outcome), scales="free_x", space="fixed")+
    scale_y_continuous(breaks = c(1,2,4,5,6,8), labels = ylabs) +
    scale_x_continuous(expand=xscale_expand, transform=xscaletrans, 
                       #limits = xlims, 
                       breaks = xrng, 
                       labels = scales::label_number(drop0trailing=TRUE))+
    xlab(xttl)+
    scale_color_manual(values=colour_palette)+
    labs(y=NULL)+
    theme_bw()+
    theme(
      legend.position="none",
      axis.text.x.top=element_text(hjust=0),
      
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      strip.background = element_blank(),
      strip.placement="outside",
      strip.text.y.left = element_text(angle=0),
      #strip.text.y.left = element_blank(),
      
      #panel.border = element_blank(),
      panel.spacing = unit(0.3, "lines"),
    )
  
  if(effect == "incremental") {
    output_dir = output_dir_inc
  } else if(effect == "comparative"){
    output_dir = output_dir_com
  }
  
  ggsave(
    filename=file.path(output_dir, glue("overall_plot_mantel_cox.png")),
    plot_temp,
    width=15, height=20, units="cm"
  )
  
  plot_temp
  
}

# output figures
HR_incremental_a %>% plot_estimates("incremental", meta_results_fig, mantel_cox_hr, hr_lower, hr_upper, logscale=TRUE, xttl="Hazard ratio over total follow-up (95% CI)")
HR_comparative_a %>% plot_estimates("comparative", meta_results_fig, mantel_cox_hr, hr_lower, hr_upper, logscale=TRUE, xttl="Hazard ratio over total follow-up (95% CI)\n Reference = Bivalent BNT162b2")
