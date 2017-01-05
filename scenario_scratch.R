library(satchel)
satchel <- Satchel$new("visualize", "data/derived/satchel/")

available_files <- satchel$available()

ext_files <- available_files$scenario_set1[grepl("_ext$", available_files$scenario_set1)]

satchel$preview("run120_nids_56_s1dpt_c4_est_01_ext" )
library(tidyverse)

scenario_df <- map_df(ext_files, function(.file) {
    satchel$use(.file)
})
scenario_label <- function(scenario) { 
  case_when(
    scenario == "r1d" ~ "rich",
    scenario == "s1d" ~ "Day 1 Peak/Trough",
    scenario == "s1dpt" ~ "Day 1 Peak/Trough, Day 2 Trough",
    scenario == "s2d" ~ "Day 1 Peak/Trough, Day 2 Peak/Trough",
    scenario == "s2trough" ~ "Day 1 Trough, Day 2 Trough",
    scenario == "tr" ~ "Day 1 Trough"
  )
}
scenario_lvl <- function(scenario) { 
  case_when(
    scenario == "r1d" ~ 1,
    scenario == "s1d" ~ 4,
    scenario == "s1dpt" ~ 3,
    scenario == "s2d" ~ 2,
    scenario == "s2trough" ~ 5,
    scenario == "tr" ~ 6
  )
}

scenario_df <- scenario_df %>% 
    mutate(
           scenario_level = scenario_lvl(scenario),
           scenario_lbl = forcats::fct_reorder(scenario_label(scenario), scenario_level)
)
scenario_df %>%
    ggplot(aes(x = CL, fill = scenario_lbl)) + 
    geom_density(alpha = 0.5) + facet_wrap(~nids, ncol = 1)


gg_cl <- scenario_df %>%
    ggplot(aes(x = CL, fill = factor(nids))) + 
    geom_density(alpha = 0.5) + facet_wrap(~scenario_lbl, ncol = 1) +
    guides(fill = F)
gg_v <- scenario_df %>%
    ggplot(aes(x = V, fill = factor(nids))) + 
    geom_density(alpha = 0.5) + facet_wrap(~scenario_lbl, ncol = 1)

library(cowplot)
theme_set(theme_bw())
plot_grid(gg_cl, gg_v)
