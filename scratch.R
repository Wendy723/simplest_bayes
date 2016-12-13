library(PKPDmisc)
library(dplyr)
library(data.table)
library(gganimate)
library(tidyverse)
paths <- list(
 c("modeling/run008c1_est_01/fort.50"),
 c("modeling/run009c1_est_01/fort.50"),
 c("modeling/run010c1_est_01/fort.50"),
 c("modeling/run011c1_est_01/fort.50"),
 c("modeling/run012c1_est_01/fort.50"),
 c("modeling/run013c1_est_01/fort.50")
)

c1 <- map_df(seq_along(paths), function(i) {
    fort50 <- fread(paths[[i]]) 
    names(fort50) <- c("REP", "ID", "CL", "V")
    return(fort50 %>% filter(ID == 1) %>% mutate(scenario = i))
})

id1_cl <- c1 %>% filter(ID == 1, scenario != 6) 

id1_cl %>%
    ggplot(aes(x = CL)) + geom_density(aes(fill = factor(scenario)), alpha = 0.6) +
    theme_bw() + base_theme() +
    scale_fill_discrete(name = "Scenario", 
                         labels = c(
                             "P/T/P/T",
                             "P/T/T",
                             "P/M/T",
                             "T/T",
                             "P/T"
                         ))

order <- list(
    c(4),
    c(4, 5),
    c(3, 4, 5),
    c(2, 3, 4, 5),
    c(1, 2, 3, 4, 5)
)

 labels <-  c(
     "P/T/P/T",
     "P/T/T",
     "P/M/T",
     "T/T",
     "P/T"
 )

plot_order <- map(order, function(o) {
    return(list(o, labels[o]))
})
gg_frames <- map(plot_order, function(o) {
    id1_cl %>% filter(scenario %in% o[[1]]) %>%
    ggplot(aes(x = CL)) + geom_density(aes(fill = factor(scenario)), alpha = 0.6) +
    theme_bw() + base_theme() +
    scale_fill_discrete(name = "Scenario", labels = o[[2]]) +
        labs(x = "Posterior Density of Clearance")
})
tmp <- tempdir()
lapply(seq_along(gg_frames), function(pi) {
    ggsave(file.path(tmp, paste0("plot", pi, ".png")), gg_frames[[pi]])
})

image_animate(image_join(lapply(1:5, function(i) {
    image_read(path = paste0("C:\\Users\\devin\\AppData\\Local\\Temp\\RtmpG40gGB\\plot", i, ".png"))
    })), fps = 1)

id_cl_anim <- gg_frames %>%
    ggplot(aes(x = CL, frame = frame)) + geom_density(aes(fill = factor(scenario)), alpha = 0.6) +
    theme_bw() + base_theme() +
    scale_fill_discrete(name = "Scenario", 
                         labels = c(
                             "P/T/P/T",
                             "P/T/T",
                             "P/M/T",
                             "T/T",
                             "P/T"
                         ))
gganimate(id_cl_anim)
c1 %>% filter(scenario == 6)

tmp <- tempdir()

names(sparser_scenarios)





fort50 %>% filter(ID == 1) %>% 
    ggplot(aes(x = CL)) + geom_histogram()
fort51 <- fread("modeling/run006c1_est_01/fort.51")
data <- fread("modeling/mdata/simple_nocovar_50id_6tp.csv", na = ".")

names(fort51) <- c("REP", "ID", "TIME", "IPRED")

s_fort51 <- fort51 %>% filter(between(ID, 2, 13)) %>% group_by(ID, TIME) %>% s_quantiles(IPRED, c(0.025, 0.5, 0.975))

id_9 <- left_join(data %>% filter(between(ID, 2, 13)), s_fort51 %>% filter(between(ID, 2, 13)))

library(ggplot2)
id_9 %>%
    ggplot(aes(x = TIME, y = DV)) + geom_point() +
    geom_ribbon(aes(ymin = IPRED_q2.5, ymax = IPRED_q97.5), alpha = 0.6, fill = "blue") +
    #geom_ribbon(aes(ymin = IPRED_q5, ymax = IPRED_q95), alpha = 0.6, fill = "blue") +
    facet_wrap(~ID) + scale_y_log10(breaks = c(1, 5, 10, 20, 40)) +
    theme_bw() +
    base_theme() +
    labs(x = "Time, hours",
         y = "Concentration, mg/L",
         subtitle = "95% Credible Interval of Individual Predicted Concentrations")
