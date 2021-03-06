library(PKPDmisc)
library(dplyr)
library(data.table)
library(magick)
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

# label values associated with each scenario
# eg scenario 1 contained 4 points (P)eak/(T)rough (P)eak/(T)rough 
labels <-  c(
    "P/T/P/T",
    "P/T/T",
    "P/M/T",
    "T/T",
    "P/T"
)

# given the plots will have sequentially more scenarios, the labels should
# only be set for the scenarios available at each step, so give back the
# scenarios (o) and the labels associated with that scenario
plot_order <- map(order, function(o) {
    return(list(o, labels[o]))
})

# list of the plots
gg_frames <- map(plot_order, function(o) {
    id1_cl %>% filter(scenario %in% o[[1]]) %>%
        ggplot(aes(x = CL)) + geom_density(aes(fill = factor(scenario)), alpha = 0.6) +
        theme_bw() + base_theme() +
        scale_fill_discrete(name = "Scenario", labels = o[[2]]) +
        labs(x = "Posterior Density of Clearance")
})

# need to save plots somewhere as magick requires them to be saved out
tmp <- tempdir()

lapply(seq_along(gg_frames), function(pi) {
    ggsave(file.path(tmp, paste0("plot", pi, ".png")), gg_frames[[pi]])
})

# magick requires all plots to be read in via image_read, image_join allows
# the list of plots to be passed in to be animated
image_animate(image_join(lapply(1:5, function(i) {
    image_read(path = paste0(tmp, "plot", i, ".png"))
})), fps = 1)
