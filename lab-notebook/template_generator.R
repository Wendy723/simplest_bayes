install.packages("infuser")

library(readr)
library(infuser)
modt <- read_file("../modeling/run006c.modt")

library(purrr)
map(1:4, ~ write_file(
    infuse(modt, 
           chain_number = ., 
           seed = round(runif(1, 1000, 100000), 0)), 
    file.path("modeling", paste0("run006c", ., ".mod"))))

