## code to prepare `z` dataset goes here

rt <- list(m = 284, sd = 50, x = 159)
rt$z <- (rt$x  -rt$m) / rt$sd

usethis::use_data(rt, overwrite = TRUE)
