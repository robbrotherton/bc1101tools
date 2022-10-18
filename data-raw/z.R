## code to prepare `z` dataset goes here

spiderman <- list(m = 284, sd = 50, x = 159)
spiderman$z <- (rt$x  -rt$m) / rt$sd

usethis::use_data(spiderman, overwrite = TRUE)


cbt <- list(m = 30.25, sd = 14.89, x = 15.49, n = 40)
cbt$z <- (rt$x - rt$m) / rt$sd
usethis::use_data(cbt, overwrite = TRUE)
