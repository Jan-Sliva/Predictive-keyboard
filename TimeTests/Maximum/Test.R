library(purrr)
library(ggplot2)
source("TimeTests/Maximum/Maximum.R")

set.seed(548646)

vec <- floor(runif(1000, 1, 1001))

splitted <- split(vec, 1:3)

res <- benchmark("intern" = {max(vec)},
                 "GetMax" = {GetMax(vec)},
                 "GetMaxVapply" = {GetMaxVapply(splitted)},
                 "GetMaxMap" = {GetMaxMap(splitted)},
                 replications = 10**4)


ggplot(res, aes(x = test, y = elapsed, fill = test)) +
  geom_col(position = "dodge")

res2 <- map_dfr(c(1:15),
                function(threads){
                  if (threads != 1){
                    splitted <- split(vec, 1:threads)
                    x <- benchmark(
                      "GetMaxVapply" = {GetMaxVapply(splitted)},
                      "GetMaxMap" = {GetMaxMap(splitted)},
                      replications = 10**4
                    )
                  }else{
                    x <- benchmark(
                      "GetMax" = {GetMax(vec)},
                      replications = 10**4
                    )
                  }
                  x$threads <- threads
                  return(x)
                })

ggplot(res2, aes(x = threads, y = elapsed, color = test)) + geom_line() + geom_point() + ylim(0, NA) +
  scale_x_continuous(breaks = 1:15, minor_breaks = NULL)
