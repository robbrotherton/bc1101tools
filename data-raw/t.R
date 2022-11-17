## code to prepare `t` dataset goes here

class_reaction_times <- c(327, 335, 359, 430, 275.4,
                          272, 350, 343.2, 278, 354,
                          303, 328, 371, 312, 346,
                          359, NA, 259, 313.6, 258,
                          244, 374.4, NA, 338, 290)


usethis::use_data(class_reaction_times, overwrite = TRUE)

# Single sample example ----
n  <- length(class_reaction_times[!is.na(class_reaction_times)])
M  <- mean(class_reaction_times, na.rm = T)
SD <- sd(class_reaction_times, na.rm = T)
mu <- 284
sigma <- 50
d  <- (M - mu)/SD
t  <- (M-mu) / (SD/sqrt(n))

t <- list(data = class_reaction_times,
          n  = n ,
          M  = M ,
          SD = SD,
          mu = mu,
          sigma = sigma,
          d  = d ,
          t  = t )

usethis::use_data(t, overwrite = TRUE)


# Independent samples example ----

conditionA <- c(1, 5, 2, 4, 3)
conditionB <- c(5, 5, 2, 5, 3)
# r <- function(x) {round(x, 2)}
# t.test(conditionA, conditionB, var.equal=T, paired=F)

SS <- function(x) sum((x - mean(x))^2)

m1 <- mean(conditionA)
m2 <- mean(conditionB)
n1 <- length(conditionA)
n2 <- length(conditionB)
N  <- n1 + n2
df1<- n1 - 1
df2<- n2 - 1
df <- df1 + df2
ss1<- SS(conditionA)
ss2<- SS(conditionB)
sd1<- sd(conditionA)
sd2<- sd(conditionB)
pooled_variance <- (ss1 + ss2)/(df1 + df2)
standard_error <- sqrt((pooled_variance/n1)+(pooled_variance/n2))
t <- (m1-m2)/standard_error

tInd <- list(
  conditionA = conditionA,
  conditionB = conditionB,
  m1  = m1 ,
  m2  = m2 ,
  n1  = n1 ,
  n2  = n2 ,
  N   = N  ,
  df1 = df1,
  df2 = df2,
  df  = df ,
  ss1 = ss1,
  ss2 = ss2,
  sd1 = sd1,
  sd2 = sd2,
  pooled_variance = pooled_variance,
  standard_error = standard_error,
  t = t
)


usethis::use_data(tInd, overwrite = TRUE)




# Related-samples example ----

A <- alone <- c(54,67,38,46,42)
C <- competition <- c(43,57,39,41,36)
df_between <- data.frame(A, C)
df_within  <- data.frame(A, C) %>% dplyr::mutate(D = C - A)

D <- diff <- C-A

md  <- mean(D)
SSd <- sum((D-mean(D))^2)
vard<- var(D)
sdd <- sd(D)

na  <- length(A)
nc  <- length(C)
dfa <- na-1
dfc <- nc-1
ma  <- mean(A)
mc  <- mean(C)
SSa <- sum((A-mean(A))^2)
SSc <- sum((C-mean(C))^2)

# independent-samples
s2p <- (SSa + SSc) / (dfa + dfc)
sm1m2 <- sqrt((s2p/na)+(s2p/nc))
t <- (ma-mc)/sm1m2

# related-samples
smd <- sdd/sqrt(length(D))
td <- md/smd

triplett <- list(
  df_between = df_between,
  df_within = df_within,
  scores_alone = A,
  scores_competition = C,
  diff_scores = D,
  mean_diff = md,
  sd_diff = sdd,
  na  = na,
  nc  = nc,
  dfa = dfa,
  dfc = dfc,
  ma  = ma,
  mc  = mc,
  SSa = SSa,
  SSc = SSc,
  s2p = s2p,
  sm1m2 = sm1m2,
  t = t,
  smd = smd,
  td = td
)

usethis::use_data(triplett, overwrite = TRUE)

