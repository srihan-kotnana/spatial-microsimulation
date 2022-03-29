ind <- read.csv("comb_inds.csv", stringsAsFactors = T)
class(ind)
head(ind)
ind <- subset(ind, select = -id)

con_age <- read.csv("age.csv")
con_sex <- read.csv("sex.csv")
con_emp <- read.csv("employment.csv")
con_mob <- read.csv("mobility.csv")
con_inc <- read.csv("income.csv")
con_cit <- read.csv("citizenship.csv")
con_mar <- read.csv("marital_status.csv")
con_sch <- read.csv("schooling.csv")
con_dis <- read.csv("disability.csv")
con_wrk <- read.csv("work.csv")
con_ins <- read.csv("insurance.csv")
con_rce <- read.csv("race.csv")
pop <- read.csv("population.csv")

dim(ind)
dim(cons)

cat_age <- model.matrix(~ ind$age - 1)
cat_sex <- model.matrix(~ ind$sex - 1)
cat_emp <- model.matrix(~ ind$emp - 1)
cat_mob <- model.matrix(~ ind$mob - 1)
cat_inc <- model.matrix(~ ind$inc - 1)
cat_cit <- model.matrix(~ ind$cit - 1)
cat_mar <- model.matrix(~ ind$mar - 1)
cat_sch <- model.matrix(~ ind$sch - 1)
cat_dis <- model.matrix(~ ind$dis - 1)
cat_wrk <- model.matrix(~ ind$wrk - 1)
cat_ins <- model.matrix(~ ind$ins - 1)
cat_rce <- model.matrix(~ ind$rce - 1)

(ind_cat <- cbind(cat_age, cat_sex, cat_emp, cat_mob, cat_inc, cat_cit, cat_mar, cat_sch, cat_dis, cat_wrk, cat_ins, cat_rce))
ind_agg <- colSums(ind_cat)

cons <- cbind(con_age, con_sex, con_emp, con_mob, con_inc, con_cit, con_mar, con_sch, con_dis, con_wrk, con_ins, con_rce)
write.csv(cons, "cons.csv")


rbind(cons[1,],ind_agg)

n_zone <- nrow(cons)
n_ind <- nrow(ind)

cons <- apply(cons, 2, as.numeric)

weights_zone <- list()
zones <- seq(stats::rnorm(n_zone))

for (val in zones){
  weights_zone <- append(weights_zone,list(ipfp(cons[val,], t(ind_cat),x0 = rep(1,n_ind))))
}

length(weights_zone)

int_trs <- function(x){
  xv <- as.vector(x) 
  xint <- floor(xv) 
  r <- xv - xint
  def <- round(sum(r))
  
  topup <- sample(length(x), size = def, prob = r)
  xint[topup] <- xint[topup] + 1
  dim(xint) <- dim(x)
  dimnames(xint) <- dimnames(x)
  xint
}

set.seed(50)

weight_int <- list()
zones <- seq(stats::rnorm(n_zone))

for (val in zones){
  weight_int <- append(weight_int, list(int_trs(x=unlist(weights_zone[val]))))
}
length(weight_int)

int_expand_vector <- function(x){
  index <- 1:length(x)
  rep(index, round(x))
}
ind_indices <- list()
for (val in zones){
  ind_indices <- append(ind_indices,list(int_expand_vector(unlist(weight_int[val]))))
}
length(ind_indices)

ind_zone <- list()
for (val in zones){
  ind_zone <- append(ind_zone, list(ind[unlist(ind_indices[val]),]))
}
length(ind_zone)

names(ind_zone) <- c(1:length(ind_zone))

lapply(1:length(ind_zone), function(i) write.csv(ind_zone[[i]], 
                                                 file = paste0("../",paste0(names(ind_zone[i]), ".csv")),
                                                 row.names = FALSE))
