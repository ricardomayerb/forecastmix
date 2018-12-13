source('./R/combinations_functions.R')

arg <- readRDS("./Argentina_quasi_ave.rds")
bra <- readRDS("./Brasil_quasi_ave.rds")
chl <- readRDS("./Chile_quasi_ave.rds")
col <- readRDS("./Colombia_quasi_ave.rds")
per <- readRDS("./Peru_quasi_ave.rds")
pry <- readRDS("./Paraguay_quasi_ave.rds")
ury <- readRDS("./Uruguay_quasi_ave.rds")

arg_rgdp_20 <- arg[[1]]
arg_realized <- window(arg_rgdp_20, end = c(2018, 3))
arg_rgdp_10 <- ts(c(arg_realized, arg[[2]]$ave_by_h_fc), 
                  frequency = frequency(arg_realized), 
                  start = start(arg_realized))
arg_rgdp_30 <- ts(c(arg_realized, arg[[4]]$ave_by_h_fc), 
                  frequency = frequency(arg_realized), 
                  start = start(arg_realized))
arg_2018_10 <- window(arg_rgdp_10, start = c(2018, 1), end = c(2018, 4))
arg_2018_20 <- window(arg_rgdp_20, start = c(2018, 1), end = c(2018, 4))
arg_2018_30 <- window(arg_rgdp_30, start = c(2018, 1), end = c(2018, 4))
arg_2019_10 <- window(arg_rgdp_10, start = c(2019, 1), end = c(2019, 4))
arg_2019_20 <- window(arg_rgdp_20, start = c(2019, 1), end = c(2019, 4))
arg_2019_30 <- window(arg_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(arg_2018_10))
print(mean(arg_2018_20))
print(mean(arg_2018_30))
print(mean(arg_2019_10))
print(mean(arg_2019_20))
print(mean(arg_2019_30))


bra_rgdp_20 <- bra[[1]]
bra_realized <- window(bra_rgdp_20, end = c(2018, 3))
bra_rgdp_10 <- ts(c(bra_realized, bra[[2]]$ave_by_h_fc), 
                  frequency = frequency(bra_realized), 
                  start = start(bra_realized))
bra_rgdp_30 <- ts(c(bra_realized, bra[[4]]$ave_by_h_fc), 
                  frequency = frequency(bra_realized), 
                  start = start(bra_realized))
bra_2018_10 <- window(bra_rgdp_10, start = c(2018, 1), end = c(2018, 4))
bra_2018_20 <- window(bra_rgdp_20, start = c(2018, 1), end = c(2018, 4))
bra_2018_30 <- window(bra_rgdp_30, start = c(2018, 1), end = c(2018, 4))
bra_2019_10 <- window(bra_rgdp_10, start = c(2019, 1), end = c(2019, 4))
bra_2019_20 <- window(bra_rgdp_20, start = c(2019, 1), end = c(2019, 4))
bra_2019_30 <- window(bra_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(bra_2018_10))
print(mean(bra_2018_20))
print(mean(bra_2018_30))
print(mean(bra_2019_10))
print(mean(bra_2019_20))
print(mean(bra_2019_30))



chl_rgdp_20 <- chl[[1]]
chl_realized <- window(chl_rgdp_20, end = c(2018, 3))
chl_rgdp_10 <- ts(c(chl_realized, chl[[2]]$ave_by_h_fc), 
                  frequency = frequency(chl_realized), 
                  start = start(chl_realized))
chl_rgdp_30 <- ts(c(chl_realized, chl[[4]]$ave_by_h_fc), 
                  frequency = frequency(chl_realized), 
                  start = start(chl_realized))
chl_2018_10 <- window(chl_rgdp_10, start = c(2018, 1), end = c(2018, 4))
chl_2018_20 <- window(chl_rgdp_20, start = c(2018, 1), end = c(2018, 4))
chl_2018_30 <- window(chl_rgdp_30, start = c(2018, 1), end = c(2018, 4))
chl_2019_10 <- window(chl_rgdp_10, start = c(2019, 1), end = c(2019, 4))
chl_2019_20 <- window(chl_rgdp_20, start = c(2019, 1), end = c(2019, 4))
chl_2019_30 <- window(chl_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(chl_2018_10))
print(mean(chl_2018_20))
print(mean(chl_2018_30))
print(mean(chl_2019_10))
print(mean(chl_2019_20))
print(mean(chl_2019_30))



col_rgdp_20 <- col[[1]]
col_realized <- window(col_rgdp_20, end = c(2018, 3))
col_rgdp_10 <- ts(c(col_realized, col[[2]]$ave_by_h_fc), 
                  frequency = frequency(col_realized), 
                  start = start(col_realized))
col_rgdp_30 <- ts(c(col_realized, col[[4]]$ave_by_h_fc), 
                  frequency = frequency(col_realized), 
                  start = start(col_realized))
col_2018_10 <- window(col_rgdp_10, start = c(2018, 1), end = c(2018, 4))
col_2018_20 <- window(col_rgdp_20, start = c(2018, 1), end = c(2018, 4))
col_2018_30 <- window(col_rgdp_30, start = c(2018, 1), end = c(2018, 4))
col_2019_10 <- window(col_rgdp_10, start = c(2019, 1), end = c(2019, 4))
col_2019_20 <- window(col_rgdp_20, start = c(2019, 1), end = c(2019, 4))
col_2019_30 <- window(col_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(col_2018_10))
print(mean(col_2018_20))
print(mean(col_2018_30))
print(mean(col_2019_10))
print(mean(col_2019_20))
print(mean(col_2019_30))



per_rgdp_20 <- per[[1]]
per_realized <- window(per_rgdp_20, end = c(2018, 3))
per_rgdp_10 <- ts(c(per_realized, per[[2]]$ave_by_h_fc), 
                  frequency = frequency(per_realized), 
                  start = start(per_realized))
per_rgdp_30 <- ts(c(per_realized, per[[4]]$ave_by_h_fc), 
                  frequency = frequency(per_realized), 
                  start = start(per_realized))
per_2018_10 <- window(per_rgdp_10, start = c(2018, 1), end = c(2018, 4))
per_2018_20 <- window(per_rgdp_20, start = c(2018, 1), end = c(2018, 4))
per_2018_30 <- window(per_rgdp_30, start = c(2018, 1), end = c(2018, 4))
per_2019_10 <- window(per_rgdp_10, start = c(2019, 1), end = c(2019, 4))
per_2019_20 <- window(per_rgdp_20, start = c(2019, 1), end = c(2019, 4))
per_2019_30 <- window(per_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(per_2018_10))
print(mean(per_2018_20))
print(mean(per_2018_30))
print(mean(per_2019_10))
print(mean(per_2019_20))
print(mean(per_2019_30))




pry_rgdp_20 <- pry[[1]]
pry_realized <- window(pry_rgdp_20, end = c(2018, 3))
pry_rgdp_10 <- ts(c(pry_realized, pry[[2]]$ave_by_h_fc),
                  frequency = frequency(pry_realized),
                  start = start(pry_realized))
pry_rgdp_30 <- ts(c(pry_realized, pry[[4]]$ave_by_h_fc),
                  frequency = frequency(pry_realized),
                  start = start(pry_realized))
pry_2018_10 <- window(pry_rgdp_10, start = c(2018, 1), end = c(2018, 4))
pry_2018_20 <- window(pry_rgdp_20, start = c(2018, 1), end = c(2018, 4))
pry_2018_30 <- window(pry_rgdp_30, start = c(2018, 1), end = c(2018, 4))
pry_2019_10 <- window(pry_rgdp_10, start = c(2019, 1), end = c(2019, 4))
pry_2019_20 <- window(pry_rgdp_20, start = c(2019, 1), end = c(2019, 4))
pry_2019_30 <- window(pry_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(pry_2018_10))
print(mean(pry_2018_20))
print(mean(pry_2018_30))
print(mean(pry_2019_10))
print(mean(pry_2019_20))
print(mean(pry_2019_30))




ury_rgdp_20 <- ury[[1]]
ury_realized <- window(ury_rgdp_20, end = c(2018, 3))
ury_rgdp_10 <- ts(c(ury_realized, ury[[2]]$ave_by_h_fc),
                  frequency = frequency(ury_realized),
                  start = start(ury_realized))
ury_rgdp_30 <- ts(c(ury_realized, ury[[4]]$ave_by_h_fc),
                  frequency = frequency(ury_realized),
                  start = start(ury_realized))
ury_2018_10 <- window(ury_rgdp_10, start = c(2018, 1), end = c(2018, 4))
ury_2018_20 <- window(ury_rgdp_20, start = c(2018, 1), end = c(2018, 4))
ury_2018_30 <- window(ury_rgdp_30, start = c(2018, 1), end = c(2018, 4))
ury_2019_10 <- window(ury_rgdp_10, start = c(2019, 1), end = c(2019, 4))
ury_2019_20 <- window(ury_rgdp_20, start = c(2019, 1), end = c(2019, 4))
ury_2019_30 <- window(ury_rgdp_30, start = c(2019, 1), end = c(2019, 4))
print(mean(ury_2018_10))
print(mean(ury_2018_20))
print(mean(ury_2018_30))
print(mean(ury_2019_10))
print(mean(ury_2019_20))
print(mean(ury_2019_30))
