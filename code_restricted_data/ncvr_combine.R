library(vabl)
library(glue)
library(tictoc)

ncvr_a <- readRDS("data/ncvr_a")
ncvr_b <- readRDS("data/ncvr_b")

n1 <- nrow(ncvr_a)
n2 <- nrow(ncvr_b)

files <- list.files("out/ncvr/hash/", full.names = T)
hash_list <- lapply(files, readRDS)
start <- tic()
hash <- combine_hash(hash_list, n1, n2)
combine_time <- unname(toc(quiet = T)$toc - start)
combine_df <- data.frame(data = "NCVR",
                         combine_time = combine_time)

saveRDS(combine_df, "out/case_study_combine_time/ncvr")

saveRDS(hash, "out/ncvr/combine/hash")
