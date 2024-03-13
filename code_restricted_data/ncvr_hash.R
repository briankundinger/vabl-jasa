library(vabl)
library(glue)
library(tictoc)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
ncvr_a <- readRDS("data/ncvr_a")
ncvr_b <- readRDS("data/ncvr_b")

n1 <- nrow(ncvr_a)
n2 <- nrow(ncvr_b)

normal_batches <- n2 %/% 225
last_batch <- n2 %% 225

batch_id <-c(rep(1:normal_batches, each = 225), rep(normal_batches + 1, last_batch))
batch <- ncvr_b[batch_id == k, ]

fields <- c(4, 5, 6, 7, 9, 10, 13)
types <- c("bi", "bi", "bi", "bi", "bi", "bi", "bi")

start <- tic()
cd <- compare_records(ncvr_a, batch, flds = fields, types = types)
compare_time <- unname(toc(quiet = T)$toc - start)
start <- tic()
hash <- hash_comparisons(cd, all_patterns = T, R = 3,
                         method = c("vabl", "fabl"))

hash_time <- unname(toc(quiet = T)$toc - start)

time_df <- data.frame(batch = k,
                      data = "NCVR",
                      comparison = compare_time,
                      hash = hash_time)
saveRDS(time_df, glue("out/case_study_time/ncvr_{k}"))
# paste0("out/ncvr/hash/", "hash_",
#        stringr::str_pad(k, 2, pad = "0"))
saveRDS(hash, paste0("out/ncvr/hash/", "hash_",
                     stringr::str_pad(k, 4, pad = "0")))
