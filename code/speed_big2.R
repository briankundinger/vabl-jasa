library(vabl)
library(glue)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
n1_vals <- c(seq(500, by = 500, length.out = 9), seq(5000, by = 5000, length.out = 20))
n1 <- n1_vals[k]
n2 <- 500
total_overlap <- n2/2
S <-  1000
burn <-  S * .1

Z_true <- rep(0, n2)
Z_true[1:total_overlap] <- 1:total_overlap

m <- rep(c(.95, .05), 5)
u <- c(.01, .99, .01, .99,
       1/30, 1- 1/30, 1/12, 1 - 1/12, 1/15, 1 - 1/15)
levels <- c(2, 2, 2, 2, 2)

possible_batches <- 1:200
pair_limit = 100000000

batches <- (n1 * n2 / possible_batches < pair_limit) %>%
  which(. == T) %>%
  .[1] %>%
  possible_batches[.]

normal_batch_size <- n2 %/% batches
last_batch_size <- n2 %% batches
if(last_batch_size == 0){
  last_batch_size <- NULL
}
batch_sizes <-c(rep(normal_batch_size, batches), last_batch_size)

batches_with_overlap <- total_overlap %/% normal_batch_size
remaining_overlap <- total_overlap %% normal_batch_size
overlap_vec <- rep(0, length(batch_sizes))
overlap_vec[1:batches_with_overlap] <- normal_batch_size
overlap_vec[batches_with_overlap + 1] <- remaining_overlap

hash_list <- vector("list", length(batch_sizes))

for (i in seq_along(batch_sizes)){
  cd <- simulate_comparisons(m, u, levels, n1, batch_sizes[i], overlap_vec[i])
  hash_list[[i]] <- hash_comparisons(cd, all_patterns = T)
  gc()
}

brl_df <- NULL
if(length(batch_sizes) == 1){
  ptm <- proc.time()
  out <- BRL::bipartiteGibbs(cd, nIter = S)
  seconds <- (proc.time() - ptm)[3]
  brl_df <- data.frame(n1 = n1,
                        time = seconds,
                        iterations = S,
                        method = "BRL")
}

hash <- combine_hash(hash_list = hash_list, n1, n2)

ptm <- proc.time()
out <- fabl(hash, S=S, burn = burn)
seconds <- (proc.time() - ptm)[3]
result <- estimate_links(out, hash)
fabl_df <- data.frame(n1 = n1,
                      time = seconds,
                      iterations = S,
                      method = "fabl")

ptm <- proc.time()
out <- BRL_hash(hash, S=S, burn = burn)
seconds <- (proc.time() - ptm)[3]
result <- estimate_links(out, hash)
brl_hash_df <- data.frame(n1 = n1,
                          time = seconds,
                          iterations = S,
                          method = "BRL_hash")

ptm <- proc.time()
out <- vabl(hash)
seconds <- (proc.time() - ptm)[3]
result <- estimate_links(out, hash, resolve = F)
vabl_df <- data.frame(n1 = n1,
                      time = seconds,
                      iterations = out$t,
                      method = "vabl")

ptm <- proc.time()
out <- svabl(hash, B = n2/10, k = 1, tau = 1)
seconds <- (proc.time() - ptm)[3]
result <- estimate_links(out, hash, resolve = F)
svabl_df <- data.frame(n1 = n1,
                              time = seconds,
                              iterations = out$t,
                              method = "svabl")


df <- rbind(brl_df, brl_hash_df, fabl_df, vabl_df, svabl_df)
saveRDS(df, glue("out/speed_big2/n_{stringr::str_pad(k, width = 2, pad = 0)}"))
