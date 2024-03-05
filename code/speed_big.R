library(fabldev)
library(glue)

k = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
n1_vals <- c(seq(500, by = 500, length.out = 9), seq(5000, by = 5000, length.out = 20))
n1 <- n2 <- n1_vals[k]
total_overlap <- n2/2
S = 1000
burn = S * .1

Z_true <- rep(0, n2)
Z_true[1:total_overlap] <- 1:total_overlap

show_progress <- T
fast = F
R <- NULL
all_patterns <- T

m <- c(.05, .95, .05, .95, .05, .95, .05, .95, .05, .95)

u <- c(.99, .01, .99, .01,
       1 - 1/30, 1/30, 1 - 1/12, 1/12, 1 - 1/15, 1/15)

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
previous_matches <- 0

hash_list <- vector("list", length(batch_sizes))

for (i in seq_along(batch_sizes)){
  cd <- simulate_comparisons(m, u, levels, n1, batch_sizes[i], overlap_vec[i],
                             previous_matches)
  hash_list[[i]] <- hash_comparisons(cd, all_patterns = T)
  #rm(cd)
  gc()
  previous_matches <- previous_matches + overlap_vec[i]
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

brl_hash_df <- NULL

ptm <- proc.time()
out <- brl_efficient_serge(hash, S=S, burn = burn, reject_iter = 10)
seconds <- (proc.time() - ptm)[3]
result <- estimate_links(out$Z, n1)
brl_hash_df <- data.frame(n1 = n1,
                      time = seconds,
                      iterations = S,
                      method = "BRLhash")

ptm <- proc.time()
out <- gibbs_efficient(hash, S=S, burn = burn)
seconds <- (proc.time() - ptm)[3]
result <- estimate_links(out$Z, n1)
fabl_df <- data.frame(n1 = n1,
                          time = seconds,
                          iterations = S,
                          method = "fabl")

ptm <- proc.time()
out <- vi_efficient(hash)
seconds <- (proc.time() - ptm)[3]
result <- vi_estimate_links(out, hash, resolve = F)
vabl_df <- data.frame(n1 = n1,
                      time = seconds,
                      iterations = out$t,
                      method = "vabl")

# ptm <- proc.time()
# out <- vi_efficient(hash, b_init = F)
# seconds <- (proc.time() - ptm)[3]
# result <- vi_estimate_links(out, hash, resolve = F)
# vabl_no_init_df <- data.frame(n1 = n1,
#                       time = seconds,
#                       iterations = out$t,
#                       method = "vabl_no_init")

svi_df <- NULL
  ptm <- proc.time()
  out <- svi_efficient(hash, B = min(n1/10, 1000), k = 1, tau = 1)
  seconds <- (proc.time() - ptm)[3]
  result <- vi_estimate_links(out, hash, resolve = F)
  svi_df <- data.frame(n1 = n1,
                                time = seconds,
                                iterations = out$t,
                                method = "svabl")

df <- rbind(brl_df, brl_hash_df, fabl_df, vabl_df, svi_df)
saveRDS(df, glue("out/speed_big/n_{stringr::str_pad(k, width = 2, pad = 0)}"))
