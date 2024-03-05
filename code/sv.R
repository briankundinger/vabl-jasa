library(fabldev)
library(glue)

k <-  as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cd <- readRDS("data/sv_comparisons.rds")

n1 <- cd$n1
n2 <- cd$n2

ptm <- proc.time()
hash <- hash_comparisons(cd, all_patterns = F)
hash_time <- proc.time() - ptm

time_df <- data.frame(batch = k,
                      data = "sv",
                      comparison = NA,
                      hash = hash_time[3])
saveRDS(time_df, glue("out/case_study_time/sv"))

ptm <- proc.time()
chain <- BRL::bipartiteGibbs(cd)
seconds <- proc.time() - ptm
results <- estimate_links(chain$Z, n1)
overlap <- sum(results$Z_hat > 0)
DID <- n1 + n2 - overlap

brl_df <- data.frame(n1 = n1,
                     n2 = n2,
                     overlap = overlap,
                     DID = DID,
                     iterations = 1000,
                     time = seconds[3],
                     method = "BRL",
                     data = "sv")

ptm <- proc.time()
chain <- brl_efficient_serge(hash, reject_iter = 10)
seconds <- proc.time() - ptm
results <- estimate_links(chain$Z, n1)
overlap <- sum(results$Z_hat > 0)
DID <- n1 + n2 - overlap

brl_hash_df <- data.frame(n1 = n1,
                      n2 = n2,
                      overlap = overlap,
                      DID = DID,
                      iterations = 1000,
                      time = seconds[3],
                      method = "BRLhash",
                      data = "sv")

ptm <- proc.time()
chain <- gibbs_efficient(hash)
seconds <- proc.time() - ptm
results <- estimate_links(chain$Z, n1)
overlap <- sum(results$Z_hat > 0)
DID <- n1 + n2 - overlap

fabl_df <- data.frame(n1 = n1,
                      n2 = n2,
                      overlap = overlap,
                      DID = DID,
                      iterations = 1000,
                      time = seconds[3],
                      method = "fabl",
                      data = "sv")
ptm <- proc.time()
out <- vi_efficient(hash)
seconds <- proc.time() - ptm
results <- vi_estimate_links(out, hash)
overlap <- sum(results$Z_hat > 0)
DID <- n1 + n2 - overlap

vabl_df <- data.frame(n1 = n1,
                     n2 = n2,
                     overlap = overlap,
                     DID = DID,
                     iterations = 1000,
                     time = seconds[3],
                     method = "vabl",
                     data = "sv")

ptm <- proc.time()
out <- svi_efficient(hash, B = 100)
seconds <- proc.time() - ptm
results <- vi_estimate_links(out, hash)
overlap <- sum(results$Z_hat > 0)
DID <- n1 + n2 - overlap

svi_df <- data.frame(n1 = n1,
                      n2 = n2,
                      overlap = overlap,
                      DID = DID,
                      iterations = 1000,
                      time = seconds[3],
                      method = "svi",
                      data = "sv")

df <- rbind(brl_df, brl_hash_df, fabl_df, vabl_df, svi_df)
saveRDS(df, "out/case_study_sv/sv")

elbo_df <- data.frame(data = "SV",
                      elbo = out$elbo_seq) %>%
  mutate(iter = row_number())

saveRDS(elbo_df, "out/case_study_elbo/sv")

trace_df <- data.frame(data = "SV",
                       trace = chain$overlap) %>%
  mutate(iter = row_number())

saveRDS(trace_df, "out/case_study_trace/sv")
