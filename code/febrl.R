library(vabl)
library(glue)
library(tictoc)


k = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

df1 <- read.csv("data/febrl_4_A.csv") %>%
  arrange(rec_id) %>%
  mutate(rec_id = row_number())
df2 <- read.csv("data/febrl_4_B.csv") %>%
  arrange(rec_id) %>%
  mutate(rec_id = row_number())

n1 <- nrow(df1)
n2 <- nrow(df2)

Z_true <- seq(1:5000)
fields <- c(2, 3, 8, 10, 11)
types <- c("lv", "lv","bi", "bi", "bi")

start <- tic()
cd <- compare_records(df1, df2, fields = fields, types = types,
                      breaks = c(0, .15))
compare_time <- unname(toc(quiet = T)$toc - start)

start <- tic()
hash <- hash_comparisons(cd, all_patterns = F)
hash_time <- unname(toc(quiet = T)$toc - start)

time_df <- data.frame(batch = k,
                      data = "FEBRL4",
                      comparison = compare_time,
                      hash = hash_time)
saveRDS(time_df, glue("out/case_study_time/FEBRL4_{k}"))

ptm <- proc.time()
chain <- BRL::bipartiteGibbs(cd)
seconds <- proc.time() - ptm
results <- estimate_links(chain, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

brl_df <- data.frame(n1 = n1,
                     n2 = n2,
                     recall = eval[1],
                     precision = eval[2],
                     f_measure = eval[3],
                     iterations = 1000,
                     time = seconds[3],
                     method = "BRL",
                     data = "febrl_4")

ptm <- proc.time()
chain <- BRL_hash(hash, S = 100)
seconds <- proc.time() - ptm
results <- estimate_links(chain, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

brl_efficient_df <- data.frame(n1 = n1,
                     n2 = n2,
                     recall = eval[1],
                     precision = eval[2],
                     f_measure = eval[3],
                     iterations = 1000,
                     time = seconds[3],
                     method = "BRL_efficient",
                     data = "febrl_4")



ptm <- proc.time()
chain <- fabl(hash, S = 100)
seconds <- proc.time() - ptm
results <- estimate_links(chain, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

fabl_df <- data.frame(n1 = n1,
                      n2 = n2,
                      recall = eval[1],
                      precision = eval[2],
                      f_measure = eval[3],
                      iterations = 1000,
                      time = seconds[3],
                      method = "fabl",
                      data = "febrl_4")
#
# ptm <- proc.time()
# chain <- fabl_serge(hash)
# seconds <- proc.time() - ptm
# results <- estimate_links(chain$Z, n1)
# eval <- evaluate_links(results$Z_hat, Z_true, n1)
#
# fabl_serge_df <- data.frame(n1 = n1,
#                             n2 = n2,
#                             recall = eval[1],
#                             precision = eval[2],
#                             f_measure = eval[3],
#                             iterations = 1000,
#                             time = seconds[3],
#                             method = "fabl_serge",
#                             data = "febrl_4")
ptm <- proc.time()
out <- vabl(hash)
seconds <- proc.time() - ptm
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

vabl_df <- data.frame(n1 = n1,
                      n2 = n2,
                      recall = eval[1],
                      precision = eval[2],
                      f_measure = eval[3],
                      iterations = out$t,
                      time = seconds[3],
                      method = "vabl",
                      data = "febrl_4")

elbo_df <- data.frame(data = "FEBRL4",
                      elbo = out$elbo_seq) %>%
  mutate(iter = row_number())

saveRDS(elbo_df, "out/case_study_elbo/FEBRL4")

trace_df <- data.frame(data = "FEBRL4",
                      trace = chain$overlap) %>%
  mutate(iter = row_number())

saveRDS(trace_df, "out/case_study_trace/FEBRL4")


# SVI

ptm <- proc.time()
out <- svabl(hash, B = 100, k = 1, tau = 1)
seconds <- proc.time() - ptm
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

svi_100_df <- data.frame(n1 = n1,
                         n2 = n2,
                         recall = eval[1],
                         precision = eval[2],
                         f_measure = eval[3],
                         iterations = out$t,
                         time = seconds[3],
                         method = "svi_100",
                         data = "febrl_4")

ptm <- proc.time()
out <- svabl(hash, B = 500, k = 1, tau = 1)
seconds <- proc.time() - ptm
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

svi_500_df <- data.frame(n1 = n1,
                         n2 = n2,
                         recall = eval[1],
                         precision = eval[2],
                         f_measure = eval[3],
                         iterations = out$t,
                         time = seconds[3],
                         method = "svi_500",
                         data = "febrl_4")

df <- rbind(brl_df, brl_efficient_df, fabl_df, vabl_df, svi_100_df, svi_500_df)
saveRDS(df, "out/case_study_results/febrl")
