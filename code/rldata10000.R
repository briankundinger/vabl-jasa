library(fabldev)
library(RecordLinkage)
library(glue)
library(tictoc)

k = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))


data <- RecordLinkage::RLdata10000 %>%
  mutate(unique_id = RecordLinkage::identity.RLdata10000)

duplicates <- nrow(data) * .1

duplicated_ids <- data %>%
  filter(duplicated(unique_id)) %>%
  select(unique_id) %>%
  pull()

duplicated_records <- data %>%
  filter(unique_id %in% duplicated_ids) %>%
  arrange(unique_id) %>%
  mutate(rn = row_number())

duplicated_1 <- duplicated_records %>%
  filter(rn %% 2 == 0)

duplicated_2 <- duplicated_records %>%
  filter(rn %% 2 == 1)

non_duplicated_records <- data %>%
  filter(!(unique_id %in% duplicated_ids)) %>%
  mutate(rn = row_number())

non_duplicated_1 <- non_duplicated_records %>%
  filter(rn %% 2 == 0)

non_duplicated_2 <- non_duplicated_records %>%
  filter(rn %% 2 == 1)

df1 <- rbind(duplicated_1, non_duplicated_1)
df2 <- rbind(duplicated_2, non_duplicated_2)
n1 <- nrow(df1)
n2 <- nrow(df2)
Z_true <- rep(0, nrow(df1))
Z_true[1:duplicates] <- 1:duplicates

fields <- c(1, 3, 5, 6, 7)
types <- c("lv", "lv", "bi", "bi", "bi")

start <- tic()
cd <- compare_records(df1, df2, flds = fields, types = types,
                      breaks = c(0, .15))
compare_time <- unname(toc(quiet = T)$toc - start)

start <- tic()
hash <- hash_comparisons(cd, all_patterns = F)
hash_time <- unname(toc(quiet = T)$toc - start)

time_df <- data.frame(batch = k,
                      data = "RLdata10000",
                      comparison = compare_time,
                      hash = hash_time)
saveRDS(time_df, glue("out/case_study_time/RLdata10000_{k}"))

ptm <- proc.time()
chain <- BRL::bipartiteGibbs(cd)
seconds <- proc.time() - ptm
results <- estimate_links(chain$Z, n1)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

brl_df <- data.frame(n1 = n1,
                     n2 = n2,
                     recall = eval[1],
                     precision = eval[2],
                     f_measure = eval[3],
                     iterations = 1000,
                     time = seconds[3],
                     method = "BRL",
                     data = "RLdata10000")

ptm <- proc.time()
chain <- BRL_hash(hash)
seconds <- proc.time() - ptm
results <- estimate_links(chain, n1)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

brl_efficient_df <- data.frame(n1 = n1,
                               n2 = n2,
                               recall = eval[1],
                               precision = eval[2],
                               f_measure = eval[3],
                               iterations = 1000,
                               time = seconds[3],
                               method = "BRL_hash",
                               data = "RLdata10000")

ptm <- proc.time()
chain <- fabl(hash)
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
                      data = "RLdata10000")

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
                      data = "RLdata10000")

elbo_df <- data.frame(data = "RLdata10000",
                      elbo = out$elbo_seq) %>%
  mutate(iter = row_number())

saveRDS(elbo_df, "out/case_study_elbo/RLdata10000")

trace_df <- data.frame(data = "RLdata10000",
                       trace = chain$overlap) %>%
  mutate(iter = row_number())

saveRDS(trace_df, "out/case_study_trace/RLdata10000")


# svabl

ptm <- proc.time()
out <- svabl(hash, B = 100, k = 1, tau = 1)
seconds <- proc.time() - ptm
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

svabl_100_df <- data.frame(n1 = n1,
                      n2 = n2,
                      recall = eval[1],
                      precision = eval[2],
                      f_measure = eval[3],
                      iterations = out$t,
                      time = seconds[3],
                      method = "svabl_100",
                      data = "RLdata10000")

ptm <- proc.time()
out <- svabl(hash, B = 500, k = 1, tau = 1)
seconds <- proc.time() - ptm
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

svabl_500_df <- data.frame(n1 = n1,
                         n2 = n2,
                         recall = eval[1],
                         precision = eval[2],
                         f_measure = eval[3],
                         iterations = out$t,
                         time = seconds[3],
                         method = "svabl_500",
                         data = "RLdata10000")

df <- rbind(brl_df, brl_efficient_df, fabl_df, vabl_df, svabl_100_df, svabl_500_df)
saveRDS(df, "out/case_study_results/RLdata10000")


