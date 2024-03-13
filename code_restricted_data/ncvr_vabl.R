library(vabl)

ncvr_a <- readRDS("data/ncvr_a")
ncvr_b <- readRDS("data/ncvr_b")

df1 <- ncvr_a %>%
  select(voter_id) %>%
  mutate(rn = row_number()) %>%
  arrange(voter_id)

df2 <- ncvr_b %>%
  select(voter_id) %>%
  arrange(voter_id)

n1 <- nrow(df1)
n2 <- nrow(df2)

joined <- right_join(df1, df2, by = "voter_id", copy = T, keep = T) %>%
  arrange(voter_id.y)
joined$rn[is.na(joined$rn)] <- 0
Z_true <- joined$rn

#hash <- readRDS("out/ncvr_combine/hash")
hash <- readRDS("out/ncvr/combine/hash")

ptm <- proc.time()
out <- vabl(hash, tmax = 500)
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
                 data = "ncvr")

#saveRDS(df, "out/case_study_results/ncvr_vabl")

elbo_df <- data.frame(data = "NCVR",
                      elbo = out$elbo_seq) %>%
  mutate(iter = row_number())

saveRDS(elbo_df, "out/case_study_elbo/ncvr")

ptm <- proc.time()
out <- svabl(hash, B = 1000, k = 1, tau = 1)
seconds <- proc.time() - ptm
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

svabl_1000_df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                 precision = eval[2],
                 f_measure = eval[3],
                 iterations = out$t,
                 time = seconds[3],
                 method = "svabl_1000",
                 data = "ncvr")

#saveRDS(df, "out/case_study_results/ncvr_svabl_1000")

elbo_df <- data.frame(data = "NCVR",
                      elbo = out$elbo_seq) %>%
  mutate(iter = row_number())

saveRDS(elbo_df, "out/case_study_elbo/ncvr_svabl_1000")

ptm <- proc.time()
out <- svabl(hash, B = 5000, k = 1, tau = 1)
seconds <- proc.time() - ptm
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

svabl_5000_df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                 precision = eval[2],
                 f_measure = eval[3],
                 iterations = out$t,
                 time = seconds[3],
                 method = "svabl_5000",
                 data = "ncvr")

#saveRDS(df, "out/case_study_results/ncvr_svabl_5000")

elbo_df <- data.frame(data = "NCVR",
                      elbo = out$elbo_seq) %>%
  mutate(iter = row_number())

df <- rbind(vabl_df, svabl_1000_df, svabl_5000_df)
saveRDS(df, "out/case_study_results/ncvr")


saveRDS(elbo_df, "out/case_study_elbo/ncvr_svabl_5000")




