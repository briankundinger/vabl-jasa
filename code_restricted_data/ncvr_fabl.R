library(vabl)

ncvr_a <- readRDS("data/ncvr_a")
ncvr_b <- readRDS("data/ncvr_b")
S <- 1000
burn <- S * .1

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

#hash <- readRDS("out/ncvr/combine/hash")
hash <- readRDS("out/ncvr/combine/hash")

ptm <- proc.time()
chain <- fabl(hash, S = S, burn = burn)
#saveRDS(chain, "out/ncvr_combine/ncvr_chain")
seconds <- proc.time() - ptm
results <- estimate_links(chain, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)
df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                      precision = eval[2],
                      f_measure = eval[3],
                      iterations = S,
                 time = seconds[3],
                      method = "fabl",
                      data = "ncvr")
saveRDS(df, "out/case_study_results/ncvr_fabl")

trace_df <- data.frame(data = "ncvr",
                       trace = chain$overlap) %>%
  mutate(iter = row_number())

saveRDS(trace_df, "out/case_study_trace/ncvr")

