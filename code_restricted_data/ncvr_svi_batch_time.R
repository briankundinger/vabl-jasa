library(vabl)
library(glue)

k = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
args <- commandArgs(trailingOnly=TRUE)
if(length(args) == 0){
exponent <- 6
} else {
  exponent <- as.integer(args[1])
}
threshold <- 1 * 10 ^ (-exponent)

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

hash <- readRDS("out/ncvr/combine/hash")
#hash <- readRDS("../../../../../usr/xtmp/bak47/ncvr/combine/vabl/hash")

batch_sizes <- c(1, 100, 1000, 2500, 5000, 10000, 20000, 100000)

if(k == length(batch_sizes) + 1){
ptm <- proc.time()
out <- vabl(hash, tmax = 10000, threshold = threshold)
seconds <- proc.time() - ptm
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                 precision = eval[2],
                 f_measure = eval[3],
                 iterations = out$t,
                 time = seconds[3],
                 threshold = threshold,
                 batch = n1,
                 method = "vabl",
                 data = "ncvr")

saveRDS(df, glue("out/ncvr_batch_time/vabl_{threshold}"))

elbo_df <- data.frame(data = "NCVR",
                      elbo = out$elbo_seq) %>%
  mutate(iter = row_number())

saveRDS(elbo_df, glue("out/ncvr_batch_elbo/vabl_{threshold}"))
}

if (k <= length(batch_sizes)){
  batch <- batch_sizes[k]
ptm <- proc.time()
out <- svabl(hash, B = batch, k = 1, tau = 1, threshold = threshold,
                      seed = 4, tmax = 10000)
seconds <- proc.time() - ptm
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

df <- data.frame(n1 = n1,
                 n2 = n2,
                 recall = eval[1],
                 precision = eval[2],
                 f_measure = eval[3],
                 iterations = out$t,
                 time = seconds[3],
                 threshold = threshold,
                 batch = batch,
                 method = paste0("svi_", paste0(batch)),
                 data = "ncvr")

saveRDS(df, glue("out/ncvr_batch_time/svi_{stringr::str_pad(batch, 5, pad = 0)}_{threshold}"))

elbo_df <- data.frame(data = "NCVR",
                      elbo = out$elbo_seq) %>%
  mutate(iter = row_number())

saveRDS(elbo_df, glue("out/ncvr_batch_elbo/svi_{stringr::str_pad(batch, 5, pad = 0)}_{threshold}"))
}



