library(vabl)
library(glue)

k = as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
args <- commandArgs(trailingOnly = TRUE)
if(length(args) == 0){
  threshold <- 1e-6
} else {
  threshold <- args[1]
}


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
hash <- readRDS("../../../../../usr/xtmp/bak47/ncvr/combine/vabl/hash")
kappa_vec <- c(.5, .6, .7, .8, .9, 1)
batch <- 20000
tau <-  1
if (k <7){


out <- svabl(hash, B = batch, k = kappa_vec[k], threshold = threshold,
                     tau = 1, seed = 5, fixed_iterations = 200)
results <- estimate_links(out, hash)
eval <- evaluate_links(results$Z_hat, Z_true, n1)

svabl_df <- data.frame(n1 = n1,
                     n2 = n2,
                     recall = eval[1],
                     precision = eval[2],
                     f_measure = eval[3],
                     iterations = out$t,
                     threshold = threshold,
                     kappa = kappa_vec[k])

svabl_elbo <- data.frame(elbo = out$elbo,
                       iter = seq_along(out$elbo),
                       threshold = threshold,
                       kappa = kappa_vec[k])


saveRDS(svabl_df, glue("out/ncvr_kappa_result_threshold/result_df_{kappa_vec[k]}_{threshold}"))
saveRDS(svabl_elbo, glue("out/ncvr_kappa_elbo_threshold/elbo_df_{kappa_vec[k]}_{threshold}"))
}
if(k == 7){

  out <- vabl(hash, fixed_iterations = 200, threshold = threshold)
  results <- estimate_links(out, hash)
  eval <- evaluate_links(results$Z_hat, Z_true, n1)

  svabl_df <- data.frame(n1 = n1,
                       n2 = n2,
                       recall = eval[1],
                       precision = eval[2],
                       f_measure = eval[3],
                       iterations = out$t,
                       threshold = threshold
                       kappa = "vabl")

  svabl_elbo <- data.frame(elbo = out$elbo,
                         iter = seq_along(out$elbo),
                         threshold = threshold
                         kappa = "vabl")


  saveRDS(svabl_df, glue("out/ncvr_kappa_result_threshold/result_df_vabl_{threshold}"))
  saveRDS(svabl_elbo, glue("out/ncvr_kappa_elbo_threshold/elbo_df_vabl_{threshold}"))
}
