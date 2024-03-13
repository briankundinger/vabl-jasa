library(RecordLinkage)
library(dplyr)
library(stringr)
library(vabl)
library(purrr)
library(readr)
library(BRL)

i <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
set.seed(41)
files <- list.files(path = "data/sadinle_sim_data/", full.names = T)

S <-  1000
burn <-  100
all_patterns <- TRUE


overlap_vals <- c(50, 250, 450)

vabl_samps <- matrix(NA, nrow = 3, ncol = 6)
vabl_partial_samps <- matrix(NA, nrow = 3, ncol = 6)

svi_samps <- matrix(NA, nrow = 3, ncol = 6)
svi_partial_samps <- matrix(NA, nrow = 3, ncol = 6)

brl_hash_samps <- matrix(NA, nrow = 3, ncol = 6)
brl_hash_partial_samps <- matrix(NA, nrow = 3, ncol = 6)

fabl_samps <- matrix(NA, nrow = 3, ncol = 6)
fabl_partial_samps <- matrix(NA, nrow = 3, ncol = 6)

sad_samps <- matrix(NA, nrow = 3, ncol = 6)
sad_partial_samps <- matrix(NA, nrow = 3, ncol = 6)

for(j in seq_along(overlap_vals)){

  overlap <- overlap_vals[j]

  records <- read_csv(files[i], col_types = cols())
  records$file <- rep(2:1, length.out = dim(records)[1])

  records <- records %>%
    janitor::clean_names() %>%
    mutate(rec_id = as.numeric(str_extract(rec_id, "\\d{3}")) + 1)

  n1 <- 500
  n2 <- 500

  Ztrue <- rep(0, n2)
  Ztrue[1:overlap] <- 1:overlap

  file1 <- records %>%
    filter(file ==1,
           rec_id <= n1) %>%
    select(-rec_id) %>%
    as.matrix(.) %>%
    data.frame(.) %>%
    mutate(occup = as.numeric(occup))

  file2 <- records %>%
    filter(file == 2,
           rec_id %in% c(1:overlap, (n1 +1):(1000 - overlap))) %>%
    select(-rec_id) %>%
    as.matrix() %>%
    data.frame(.) %>%
    mutate(occup = as.numeric(occup))

  cd <- compare_records(file1, file2, fields = c(2, 3, 5, 6),
                        types = c("lv", "lv", "bi", "bi"))

  hash <- hash_comparisons(cd, all_patterns = all_patterns)

  # vabl
  ptm <- proc.time()
  out <- vabl(hash)
  elapsed <- proc.time() - ptm
  result <- estimate_links(out, hash)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  vabl_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- estimate_links(out, hash, l_R = .1)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  RR <- sum(result$Z_hat == -1)/n2
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  vabl_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)

  # svabl
  ptm <- proc.time()
  out <- svabl(hash, threshold, tmax, B = 100, k = 1)
  elapsed <- proc.time() - ptm
  result <- estimate_links(out, hash)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  svi_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- estimate_links(out, hash, l_R = .1)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  RR <- sum(result$Z_hat == -1)/n2
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  svi_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)

  # fabl
  ptm <- proc.time()
  chain <- fabl(hash)
  elapsed <- proc.time() - ptm
  result <- estimate_links(chain, n1, 1, 1, 2, Inf)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  fabl_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- estimate_links(chain, n1, 1, 1, 2, .1)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  RR <- sum(result$Z_hat == -1)/n2
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  fabl_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)

  # BRL Hash
  ptm <- proc.time()
  chain <- BRL_hash(hash)
  elapsed <- proc.time() - ptm
  result <- estimate_links(chain, hash, 1, 1, 2, Inf)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  brl_hash_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- estimate_links(chain, hash, 1, 1, 2, .1)
  RR <- sum(result$Z_hat == -1)/n2
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  brl_hash_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)


  #Sadinle 2017 Method
  ptm <- proc.time()
  chain <- BRL::bipartiteGibbs(cd)
  elapsed <- proc.time() - ptm
  Z_hat <- BRL::linkRecords(chain$Z[, 101:1000], n1, 1, 1, 2, Inf)
  Z_hat[Z_hat > n1] <- 0
  eval <- evaluate_links(Z_hat, Ztrue, n1)
  sad_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  Z_hat <- BRL::linkRecords(chain$Z[, 101:1000], n1, 1, 1, 2, .1)
  Z_hat[Z_hat > n1] <- 0
  RR <- sum(Z_hat == -1)/n2
  eval <- evaluate_links(Z_hat, Ztrue, n1)
  eval[1] <- sum(Z_hat == Ztrue & Ztrue == 0) / (sum(Z_hat == 0))
  sad_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)
}

vabl_samps <- data.frame(vabl_samps, "vabl") %>%
  unname() %>%
  data.frame()

vabl_partial_samps <- data.frame(vabl_partial_samps, "vabl_partial") %>%
  unname() %>%
  data.frame()

svi_samps <- data.frame(svi_samps, "svi") %>%
  unname() %>%
  data.frame()

svi_partial_samps <- data.frame(svi_partial_samps, "svi_partial") %>%
  unname() %>%
  data.frame()

brl_hash_samps <- data.frame(brl_hash_samps, "brl_hash") %>%
  unname() %>%
  data.frame()

brl_hash_partial_samps <- data.frame(brl_hash_partial_samps, "brl_hash_partial") %>%
  unname() %>%
  data.frame()

fabl_samps <- data.frame(fabl_samps, "fabl") %>%
  unname() %>%
  data.frame()
fabl_partial_samps <- data.frame(fabl_partial_samps, "fabl_partial") %>%
  unname() %>%
  data.frame()
sad_samps <- data.frame(sad_samps, "BRL") %>%
  unname() %>%
  data.frame()
sad_partial_samps <- data.frame(sad_partial_samps, "BRL_partial") %>%
  unname() %>%
  data.frame()

result_df <- rbind(vabl_samps, vabl_partial_samps,
                   svi_samps, svi_partial_samps,
                   brl_hash_samps, brl_hash_partial_samps,
                   fabl_samps, fabl_partial_samps,
                   sad_samps, sad_partial_samps)

if(i < 100){
  error <- "One Error"
} else if (i < 200) {
  error <- "Two Errors"
} else {
  error <- "Three Errors"
}
names(result_df) <- c("recall", "precision", "f-measure", "RR",
                      "time", "overlap", "method")
result_df$error <- error

saveRDS(result_df, file = paste0("out/sadinle_sim/sim_",
                                      str_pad(i, 3, pad = "0")))
