library(RecordLinkage)
library(dplyr)
library(stringr)
library(fabldev)
library(purrr)
library(readr)
library(BRL)
library(fastLink)

taskID <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))
i = taskID
set.seed(41)
files <- list.files(path = "data/sadinle_sim_data/", full.names = T)

m_prior = 1
u_prior = 1
alpha = 1
beta = 1
S = 1000
burn = 100
show_progress = F
fast = F
R = NULL
all_patterns = TRUE
tmax= 200
threshold = 1e-6
resolve = T


overlap_vals <- c(50, 250, 450)

fastlink_samps <- matrix(NA, nrow = 3, ncol = 6)
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
  #overlap <- n2/2

  # Ztrue <- n1 + 1:n2
  # Ztrue[1:overlap] <- 1:overlap

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

  out_fl <- fastLink(file1, file2, varnames = names(file1)[c(2, 3, 5, 6)],
                     stringdist.match = names(file1)[c(2, 3)],
                     partial.match = names(file1)[c(2, 3)],
                     cut.a = 1, cut.p = .75, threshold.match = .5)

  MakeZhat_from_fastlink <- function(id_1, id_2){
    Zhat <- rep(0, n2)
    Zhat[id_1] <- id_2
    Zhat
  }

  Zhat <- MakeZhat_from_fastlink(out_fl$matches$inds.a, out_fl$matches$inds.b)

  eval <- evaluate_links(Zhat, Ztrue, n1)
 fastlink_samps[j, ] <- c(eval, NA, NA, overlap)


  cd <- compare_records(file1, file2, c(2, 3, 5, 6),
                        types = c("lv", "lv", "bi", "bi"))
                        #breaks = c(0, .25))
  cd[[1]] <- apply(cd[[1]], 2, as.numeric)


  hash <- hash_comparisons(cd, all_patterns, algorithm = c("vabl", "fabl", "brl"))

  # vabl
  ptm <- proc.time()
  out <- vi_efficient(hash, threshold, tmax)
  elapsed <- proc.time() - ptm
  result <- vi_estimate_links(out, hash)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  vabl_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- vi_estimate_links(out, hash, lR = .1)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  RR <- sum(result$Z_hat == -1)/n2
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  vabl_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)

  #svi
  ptm <- proc.time()
  out <- svi_efficient(hash, threshold, tmax, B = 100, k = 1)
  elapsed <- proc.time() - ptm
  result <- vi_estimate_links(out, hash)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  svi_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- vi_estimate_links(out, hash, lR = .1)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  RR <- sum(result$Z_hat == -1)/n2
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  svi_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)

  # fabl
  ptm <- proc.time()
  Zchain <- gibbs_efficient(hash)
  elapsed <- proc.time() - ptm
  result <- estimate_links(Zchain[[1]][, 101:1000], n1, 1, 1, 2, Inf)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  fabl_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- estimate_links(Zchain[[1]][,101:1000], n1, 1, 1, 2, .1)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  RR <- sum(result$Z_hat == -1)/n2
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  fabl_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)

  # BRL Hash
  ptm <- proc.time()
  Zchain <- brl_efficient_serge(hash)
  elapsed <- proc.time() - ptm
  result <- estimate_links(Zchain[[1]][, 101:1000], n1, 1, 1, 2, Inf)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  brl_hash_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- estimate_links(Zchain[[1]][, 101:1000], n1, 1, 1, 2, .1)
  RR <- sum(result$Z_hat == -1)/n2
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  brl_hash_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)


  #Sadinle 2017 Method
  ptm <- proc.time()
  Zchain <- BRL::bipartiteGibbs(cd)[[1]]
  elapsed <- proc.time() - ptm
  result <- estimate_links(Zchain[, 101:1000], n1, 1, 1, 2, Inf)
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  sad_samps[j, ] <- c(eval, NA, elapsed[3], overlap)

  result <- estimate_links(Zchain[, 101:1000], n1, 1, 1, 2, .1)
  RR <- sum(result$Z_hat == -1)/n2
  eval <- evaluate_links(result$Z_hat, Ztrue, n1)
  eval[1] <- sum(result$Z_hat == Ztrue & Ztrue == 0) / (sum(result$Z_hat == 0))
  sad_partial_samps[j, ] <- c(eval, RR, elapsed[3], overlap)

  #  print(i)
  #}
}
fastlink_samps <- data.frame(fastlink_samps, "fastLink") %>%
  unname() %>%
  data.frame()
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

result_df <- rbind(fastlink_samps,
                   vabl_samps, vabl_partial_samps,
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
