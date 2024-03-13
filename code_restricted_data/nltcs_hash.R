library(vabl)
library(tictoc)
library(glue)

k <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID"))

nltcs <- read.csv("data/proc_nltcs.csv")
df82 <- nltcs %>%
  filter(FILE == 82) %>%
  select(-FILE, -SEQ) %>%
  mutate(unique_ID = stringr::str_sub(REC, start = 3L),
         DOB_DAY = stringr::str_pad(DOB_DAY, 2, pad = "0"),
         DOB_MONTH = stringr::str_pad(DOB_MONTH, 2, pad = "0"),
         DOB_YEAR = stringr::str_pad(DOB_YEAR, 2, pad = "0"),
         STATE = stringr::str_pad(STATE, 2, pad = "0"),
         REGOFF = stringr::str_pad(REGOFF, 2, pad = "0")) %>%
  #tidyr::unite(dob, 2:4, sep = "") %>%
  tidyr::unite(location, c(STATE, REGOFF), sep = "")


df89 <- nltcs %>%
  filter(FILE == 89) %>%
  select(-FILE, -SEQ) %>%
  mutate(unique_ID = stringr::str_sub(REC, start = 3L),
         DOB_DAY = stringr::str_pad(DOB_DAY, 2, pad = "0"),
         DOB_MONTH = stringr::str_pad(DOB_MONTH, 2, pad = "0"),
         DOB_YEAR = stringr::str_pad(DOB_YEAR, 2, pad = "0"),
         STATE = stringr::str_pad(STATE, 2, pad = "0"),
         REGOFF = stringr::str_pad(REGOFF, 2, pad = "0")) %>%
  #tidyr::unite(dob, 2:4, sep = "") %>%
  tidyr::unite(location, c(STATE, REGOFF), sep = "")


chunks <- 30
chunk_size <- ceiling(dim(df89)[1]/chunks)
chunk_id <- rep(1:chunks, each = chunk_size)[1:dim(df89)[1]]

chunk_marker <- chunk_id == k
chunk89 <- df89[chunk_marker, ]

start <- tic()
cd <- fabldev:::compare_nltcs(df82, chunk89)
compare_time <- unname(toc(quiet = T)$toc - start)
start <- tic()
hash <- hash_comparisons(cd, all_patterns = TRUE)
hash_time <- unname(toc(quiet = T)$toc - start)

time_df <- data.frame(batch = k,
                      data = "NLTCS",
                      comparison = compare_time,
                      hash = hash_time)

saveRDS(time_df, glue("out/case_study_time/nltcs_{k}"))

saveRDS(hash, file = paste0("out/nltcs/hash/", "hash_",
                            stringr::str_pad(k, 2, pad = "0")))
