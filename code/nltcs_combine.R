library(vabl)
library(glue)
library(tictoc)

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
  tidyr::unite(dob, 2:4, sep = "") %>%
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
  tidyr::unite(dob, 2:4, sep = "") %>%
  tidyr::unite(location, c(STATE, REGOFF), sep = "")

n1 <- nrow(df82)
n2 <- nrow(df89)

files <- list.files("out/nltcs/hash/", full.names = T)
hash_list <- lapply(files, readRDS)
start <- tic()
hash <- combine_hash(hash_list, n1, n2)
combine_time <- unname(toc(quiet = T)$toc - start)
combine_df <- data.frame(data = "NLTCS",
                         combine_time = combine_time)

saveRDS(combine_df, "out/case_study_combine_time/nltcs")
saveRDS(hash, "out/nltcs/combine/hash")
