library(dplyr)

ncvr <- read.csv("data/ncvr.csv")

ncvr_a <- ncvr %>%
  filter(file_id == "a") %>%
  filter(!duplicated(voter_id)) %>%
  arrange(voter_id)

ncvr_b <- ncvr %>%
  filter(file_id == "b") %>%
  filter(!duplicated(voter_id)) %>%
  arrange(voter_id)

saveRDS(ncvr_a, "data/ncvr_a")
saveRDS(ncvr_b, "data/ncvr_b")
