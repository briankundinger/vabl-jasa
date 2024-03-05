library(knitr)
library(kableExtra)
library(fabldev)
library(ggplot2)
files <- list.files("out/case_study_results/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .) %>%
  mutate(method = as.factor(method)) %>%
  group_by(data) %>%
  tidyr::complete(method) %>%
  ungroup() %>%
  mutate(n2 = case_when(
    data == "ncvr"~ 222207,
    data == "nltcs" ~ 17466,
    data == "RLdata500" ~ 250,
    data == "RLdata10000" ~ 5000,
    data == "febrl_4" ~ 5000
  )) %>%
  arrange(n2, data, method) %>%
  relocate(method, .after = n2)

rownames(results) <- NULL

results %>%
  select(-f_measure, -iterations) %>%
  filter(data != "RLdata500") %>%
  kable(digits = 2, ) %>%
  kable_classic() %>%
  kable_styling(full_width = F) %>%
  save_kable(file = "figures/case_study_table.png")

results %>%
  select(-f_measure, -iterations) %>%
  filter(data != "RLdata500") %>%
  kable("latex", digits = 2)


files <- list.files("out/case_study_time/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

temp <- results %>%
  group_by(data) %>%
  summarize(compare_time = mean(comparison),
            hash_time = mean(hash))

temp$batches <- c(1, 988, 30, 1, 1, 1)
temp %>%
  filter(data != "RLdata500") %>%
  arrange(batches) %>%
  relocate(batches, .after = data) %>%
  kable("latex", digits = 1)

files <- list.files("out/case_study_combine_time/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

results

files <- list.files("out/case_study_elbo/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

datasets <- unique(results$data)

for(x in datasets){
  results %>%
    filter(data == x) %>%
    ggplot() +
    aes(x = iter, y = elbo) +
    geom_line() +
    theme_bw() +
    scale_y_continuous(labels = scales::scientific) +
    labs(y = "ELBO",
         x = "Iteration")

  ggsave(glue("figures/case_studies/elbo/{x}.png"))
}

files <- list.files("out/case_study_trace/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

datasets <- unique(results$data)

for(x in datasets){
  results %>%
    filter(iter > 100) %>%
    filter(data == x) %>%
    ggplot() +
    aes(x = iter, y = trace) +
    geom_line() +
    theme_bw() +
    labs(y = "Overlap",
         x = "Iteration")

  ggsave(glue("figures/case_studies/trace/{x}.png"))
}

files <- list.files("out/case_study_sv/", full.names = T)
results <- lapply(files, readRDS) %>%
  do.call(rbind, .)

rownames(results) <- NULL

results %>%
  relocate(data, .before = n1) %>%
  relocate(method, .after = n2) %>%
  select(-overlap, -iterations) %>%
  kable("latex", digits = 1)
