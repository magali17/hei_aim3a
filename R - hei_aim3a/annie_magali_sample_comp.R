rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))}

# Load pacman into memory, installing as needed
my_repo <- 'http://cran.r-project.org'
if (!require("pacman")) {install.packages("pacman", repos = my_repo)}

pacman::p_load(tidyverse)

qc_path <- "Output/v3_20230321/onroad/qc/annie_magali_comp"
if(!dir.exists(qc_path)) dir.create(qc_path)

################################################################################################

a1 <- readRDS("data/onroad/annie/PNC_nonspatial_annavgs.rds") %>%
  filter(#version=="all hours",
         design== "balanced",
         #grepl("4|12", visits),
         adjusted=="unadjusted")

a1 <- rename(a1, annual_mean_annie = annual_mean)

# distinct(a1, version, design, visits, adjusted)

m1 <- readRDS("data/onroad/annie/v2/site_avgs/nonspatial/unadjusted_12_balanced_allhours.rds") %>%
  bind_rows(readRDS("data/onroad/annie/v2/site_avgs/nonspatial/unadjusted_4_balanced_allhours.rds")) %>%
  # BH
  bind_rows(readRDS("data/onroad/annie/v2/site_avgs/nonspatial/unadjusted_12_balanced_businesshours.rds")) %>%
  bind_rows(readRDS("data/onroad/annie/v2/site_avgs/nonspatial/unadjusted_4_balanced_businesshours.rds"))

m1 <- rename(m1, annual_mean_magali = annual_mean)



comp <- left_join(a1, m1) %>%
  mutate(
    design_label = paste(version, design, visits, adjusted)
  )
################################################################################################
# all same IDs
table(a1$id %in% m1$id)


################################################################################################
# scatterplot
comp %>%
  ggplot(aes(x=annual_mean_annie, y=annual_mean_magali)) + 
  facet_wrap_equal(~design_label) +
  geom_abline(slope = 1, intercept = 0) +
  geom_vline(xintercept = 1e5, linetype=2, alpha=0.5) +
  geom_hline(yintercept = 1e5, linetype=2, alpha=0.5) +
  geom_smooth() + 
  geom_point(alpha=0.3, aes(col=design_label), show.legend = F) +
  theme(legend.position="bottom") 

ggsave(file.path(qc_path, "scatterplot.png"), width = 8, height = 6)


# boxplots
comp %>%
  pivot_longer(contains("annual_mean")) %>%
  
  ggplot(aes(y=design_label, x=value, col=name)) + 
  geom_boxplot() +
  theme(legend.position="bottom") + 
  labs(title = "annual averages comparison")

ggsave(file.path(qc_path, "boxplots.png"), width = 8, height = 6)


# diff
comp %>%
  mutate(diff = annual_mean_annie - annual_mean_magali) %>% 
  ggplot(aes(x=diff)) + 
  geom_histogram(aes(fill=design_label), show.legend = F) + 
  facet_wrap(~design_label#, scales = "free"
             ) +
  geom_vline(xintercept = c(0, -10e3, 10e3)) +
  theme(legend.position="bottom") + 
  labs(title="Difference between Annie & Magali's annual averages",
       subtitle = "lines are at 0, +-10k"
       )
ggsave(file.path(qc_path, "difference_histograms.png"), width = 8, height = 6)



################################################################################################



