
a1 <- readRDS("data/onroad/annie/PNC_nonspatial_annavgs.rds") %>%
  filter(version=="all hours",
         design== "balanced",
         grepl("12", visits),
         adjusted=="unadjusted")

a1 <- rename(a1, annual_mean_annie = annual_mean)

# distinct(a1, version, design, visits, adjusted)

m1 <- readRDS("data/onroad/annie/v2/site_avgs/nonspatial/unadjusted_12_balanced_allhours.rds")
m1 <- rename(m1, annual_mean_magali = annual_mean)



comp <- left_join(a1, m1)
################################################################################################
# all same IDs
table(a1$id %in% m1$id)



################################################################################################

comp %>%
  ggplot(aes(x=annual_mean_annie, y=annual_mean_magali)) + 
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth() + 
  geom_point(alpha=0.1)

