#This script explores reporting trends to understand whether the DGP could actually be changing throughout the time series

ww_dd_raw <- read_csv("input/shared/SARS-CoV-2_Dashboard_Data_ 2023-04-24.csv") %>%
  clean_names()

ww_dd <- ww_dd_raw %>%
  drop_na(sars_2_copies_l_uncorrected) %>%
  mutate(dow=factor(weekdays(date),levels=weekday.names))

ww_dd %>%
  count(utility,dow) %>%
  ggplot(aes(x=dow,y=reorder(utility,n),fill=n)) +
  geom_tile() +
  scale_fill_viridis_b(name="Measurements") +
  labs(x=NULL,y="Utilities")

ggsave("outputs/reporting_dow.png",height = 10,width = 8,units = "in")
