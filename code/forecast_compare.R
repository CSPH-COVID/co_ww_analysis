
forcast_dat <- read_csv("cache/broomfield_forecast.csv")

ww_data <- read_csv("input/ww_raw.csv") %>%
  filter(utility == "Broomfield") %>%
  select(measure_date,sars) %>%
  mutate(measure_date=as_date(measure_date),
         sars=log10(sars),
         sars=na_if(sars,-Inf),
         sars=na_if(sars,Inf)) %>%
  arrange(measure_date) %>%
  drop_na() %>%
  filter(measure_date > as_date("2022-10-14"))


to_plot <- forcast_dat %>%
  filter(measure_date>as_date("2022-03-01")) %>%
  left_join(ww_data,by="measure_date")

to_plot %>%
  ggplot(aes(x=measure_date)) +
  geom_point(aes(y=sars.x)) +
  geom_line(aes(y=trend),color="blue") +
  geom_ribbon(aes(ymin=lower_ci,ymax=upper_ci),alpha=.2,fill="darkgreen") +
  geom_point(aes(y=sars.y),color="red")
