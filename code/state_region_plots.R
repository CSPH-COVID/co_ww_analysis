#This script reads in the model-smoothed wastewater data and generates plots of
#CO state level trends and regional trends

#Read cached raw data 
ww_data <- read_csv("input/ww_raw.csv") %>% select(-sars)
summary(ww_data$measure_date)
n_distinct(ww_data$utility)

#Read in BSTS smoothed data - note: field is called trend
ww_trend <- read_csv(str_c("cache/utility_bsts_fit_forecast_",today(),".csv")) %>%
  #inner_join(ww_data %>% select(utility,region,pop) %>% distinct(utility,.keep_all=T))
  inner_join(ww_data %>% select(measure_date,utility,region,pop))
summary(ww_trend$measure_date)
n_distinct(ww_trend$utility)
n_distinct(ww_trend$utility[ww_trend$measure_date>as_date(today()-14)])

#construct dataframe for converting data to epi week
date_tab <- tibble(measure_date=seq(as_date("2020-08-02"),today(),by=1)) %>%
  group_by(year=epiyear(measure_date),week=epiweek(measure_date)) %>%
  summarize(measure_date=first(measure_date)) %>%
  ungroup()

#Constructing weekly measure with error estimates
ww_merge <- ww_trend %>%
  group_by(year=epiyear(measure_date),week=epiweek(measure_date),utility) %>%
  summarize(trend=mean(trend,na.rm=T),
            pop=max(pop,na.rm=T)) %>%
  ungroup() %>%
  inner_join(date_tab,by = c("year", "week")) %>%
  group_by(measure_date) %>%
  summarize(across(c(trend),
                   list(mean=~weighted.mean(.,pop,na.rm=T),
                        sd=~sqrt(wtd.var(.,pop,na.rm=T))))) %>%
  ungroup() %>%
  mutate(trend_min=trend_mean-1.96*trend_sd,
         trend_max=trend_mean+1.96*trend_sd)

#Constructing weekly measure by region with error estimates
ww_region <- ww_trend %>%
  group_by(year=epiyear(measure_date),week=epiweek(measure_date),utility) %>%
  summarize(trend=mean(trend,na.rm=T),
            pop=max(pop,na.rm=T),
            region=first(region)) %>%
  ungroup() %>%
  inner_join(date_tab,by = c("year", "week")) %>%
  group_by(measure_date,region) %>%
  summarize(across(c(trend),
                   list(mean=~weighted.mean(.,pop,na.rm=T),
                        sd=~sqrt(wtd.var(.,pop,na.rm=T))))) %>%
  ungroup() %>%
  mutate(trend_min=trend_mean-1.96*trend_sd,
         trend_max=trend_mean+1.96*trend_sd)


m_date <- ww_trend %>%
  summarize(max(measure_date)) %>%
  pull()

sdate <- "2021-11-01"

bg <- tibble(high=quantile(ww_trend$trend,probs=seq(.25, 1, 0.25),na.rm = T),
             low=quantile(ww_trend$trend,probs=c(0,seq(.25, .75, 0.25)),na.rm = T),
             clrs=str_c("#",c("2a9d8f","e9c46a","f4a261","e76f51")))

ww_merge %>%
  filter(measure_date > as_date(sdate)) %>%
  ggplot() +
  geom_rect(data=bg,aes(xmin=as_date(sdate),xmax=as_date(m_date),
                        ymin=low,ymax=high,fill=clrs),alpha=.4,show.legend = F) +
  geom_point(aes(x=measure_date,y=trend_mean),alpha=.7,size=1,show.legend = F) +
  geom_line(aes(x=measure_date,y=trend_mean),alpha=.5,size=1,show.legend = F) +
  geom_ribbon(aes(x=measure_date,y=trend_mean,ymin=trend_min,ymax=trend_max),alpha=.2,size=1,show.legend = F) +
  scale_x_date(date_breaks = "2 months",date_labels = "%b-%y",date_minor_breaks = "1 month") +
  scale_fill_identity() +
  theme_minimal(base_size = 15) +
  labs(x=NULL,y="Wastewater Concentration\n(log scale)",
       caption = str_c("Data as of ",m_date))

ggsave(str_c("outputs/ww_co_",today(),".png"),width = 6,height = 5,units = "in")


ww_region %>%
  filter(measure_date > as_date(sdate)) %>%
  ggplot() +
  geom_rect(data=bg,aes(xmin=as_date(sdate),xmax=as_date(m_date),
                        ymin=low,ymax=high,fill=clrs),alpha=.4,show.legend = F) +
  geom_point(aes(x=measure_date,y=trend_mean),alpha=.7,size=1,show.legend = F) +
  geom_line(aes(x=measure_date,y=trend_mean),alpha=.5,size=1,show.legend = F) +
  geom_ribbon(aes(x=measure_date,y=trend_mean,ymin=trend_min,ymax=trend_max),alpha=.2,size=1,show.legend = F) +
  scale_x_date(date_breaks = "2 months",date_labels = "%b\n%y",date_minor_breaks = "1 month") +
  scale_fill_identity() +
  theme_minimal(base_size = 15) +
  labs(x=NULL,y="Wastewater Concentration\n(log scale)",
       caption = str_c("Data as of ",m_date)) +
  facet_wrap(~region)

ggsave(str_c("outputs/ww_co_region_",today(),".png"),width = 10,height = 7,units = "in")


######################################
#Plots to assess data quality

#Number of observations by day by region
ww_trend %>%
  count(measure_date,region) %>%
  ggplot(aes(x=measure_date,y=n,fill=region,color=region)) +
  geom_col() +
  theme_bw() +
  labs(y="Number of Utility Observations",x=NULL)

ggsave("outputs/utilities_by_day_by_region.png")

#zoom in to past 3 months
ww_trend %>%
  count(measure_date,region) %>%
  filter(measure_date>as_date(today()-90)) %>%
  ggplot(aes(x=measure_date,y=n,fill=region,color=region)) +
  geom_col() +
  theme_bw() +
  labs(y="Number of Utility Observations",x=NULL)

ggsave("outputs/utilities_by_day_by_region_zoom.png")

#number of utilities reporting by week (year comparison)
ww_trend %>%
  group_by(year=year(measure_date),week=epiweek(measure_date)) %>%
  summarize(n=n_distinct(utility)) %>%
  ggplot(aes(x=week,y=n,color=factor(year),group=year)) +
  geom_line() +
  scale_x_continuous(breaks = seq.int(0,52,by=10)) +
  theme_bw() +
  labs(y="Number of Utilities",x="Epidemiological Week")

ggsave("outputs/utilities_by_epiweek.png")

ww_trend %>%
  filter(measure_date>as_date("2022-10-01")) %>%
  ggplot(aes(x=measure_date,y=utility,fill=trend)) +
  geom_vline(xintercept = max(ww_trend$measure_date),linetype="dashed",alpha=.5) +
  geom_tile() +
  theme_bw() +
  facet_wrap(~region)

ggsave("outputs/utilities_heatmap.png")
