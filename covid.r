library(tidyverse)
library(lubridate)


# pull these data down from <https://github.com/CSSEGISandData/COVID-19>
# save in same directory as this script
deathdata <- read_csv("time_series_covid19_deaths_US.csv")
#View(deathdata)

deathdata2 <- deathdata %>%
  select(-c(1:6,8:10)) %>%  #monitor these variables to confirm they haven't changed.
  pivot_longer(-c(Province_State,Combined_Key, Population), names_to="date", values_to="count")

deathdata2long$date2 <- mdy(deathdata2long$date)


sumstatesummortlast <- deathdata2long %>%
  filter(date2==max(date2)&Population>0) %>%
  group_by(Province_State) %>%
  dplyr::summarize(propmort = sum(count, na.rm=T)/sum(Population, na.rm=T)) %>%
  arrange(-propmort)


par(mai=c(1,2,0.1,0.1))
barplot(rev(sumstatesummortlast$propmort[1:25]*1000000), names.arg=rev(sumstatesummortlast$Province_State[1:25]), horiz=T, las=1, xlab="Fatalities per Million, top 25 states and territories,\nas of 2 Apr 2020, per JHU APL data")
## Date here is entered manually, will eventually be auto


sumstatesummort <- deathdata2long %>%
  group_by(Province_State, date2) %>%
  summarize(sumpop = sum(Population, na.rm=T),
            sumcount = sum(count, na.rm=T),
            propmort = sumcount/sumpop)

ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-03-01"), aes(x=date2, y=propmort, group=Province_State)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_y_log10()


ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-03-01"), aes(x=date2, y=propmort)) +
  geom_point() +
  geom_smooth(method="loess", se=T) +
  scale_y_log10()
