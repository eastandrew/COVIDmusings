library(tidyverse)
library(lubridate)


# pull these data down from <https://github.com/CSSEGISandData/COVID-19>
# save in same directory as this script
deathdata <- read_csv("time_series_covid19_deaths_US.csv")
#View(deathdata)

deathdata2long <- deathdata %>%
  select(-c(1:6,8:10)) %>%  #monitor these variables to confirm they haven't changed.
  pivot_longer(-c(Province_State,Combined_Key, Population), names_to="date", values_to="count")

deathdata2long$date2 <- mdy(deathdata2long$date)

deathdata2long$Province_State <- factor(deathdata2long$Province_State)


sumstatesummortlast <- deathdata2long %>%
  filter(date2==max(date2)&Population>0) %>%
  group_by(Province_State) %>%
  dplyr::summarize(propmort = sum(count, na.rm=T)/sum(Population, na.rm=T)) %>%
  arrange(-propmort)


par(mai=c(1,2,0.1,0.1))
barplot(rev(sumstatesummortlast$propmort[1:25]*100000), names.arg=rev(sumstatesummortlast$Province_State[1:25]), horiz=T, las=1, xlab="Fatalities per Hundred Thousand, top 25 states and territories,\nas of 23 Apr 2020, per JHU APL data")
## Date here is entered manually, will eventually be auto


sumstatesummort <- deathdata2long %>%
  group_by(Province_State, date2) %>%
  summarize(sumpop = sum(Population, na.rm=T),
            sumcount = sum(count, na.rm=T),
            propmort = sumcount/sumpop) %>%
  arrange(-propmort)

sumsummort <- deathdata2long %>%
  group_by(date2) %>%
  summarize(sumpop = sum(Population, na.rm=T),
            sumcount = sum(count, na.rm=T),
            propmort = sumcount/sumpop) %>%
  arrange(-propmort)




ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-03-01"), aes(x=date2, y=propmort, group=Province_State)) +
  geom_point() +
  geom_smooth(method="lm", se=F) +
  scale_y_log10()


ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-03-01"), aes(x=date2, y=propmort)) +
  geom_point() +
  geom_smooth(method="loess", se=T) +
  scale_y_log10()





ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-04-01"), aes(fill=Province_State, y=propmort*100000, x=date2)) +
  geom_bar(position="fill", stat="identity")

library(forcats)
ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-04-01"), aes(fill=factor(date2), y=propmort*100000, x=fct_infreq(Province_State))) +
  geom_bar(position="stack", stat="identity")

ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-04-01"&propmort>=0.000025), aes(x=fct_reorder(Province_State, -propmort,min), y=propmort*100000)) +
  geom_bar(position="stack", stat="identity") +
  #geom_smooth(method="lm", se=F) +
  facet_wrap(~fct_inorder(factor(date2))) +
  guides(fill=F) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab("State, in order of most recent day") +
  ylab("Number of Fatalities per Hundred Thousand")

ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-04-01"), aes(fill=factor(date2), y=propmort*100000, x=fct_reorder(Province_State, -propmort,min))) +
  geom_bar(position="stack", stat="identity")  +
  theme(axis.text.x = element_text(angle = 75, hjust = 1))

statelist <- c("New York", "New Jersey", "Louisiana", "Michigan", "Connecticut", "Washington")
statelist2 <- c("Massachusetts", "Vermont","District of Columbia", "Colorado", "Rhode Island", "Guam", "Illinois", "Georgia", "Indiana", "Northern Mariana Islands", "Mississippi", "Delaware")
statelist3 <- c("Massachusetts", "Vermont","District of Columbia", "Colorado", "Rhode Island", "Illinois", "Georgia", "Indiana", "Mississippi", "Delaware")

ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-04-01"&Province_State %in% statelist), aes(y=propmort*100000, x=fct_inorder(factor(date2)))) +
  geom_bar(position="stack", stat="identity")  +
  facet_wrap(~fct_reorder(Province_State, -propmort,min)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab("Date, by state") +
  ylab("Number of Fatalities per Hundred Thousand")

ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-04-01"&Province_State %in% statelist2), aes(y=propmort*100000, x=fct_inorder(factor(date2)))) +
  geom_bar(position="stack", stat="identity")  +
  facet_wrap(~fct_reorder(Province_State, -propmort,min)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab("Date, by state") +
  ylab("Number of Fatalities per Hundred Thousand")

ggplot(data=filter(sumstatesummort, sumcount!=0&sumpop!=0&date2>="2020-04-01"), aes(y=propmort*100000, x=fct_inorder(factor(date2)))) +
  geom_bar(position="stack", stat="identity")  +
  facet_wrap(~fct_reorder(Province_State, -propmort,min)) +
  theme(axis.text.x = element_text(angle = 75, hjust = 1)) +
  xlab("Date, by state") +
  ylab("Number of Fatalities per Hundred Thousand")

sumsummort$fatalityrate <- sumsummort$propmort*100000
sumsummort$day <- yday(sumsummort$date2)
par(mai=c(1,1,0.1,0.1))
plot(fatalityrate~day, data=sumsummort, type="b", log="y")

lm1 <- lm(log10(fatalityrate)~day, data=subset(sumsummort, fatalityrate!=0))
summary(lm1)
plot(log10(fatalityrate)~day, data=sumsummort)
abline(lm1)

library(drc)
library(magic)
drm1 <- drm(fatalityrate~day, data=sumsummort, fct=LL.5(), type="continuous")
plot(drm1, log="")
ED(drm1, 50)

sumstatesummort$fatalityrate <- sumstatesummort$propmort*100000
sumstatesummort$day <- yday(sumstatesummort$date2)

drm2 <- drm(fatalityrate~day, curveid=Province_State, data=subset(sumstatesummort, fatalityrate!=0&Province_State %in% statelist), fct=LL.4(), type="continuous", separate=T)
plot(drm2, log="y", legend=F, xlab="Ordinal Date", ylab="Fatality Rate")
legend("topleft", statelist, pch=c(1,2,3,4,5,6), lty=c(1,2,3,4,5,6), bty="n")
ED(drm2, 50, type="relative", interval="delta")


plot(drm2, log="", legend=F, xlab="Ordinal Date", ylab="Fatality Rate")
legend("topleft", statelist, pch=c(1,2,3,4,5,6), lty=c(1,2,3,4,5,6), bty="n")


drm3 <- drm(fatalityrate~day, curveid=Province_State, data=subset(sumstatesummort, fatalityrate!=0&Province_State %in% statelist3), fct=LL.4(), type="continuous", separate=T)
plot(drm3, log="", legend=F, xlab="Ordinal Date", ylab="Fatality Rate")
legend("topleft", statelist3, pch=c(1,2,3,4,5,6,7,8,9,10), lty=c(1,2,3,4,5,6,7,8,9,10), bty="n")

par(mfrow=c(1,2), mai=c(0.8,0.8,0.05,0.05))
plot(drm2, log="", legend=F, xlab="Ordinal Date", ylab="Fatality Rate", ylim=c(1,100), xlim=c(60,115))
legend("topleft", statelist, pch=c(1,2,3,4,5,6), lty=c(1,2,3,4,5,6), bty="n")
text(75,20,"Top 6", font=2, cex=2)
plot(drm3, log="", legend=F, xlab="Ordinal Date", ylab="Fatality Rate", ylim=c(1,100), xlim=c(60,115))
legend("topleft", statelist3, pch=c(1,2,3,4,5,6,7,8,9,10), lty=c(1,2,3,4,5,6,7,8,9,10), bty="n")
text(80,20, "Tier 2", font=2, cex=2)
par(mfrow=c(1,1), mai=c(1,1,0.1,0.1))


drm1b <- drm(propmort~day, data=sumsummort, fct=LL.5(), type="continuous")
plot(drm1b, log="")
ED(drm1b, 50)

drm2b <- drm(propmort~day, curveid=Province_State, data=subset(sumstatesummort, fatalityrate!=0&Province_State %in% statelist), fct=LL.4(), type="continuous", separate=T)
plot(drm2b, log="y", legend=F, xlab="Ordinal Date", ylab="Proportional Fatality")
legend("topleft", statelist, pch=c(1,2,3,4,5,6), lty=c(1,2,3,4,5,6), bty="n")
ED(drm2b, 50, type="relative", interval="delta")


plot(drm2b, log="", legend=F, xlab="Ordinal Date", ylab="Proportional Fatality")
legend("topleft", statelist, pch=c(1,2,3,4,5,6), lty=c(1,2,3,4,5,6), bty="n")


drm3b <- drm(propmort~day, curveid=Province_State, data=subset(sumstatesummort, fatalityrate!=0&Province_State %in% statelist3), fct=LL.4(), type="continuous", separate=T)
plot(drm3b, log="", legend=F, xlab="Ordinal Date", ylab="Proportional Fatality")
legend("topleft", statelist3, pch=c(1,2,3,4,5,6,7,8,9,10), lty=c(1,2,3,4,5,6,7,8,9,10), bty="n")

par(mfrow=c(1,2), mai=c(0.8,0.8,0.05,0.05))
plot(drm2b, log="", legend=F, xlab="Ordinal Date", ylab="Proportional Fatality", ylim=c(0,0.001))
legend("topleft", statelist, pch=c(1,2,3,4,5,6), lty=c(1,2,3,4,5,6), bty="n")
text(75,200,"Top 6", font=2, cex=2)
plot(drm3b, log="", legend=F, xlab="Ordinal Date", ylab="Proportional Fatality", ylim=c(0,0.001), xlim=c(60,115))
legend("topleft", statelist3, pch=c(1,2,3,4,5,6,7,8,9,10), lty=c(1,2,3,4,5,6,7,8,9,10), bty="n")
text(80,200, "Tier 2", font=2, cex=2)
par(mfrow=c(1,1), mai=c(1,1,0.1,0.1))


par(mfrow=c(1,2), mai=c(0.8,0.8,0.05,0.05))
plot(drm2b, log="y", legend=F, xlab="Ordinal Date", ylab="Proportional Fatality", ylim=c(0.00000008,0.001), xlim=c(60,115))
legend("topleft", statelist, pch=c(1,2,3,4,5,6), lty=c(1,2,3,4,5,6), bty="n")
text(75,200,"Top 6", font=2, cex=2)
plot(drm3b, log="y", legend=F, xlab="Ordinal Date", ylab="Proportional Fatality", ylim=c(0.00000008,0.001), xlim=c(60,115))
legend("topleft", statelist3, pch=c(1,2,3,4,5,6,7,8,9,10), lty=c(1,2,3,4,5,6,7,8,9,10), bty="n")
text(80,200, "Tier 2", font=2, cex=2)
par(mfrow=c(1,1), mai=c(1,1,0.1,0.1))



#"%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
statecut <- c("Grand Princess", "Guam", "Puerto Rico", "District of Columbia", "Northern Mariana Islands", "Virgin Islands","American Samoa", "Diamond Princess")
sumstatesummortsub <- filter(sumstatesummort, !Province_State%in%statecut&date2==max(date2))
plot(propmort~log10(sumpop), data=sumstatesummortsub, pch=16)
abline(lm(propmort~log10(sumpop), data=sumstatesummortsub))
text(log10(sumstatesummortsub$sumpop), sumstatesummortsub$propmort, sumstatesummortsub$Province_State, adj=c(0,0))

plot(log10(propmort)~log10(sumpop), data=filter(sumstatesummortsub, sumcount!=0), pch=16)
abline(lm(log10(propmort)~log10(sumpop), data=filter(sumstatesummortsub, sumcount!=0)))
text(log10(sumstatesummortsub$sumpop), log10(sumstatesummortsub$propmort), sumstatesummortsub$Province_State, adj=c(0,0))
summary(lm(log10(propmort)~log10(sumpop), data=filter(sumstatesummortsub, sumcount!=0)))

plot(propmort~sumpop, data=sumstatesummortsub, pch=16)
abline(lm(propmort~sumpop, data=sumstatesummortsub))
text(sumstatesummortsub$sumpop, sumstatesummortsub$propmort, sumstatesummortsub$Province_State, adj=c(0,0))

plot(density(log10(sumstatesummortsub$propmort)))
plot(density(sumstatesummortsub$propmort))