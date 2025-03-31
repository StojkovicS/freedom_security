# libs
library(dplyr); library(ggplot2); library(ggrepel)

# data
original <- read.csv("WV7_freedom_vs_security.csv")
mydata <- original %>% select(country, security, freedom)

# add eu average from available eu countries
eu <- mydata %>% filter(country %in% c("Czechia", "Germany", "Greece", "Netherlands",
                                 "Romania", "Slovakia"))
eu_mean <- eu %>% 
  summarise(security=mean(security),
            freedom=mean(freedom))

eu_as_observation <- c("EU average", eu_mean$security, eu_mean$freedom)
mydata <- rbind(mydata, eu_as_observation)

mydata$security <- as.numeric(mydata$security)
mydata$freedom <- as.numeric(mydata$freedom)

# descriptives
head(mydata)
summary(mydata[c("security", "freedom")])

########### PLOT ##############
ggplot(mydata, aes(security, freedom))+
  geom_point()

ggplot(mydata, aes(security, freedom))+
  geom_point() +
  geom_abline(intercept=95, slope=-1, linetype="dashed")+
  xlim(0,100)+
  ylim(0,100)

ggplot(mydata, aes(security, freedom))+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+ # white are borders of dots, but fill/color of themselves is green
 # geom_abline(intercept = 95, slope = -1, linetype=2)+
  scale_x_continuous(limits=c(0,100))+
  scale_y_continuous(limits=c(0,100))+
  labs(title="Security vs Freedom",
       subtitle="Period: 2017-2022",
       caption="Source: World Value Survey",
       x="Security (%)",
       y="Freedom (%)") #+
#  theme_minimal()

#### SUBSET TOP SECURITY VS FREEDOM ####
top_security <- mydata %>% arrange(security-freedom) %>% head(1)
top_freedom <- mydata %>% arrange(freedom-security) %>% head(2)
ojropa <- mydata %>% filter(country == "EU average")

ggplot(mydata, aes(security, freedom, label=country))+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
#  geom_abline(intercept = 95, slope = -1, linetype=2)+
  scale_x_continuous(limits=c(0,100))+
  scale_y_continuous(limits=c(0,100))+
  labs(title="Security vs Freedom",
       subtitle="Period: 2017-2022",
       caption="Source: World Value Survey
       Author: Stefan Stojkovic",
       x="Security (%)",
       y="Freedom (%)")+
  geom_text_repel(data=top_security, aes(label = country), color = "black", nudge_y = 2) +
  geom_text_repel(data=top_freedom, aes(label = country), color = "black", nudge_y = -2) +
  geom_text_repel(data = ojropa, aes(label = country), color = "black", nudge_y = -8)
  theme_bw()

### hline vline?
ggplot(mydata, aes(security, freedom, label=country))+
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
 # geom_abline(intercept = 100, slope = -1, linetype=2)+
  scale_x_continuous(limits=c(0,100))+
  scale_y_continuous(limits=c(0,100))+
  geom_hline(yintercept=50, linetype=2)+
  geom_vline(xintercept=50, linetype=2)+
  labs(title="Which is more important to you, security or freedom?",
       subtitle="Period: 2017-2022",
       caption="Source: World Value Survey
       Author: Stefan Stojkovic (https://www.linkedin.com/in/stefan-stojkovic-/)",
       x="Security (%)",
       y="Freedom (%)")+
  geom_text_repel(data=top_security, aes(label = country), color = "black", nudge_y = 4) +
  geom_text_repel(data=top_freedom, aes(label = country), color = "black", nudge_y = 4) +
  geom_text_repel(data = ojropa, aes(label = country), color = "black", nudge_y = -7) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 15),
    plot.caption = element_text(size = 12)  # Adjust the font size of the caption
  )





