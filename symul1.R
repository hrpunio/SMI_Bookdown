#library(car)
#library(MASS)
library("ggplot2")
library("tidyverse")
library ("ggpubr")


#### korelacja
set.seed(1234)
theme_update(
axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()
)
##Linear Regression
#Generate the independent variable and the error
N <- 30
S <- 15
#x1=rnorm(N,50,2)
x1 <- seq(from=3, to=32, by=1)
length(x1)
x2 <- rnorm(N,200,64)

#create the model
m1=lm(y1~x1)
summary(m1)


## b. dobre dopasowanie
set.seed(99134)
error=rnorm(N,0,20)
#Generate the dependent variable (b0=150, b1=-4, b2=2.5)
y1=S-(4*x1)+error
xy <- data.frame(x1,y1)
r <- cor(x1, y1)
##
p1 <- ggplot(xy, aes(x=x1, y=y1)) +
  geom_point(size=2, col="blue") +
  ggtitle ("Silna ujemna korelacja") +
  geom_smooth(method='lm', color='red') +
  xlab("") + ylab("") +
  annotate("text", x=15, y=15, label=sprintf("r=%.2f", r),
           size=8)


## ujemna silna
y2=S+(5*x1)+error
xy <- data.frame(x1,y2)
r <- cor(x1, y2)
p2 <- ggplot(xy, aes(x=x1, y=y2)) +
  geom_point(size=2, col="blue") +
  ggtitle ("Silna dodatnia korelacja") +
  geom_smooth(method='lm', color='red') +
  xlab("") + ylab("") +
  annotate("text", x=15, y=150, label=sprintf("r=%.2f", r),
           size=8)


##
set.seed(71234)
errorb <- rnorm(N,0,110)
###
y1=S-(3*x1)+errorb
xy <- data.frame(x1,y1)
r <- cor(x1, y1)
## ujemna słaba
p3<- ggplot(xy, aes(x=x1, y=y1)) +
  geom_point(size=2, col="blue") +
  ggtitle ("Słaba ujemna korelacja") +
  geom_smooth(method='lm', color='red') +
  xlab("") + ylab("") +
  annotate("text", x=15, y=100, label=sprintf("r=%.2f", r),
           size=8)

## dodatnia słaba
set.seed(99134)
errorb <- rnorm(N,0,120)
y2=S+3*x1+errorb
xy <- data.frame(x1,y2)
r <- cor(x1, y2)

p4 <- ggplot(xy, aes(x=x1, y=y2)) +
  geom_point(size=2, col="blue") +
  ggtitle ("Słaba dodatnia korelacja") +
  geom_smooth(method='lm', color='red') +
  xlab("") + ylab("") +
  annotate("text", x=15, y=250, label=sprintf("r=%.2f", r),
           size=8)


## Brak
set.seed(89134)
errorb <- rnorm(N,0,99)
###
y1=S-(.1*x1)+errorb
xy <- data.frame(x1,y1)
r <- cor(x1, y1)
## ujemna słaba
p5 <- ggplot(xy, aes(x=x1, y=y1)) +
  ggtitle ("Brak korelacji") +
  geom_point(size=2, col="blue") +
  geom_smooth(method='lm', color='red') +
  xlab("") + ylab("") +
  annotate("text", x=15, y=100, label=sprintf("r=%.2f", r),
           size=8)

## Funkcyjna
set.seed(99134)
errorb <- rnorm(N,0,2.24)
y2=S+3*x1+errorb
xy <- data.frame(x1,y2)
r <- cor(x1, y2)

p6 <- ggplot(xy, aes(x=x1, y=y2)) +
  ggtitle ("Zależność funkcyjna") +
  geom_point(size=2, col="blue") +
  geom_smooth(method='lm', color='red') +
  xlab("") + ylab("") +
  annotate("text", x=15, y=100, label=sprintf("r=%.2f", r),
           size=8)



p0 <- ggarrange(p1,p2,p3,p4,p5,p6, ncol=3, nrow = 2 )
p0
ggsave(p0, file="correlation_expl.png", width = 11, height=8)
## https://rpubs.com/nguyet/820051

###########################################################
##Simulate data:

set.seed(5)
n <- 10000

df <- data.frame(a = rbeta(n, 5, 2) * 4,
                 b = rbeta(n, 2, 5) * 4,  ### beta jest od zera do 1
                 c = rnorm(n, 2)) %>%
  pivot_longer(cols = c(a, b, c), names_to = 'DataType', values_to = 'Value') %>%
  mutate (DataType = case_match(DataType,
              "a" ~ "czerwony",
              "b" ~ "zielony",
              "c" ~ "niebieski",
              .default = DataType))



# Calculate median and mean by data group:

df %>%
  group_by(DataType) %>%
  summarise(Median = median(Value)) %>%
  ungroup() %>%
  mutate(Text = paste0("Median: ", round(Median, 2))) -> df_median

df %>%
  group_by(DataType) %>%
  summarise(Mean = mean(Value)) %>%
  ungroup() %>%
  mutate(Text = paste0("Mean: ", round(Mean, 2))) -> df_mean

p7 <- df %>%
  ggplot(aes(x = Value, color=DataType, linetype=DataType)) +
  geom_density(size=1) +
  xlab("") + ylab("") +
  scale_linetype(guide=FALSE) +
  scale_color_manual( breaks = c("czerwony", "zielony", "niebieski"),
    values=c("red", "darkgreen", "blue")) +
  labs(color = "Rozkład: ", linetype="") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position="top") +
  geom_vline(data = df_mean, aes(xintercept = Mean,  color=DataType,
                                 linetype=DataType), size = .4)

p7

ggsave(p7, file="neg_posit_screw.png")

## https://rpubs.com/chidungkt/690039

#################################################
## Rozkłady
set.seed(55)
n <- 12000
df <- data.frame(czerwony = rnorm(n, 8, 1.1),
                 turkusowy = rnorm(n, 4, 1) ) %>%
  pivot_longer(cols = c(czerwony, turkusowy), names_to = 'DataType', values_to = 'Value')



summary(df)

df %>%
  group_by(DataType) %>%
  summarise(Median = median(Value)) %>%
  ungroup() %>%
  mutate(Text = paste0("Median: ", round(Median, 2))) -> df_median

df %>%
  group_by(DataType) %>%
  summarise(Mean = mean(Value)) %>%
  ungroup() %>%
  mutate(Text = paste0("Mean: ", round(Mean, 2))) -> df_mean

p8 <- df %>%
  ggplot(aes(x = Value, color=DataType, linetype=DataType)) +
  geom_density(alpha = 0.2, size=1) +
  xlab("") + ylab("") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position="top") +
  #theme(legend.position="none") +
  scale_linetype(guide=FALSE) +
  labs(color = "Rozkład", linetype="") +
  #ggtitle("Rozkład dwumodalny i asymetryczny")+
  geom_vline(data = df_mean, aes(xintercept = Mean, linetype=DataType, color=DataType), size = .4)
p8


https://stackoverflow.com/questions/28099590/create-sample-vector-data-in-r-with-a-skewed-distribution-with-limited-range


### ####################################################

set.seed(71)
n <- 12000

df <- data.frame(a = rnorm(n, 8, 1.7),
                 b = rnorm(n, 4, 1),  ### beta jest od zera do 1
                 c = rbeta(n, 2, 5) *15 ) %>%
  pivot_longer(cols = c(a, b, c), names_to = 'DataType', values_to = 'Value') %>%
  mutate (DataType = case_match(DataType,
                                "a" ~ "czerwony",
                                "b" ~ "czerwony",
                                "c" ~ "niebieski",
                                .default = DataType))


#df <- data.frame(a = rnorm(n, 8, 0.9),
#                 b = rnorm(n, 4, 1),
#                 c = rbeta(n, 2,5) * 15,  ### beta jest od zera do 1
#                 s = rnbinom(n, 12, 1)) %>%
#  pivot_longer(cols = c(a, b, c), names_to = 'DataType', values_to = 'Value') %>%
#  mutate (DataType = case_match(DataType, "a" ~ "b", "c" ~ "c", .default = DataType))



##summary(df)

df %>%
  group_by(DataType) %>%
  summarise(Median = median(Value)) %>%
  ungroup() %>%
  mutate(Text = paste0("Median: ", round(Median, 2))) -> df_median

df %>%
  group_by(DataType) %>%
  summarise(Mean = mean(Value)) %>%
  ungroup() %>%
  mutate(Text = paste0("Mean: ", round(Mean, 2))) -> df_mean

p9 <- df %>%
  ggplot(aes(x = Value, color=DataType, linetype=DataType)) +
  geom_density(alpha = 0.2, size=1) +
  xlab("") + ylab("") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  theme(legend.position="top") +
  #theme(legend.position="none") +
  #ggtitle("Rozkład dwumodalny i asymetryczny")+
  scale_color_manual( breaks = c("czerwony", "zielony", "niebieski"),
                      values=c("red", "darkgreen", "blue")) +
  scale_linetype(guide=FALSE) +
  labs(color = "Rozkład: ", linetype="") +
  geom_vline(data = df_mean, aes(xintercept = Mean, linetype=DataType, color=DataType), size = .4)
p9

p101 <- ggarrange(p8, p9, ncol=2)
p101
ggsave(p101, file="distributions.png", width=8.5)


https://stackoverflow.com/questions/28099590/create-sample-vector-data-in-r-with-a-skewed-distribution-with-limited-range

