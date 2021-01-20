# 2019 Gaspe Water Isotopes  #
# Kaiying Sally Ju | July 2020
rm(list=ls())


library(dplyr)
library(ggplot2)
library(dplyr)
library(tidyr)

setwd(choose.dir()) # Choose directory
df<-read.csv("./2019-Sept-WaterIsotopes.csv")
View(df)
str(df)

# 0. Massage Data #
df<-separate(df, "Sample", c("Site", "Sample"), "919") #Split Sample ID
df$Site<-as.factor(df$Site)
df$Sample<-as.factor(df$Sample)
spray.sites <- c("S5", "S6", "P20", "S3", "S1", "S2")
df$Treatment <- ifelse(df$Site %in% spray.sites, "Spray", "Defoliated") #Add treatment column
df$Site2 <- mapvalues(df$Site, from=c("S4","S5","S6","N4","N5","P20","N6","S3","N1","S1","N2","S2"), 
                      to=c("U1","U2","U3","C4","C5","C6","C7","L8","L9","L10","L11","L12")) #Add new site names (Site2)
df <- df %>%  # Add location of sites (U, C, L)
  separate(Site2, sep=1,into=c("Location"), extra="drop", remove=F)
df$Location<-as.factor(df$Location)
df<-df %>%
  filter(!grepl('QCD', Sample)) #Remove QC rows
df <- df[c(1,8,9,2,7,3:6)] # Reorder columns
df$Location <- factor(df$Location, levels = c("L", "C", "U")) # Order factors

df.clean<-df %>% #df.clean is a dataframe without the 3 samples that are contaminated 
  filter(!grepl('NBS', Contaminant)) 
head(df.clean, 20)

# 1. Data Exploration #
# Compare Spray and Defoliated sites #

df.clean %>%  
  group_by(Location) %>% 
  ggplot(aes(x=Site2, y=d2H, fill=Location)) + 
  geom_boxplot() +
  ggtitle("Water d2H in Spray vs Defoliated sites ")

# Distribution of d2H Isotopes across all sites 
df.clean %>% 
  ggplot(aes(d2H)) +
  geom_histogram(binwidth = 3, fill="lightblue", color="black") +
  ggtitle("Distribution of d2H Isotopes") +
  scale_y_continuous(breaks = seq(0, 11, by=1)) 

df.clean %>%  # Use df.clean due to contamination in C6, C7
  ggplot(aes(d2H)) +
  geom_histogram(binwidth = 3, fill="lightblue", color="black") +
  ggtitle("Distribution of d2H Isotopes by different sites") +
  scale_y_continuous(breaks = seq(0, 11, by=1)) +
  facet_wrap(~Site2)

# Compare d2H in different locations 
df.clean %>%  # Compare spray vs defoliated sites 
  ggplot(aes(x=Site2, y=d2H, fill=Location)) + 
  geom_boxplot() +
  facet_wrap(~Treatment, scale="free_x") +
  ggtitle("Water d2H in Spray vs Defoliated sites")

df.clean %>%  # Boxplot of d2H values in different locations - individual sites
  ggplot(aes(x=Site2, y=d2H, fill=Location)) + 
  geom_boxplot() +
  facet_wrap(~Location, scale="free_x") +
  ggtitle("Water d2H in Central, Upper, and Lower Sites")

df.clean %>% # Boxplot of d2H values in different locations - Location as factor
  group_by(Location) %>%
  mutate(mean_d2H=mean(d2H)) %>% 
  ggplot(aes(factor(Location),d2H)) + 
  geom_boxplot(aes(fill=Location)) +
  scale_x_discrete(labels=c("L" = "Lower", "C" = "Central", "U" = "Upper")) +
  ggtitle("Water d2H in Central, Upper, and Lower Sites")

df.clean %>% # Distribution of isotopes in diffent locations
  ggplot(aes(x=d2H, color=Location, fill=Location)) + 
  geom_histogram(aes(y=..density..), binwidth=0.8, alpha=0.5, position="identity")+
  geom_density(alpha=.2) +
  ggtitle("d2H Distribution density in different locations")


# 2. Stats #
# Looks like interesting things are happening in the different locations 
# Lets do some explorin to see if there's any difference b/t L, C, U locations

shapiro.test(df.clean$d2H) #p<0.05, non-Normal
model <- lm(d2H ~ Location, data = df.clean)
summary(model)
plot(model) # plots to determine ANOVA assumptions - doesn't look like it is very normal

# Non - Normal analysis 
kruskal.test(d2H ~ Location, data=df.clean) # p << 0.05
pairwise.wilcox.test(df.clean$d2H, df.clean$Location,
                     p.adjust.method = "BH") # post-hoc test: all 3 groups are different from each other




#### ignore everything down here ####

anova <- aov(d2H ~ Location, data=df)
TukeyHSD(anova)
summary.lm(anova)



library(lme4)
install.packages("multcomp")
library(multcomp)
lm<-lmer(d2H ~ Location + (1|Site2), data = df) 
anova(lm)
summary(lm)
summary(glht(lm, linfct=mcp(Visit="Tukey")))
plot(lm)
