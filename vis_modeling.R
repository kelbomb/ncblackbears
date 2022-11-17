##set working directory
setwd("~/Black Bears")

# Clear Environment and Load Packages -------------------------------------

rm(list = ls())
library(AER)
library(arm)
library(Sleuth3)
library(lmerTest)
library(tidyverse)
library(GGally) #check assumptions
library(ggplot2) #graphing
library(AICcmodavg) #AIC tables
library(usdm) #for VIF calculations on dataframe
library(lme4) #for assumption tests in mixed effects model
library(gridExtra) #grid for plots
library(sjPlot) #visualizing mixed-effects models
library(sjstats) #ICC - intraclass-correlation coefficent
require(jtools) ##plots and saving plots
theme_set(theme_classic(12))

##load data
load(file = "luna_vis.RData")

##remove october & July data
luna_vis <- luna_vis[!(luna_vis$Month == "7"),]
luna_vis <- luna_vis[!(luna_vis$Month == "10"),]

#########################
#########################
##Luna Visibility Model##
#########################
#########################

##check distribution of hours
xtabs(~visible + Hour, data = luna_vis)

#group
luna_vis$Hour_group <- ifelse(luna_vis$Hour %in% c(9, 10, 11), "Morning",
                               ifelse(luna_vis$Hour %in% c(12, 13), "Mid-day",
                                      ifelse(luna_vis$Hour %in% c(14, 15, 16), "Afternoon", "")))

xtabs(~visible + Hour_group, data = luna_vis)

##as factor
luna_vis$Hour_group <- as.factor(luna_vis$Hour_group)


##explore the data
head(luna_vis)
str(luna_vis)

#USE A BOXPLOT/TABLE/FIND DESCRIPTIVE STATS to EXPLORE FACTORS
apply(table(luna_vis[, c("visible", "Access")]), 1, prop.table)
apply(table(luna_vis[, c("visible", "Hour_group")]), 1, prop.table)
##period of day fairly balanced, differences in access balance but this is what we're interested in

#LET'S CHECK OUT #S FOR AN INTERACTION BTWN ACCESS AND HOUR
library(tables)
tabular(Access*Hour_group~visible, data = luna_vis)
#possible interaction, visibility seems lowest in the afternoon

##################
##Start modeling##
##################

#Visible as binomial factor
luna_vis$visible <- factor(luna_vis$visible)

#Create list of models and test all possibilities

modlist <- list()


modlist[["null"]] <- null <- glm(visible ~ 1, data = luna_vis, family = "binomial")

modlist[["full"]] <- full <- glm(visible ~ Access + Hour_group, data = luna_vis, family = "binomial")

modlist[["access"]] <- a <- glm(visible ~ Access, data = luna_vis, family = "binomial")

modlist[["hour"]] <- h <- glm(visible ~ Hour_group, data = luna_vis, family = "binomial")

modlist[["int"]] <- int <- glm(visible ~ Hour_group*Access, data = luna_vis, family = "binomial")

#compare AIC
aictab(modlist)
#access only model is the best model
summary(a)

tab_model(a)
tab_model(a, file = "luna_vis.doc")

##unique probabilities
probs <- unique(fitted(a))

####################
##Test Assumptions##
####################

#pull coefficients
co_lunavis <- coef(a)
co_lunavis

###################
##Plot the Model ##
###################

###prepare plot
pred_data <- expand.grid(Access = c("No","Yes"),
                         Season = c("Non-Denning", "Denning"))
fit <- data.frame(x = predict(a, newdata = pred_data, se.fit = T, type = "response"))
pdat1 <- data.frame(pred_data, Viz = fit$x.fit, SE = fit$x.se.fit)
pdata <- pdat1 %>% group_by(Access) %>% summarise(mn = mean(Viz, na.rm = T), se = mean(SE))

##rename factor levels
pdata <- mutate(pdata, labs = c("Limited Access", "Full Access"))

#plot
jpeg(file="luna_viz.jpeg")
p <- ggplot(pdata, aes(x = labs, y = mn, fill = Access)) + 
  geom_col(data = pdata, aes(x = labs, y = mn),
           size = 1, position = position_dodge(width = 0.8)) + 
  geom_errorbar(data = pdata, aes(x = labs, y = mn, ymax = mn + se, 
                                    ymin = mn - se), width = 0.2, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("lightsteelblue", "firebrick"))  +
  labs(y = "Predicted likelihood of visibility", x = "Access Type") +
  theme_bw(base_size = 30) +
  theme(text = element_text(size=15))
p + theme(legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black")) + 
  theme(legend.position='none')
dev.off()

###############################################################################

##set working directory
setwd("~/Black Bears")

# Clear Environment and Load Packages -------------------------------------

rm(list = ls())
library(AER)
library(arm)
library(Sleuth3)
library(lmerTest)
library(tidyverse)
library(GGally) #check assumptions
library(ggplot2) #graphing
library(AICcmodavg) #AIC tables
library(usdm) #for VIF calculations on dataframe
library(lme4) #for assumption tests in mixed effects model
library(gridExtra) #grid for plots
library(sjPlot) #visualizing mixed-effects models
library(sjstats) #ICC - intraclass-correlation coefficent
require(jtools) ##plots and saving plots
theme_set(theme_classic(12))

##load data
load(file = "nova_vis.RData")

##remove october & July data
nova_vis <- nova_vis[!(nova_vis$Month == "7"),]
nova_vis <- nova_vis[!(nova_vis$Month == "10"),]


#########################
#########################
##Nova Visibility Model##
#########################
#########################

##check distribution of hours
xtabs(~visible + Hour, data = nova_vis)
nova_vis$Hour_group <- ifelse(nova_vis$Hour %in% c(9, 10, 11), "Morning",
                              ifelse(nova_vis$Hour %in% c(12, 13), "Mid-day",
                                     ifelse(nova_vis$Hour %in% c(14, 15, 16), "Afternoon", "")))

xtabs(~visible + Hour_group, data = nova_vis)

##as factor
nova_vis$Hour_group <- as.factor(nova_vis$Hour_group)


##explore the data
head(nova_vis)
str(nova_vis)

#USE A BOXPLOT/TABLE/FIND DESCRIPTIVE STATS to EXPLORE FACTORS
apply(table(nova_vis[, c("visible", "Access")]), 1, prop.table)
apply(table(nova_vis[, c("visible", "Hour_group")]), 1, prop.table)
##period of day fairly balanced, differences in access balance but this is what we're interested in


#LET'S CHECK OUT #S FOR AN INTERACTION BTWN ACCESS AND HOUR
library(tables)
tabular(Access*Hour_group~visible, data = nova_vis)
#does not seem to be an interaction

##################
##Start modeling##
##################

#Visible as binomial factor
nova_vis$visible <- factor(nova_vis$visible)

#Create list of models and test all possibilities

modlist <- list()


modlist[["null"]] <- null <- glm(visible ~ 1, data = nova_vis, family = "binomial")

modlist[["full"]] <- full <- glm(visible ~ Access + Hour_group, data = nova_vis, family = "binomial")

modlist[["access"]] <- a <- glm(visible ~ Access, data = nova_vis, family = "binomial")

modlist[["hour"]] <- h <- glm(visible ~ Hour_group, data = nova_vis, family = "binomial")

modlist[["int"]] <- int <- glm(visible ~ Hour_group*Access, data = nova_vis, family = "binomial")

#compare AIC
aictab(modlist)

#full model is the best model
#but access is not significant
#so we select the hour only model
summary(h)

##############

tab_model(h)
tab_model(h, file = "nova_vis.doc")

####################
##Test Assumptions##
####################

#pull coefficients
co_novavis <- coef(h)
co_novavis


###################
##Plot the Model ##
###################

###prepare plot
pred_data <- expand.grid(Hour_group = c("Morning", "Mid-day", "Afternoon"),
                         Season = c("Non-Denning", "Denning"))
fit <- data.frame(x = predict(h, newdata = pred_data, se.fit = T, type = "response"))
pdat1 <- data.frame(pred_data, Viz = fit$x.fit, SE = fit$x.se.fit)
pdath <- pdat1 %>% group_by(Hour_group) %>% summarise(mn = mean(Viz, na.rm = T), se = mean(SE))
#dont need -> pdatint$group <- 2

##rename factor levels
levels(pdath$Hour_group) <- c("Morning", "Mid-day", "Afternoon")

#plot
jpeg(file="nova_viz.jpeg")
p <- ggplot(pdath, aes(x = Hour_group, y = mn, fill = Hour_group)) + 
  geom_col(data = pdath, aes(x = Hour_group, y = mn),
           size = 1, position = position_dodge(width = 0.8)) + 
  geom_errorbar(data = pdath, aes(x = Hour_group, y = mn, ymax = mn + se, 
                                  ymin = mn - se), width = 0.2, position = position_dodge(width = 0.8)) +
  scale_fill_manual(values = c("lightsteelblue", "firebrick", "#F0E442")) +
  labs(y = "Predicted likelihood of visibility", x = "Period of Day") +
  theme_bw(base_size = 30) +
  theme(text = element_text(size=15))
p + theme(legend.background = element_rect(size=0.5, linetype="solid", 
                                           colour ="black")) + 
  theme(legend.position='none')
dev.off()