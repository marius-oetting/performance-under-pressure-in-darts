# This R-Script covers all the code needed for fitting the models presented in 
# 'Performance under pressure in skill tasks: An analysis of professional darts'
# by ...

# This file provides R-Code for fitting model 1, model 2 and the model in the appendix
# In addition, also the code for Figure 2 and all tables is included


# set your working directory here
setwd("...")

# load packages
library(lme4)
library(ggplot2)
library(lemon)
library(data.table)
library(dplyr)

# import data
darts.maindata <- read.csv("darts_data.csv")
# filter observations where both players have a finish
darts.maindata <- darts.maindata %>% filter(oppfin != "no finish")
# calculate checkout proportions for all scores
checkout.probs <- darts.maindata %>% group_by(pts_before_t1) %>% summarize(meancheckout = mean(checkout)) %>% as.data.frame()
# merge checkout proportion to data for the player's score
darts.maindata <- merge(darts.maindata, checkout.probs, by = c("pts_before_t1"))
# merge checkout proportion for the opponent's score
colnames(checkout.probs) <- c("pts_opp", "checkout_opp")

darts.maindata <- merge(darts.maindata, checkout.probs, by = c("pts_opp"), all.x = TRUE)
# points above 170 cannot be checked out with three darts
darts.maindata$checkout_opp[darts.maindata$pts_opp > 170] <- 0
# bogey numbers cannot be checked out with three darts
darts.maindata$checkout_opp[darts.maindata$pts_opp %in% c(159, 162, 163, 165, 166, 168, 169)] <- 0



# descriptive statistics --------------------------------------------------

# Table 1
summary(darts.maindata[, c("checkout", "meancheckout", "checkout_opp", "exp", "cb")])



# fitting models: part 1 --------------------------------------------------
# the models fitted here are shown in Table 2

# fit turn-level model to all attempts
mod11 <- glmer(checkout ~ meancheckout + checkout_opp + exp + cb
               + (1|name)
               ,family = binomial(link = "logit"),
               control = glmerControl(optimizer = 'Nelder_Mead'),
               data = darts.maindata)
summary(mod11)

min(ranef(mod11)$name); max(ranef(mod11)$name)


# fit turn-level model to non-decider legs
idx.decider <- which(darts.maindata$legs_until_win == 1 & darts.maindata$legs_until_win_opp == 1)

mod12 <- glmer(checkout ~ meancheckout + checkout_opp + exp + cb
               + (1|name)
               ,family = binomial(link = "logit"),
               control = glmerControl(optimizer = 'Nelder_Mead'),
               data = darts.maindata[-idx.decider, ])
summary(mod12)


# fit turn-level model to decider legs
mod13 <- glmer(checkout ~ meancheckout + checkout_opp + exp + cb
               + (1|name)
               ,family = binomial(link = "logit"),
               control = glmerControl(optimizer = 'Nelder_Mead'),
               data = darts.maindata[idx.decider, ])
summary(mod13)



# throw-level data --------------------------------------------------------
col.stack <- c("pts_before_t1", "pts_before_t2", "pts_before_t3")
colnumbers <- which(colnames(darts.maindata) %in% col.stack)
col.take <- (1:ncol(darts.maindata))[-colnumbers]
darts.maindata.throws <- data.frame(darts.maindata[c(col.take)], stack(darts.maindata[col.stack]))

# generate new finish and checkout columns
darts.maindata.throws <- darts.maindata.throws %>% 
  mutate(finish_new = case_when(ind == "pts_before_t1" & 
                                  chancew1 == 1 ~ 1,
                                ind == "pts_before_t2" & 
                                  chancew2 == 1 ~ 1,
                                ind == "pts_before_t3" & 
                                  chancew3 == 1  ~ 1,
                                TRUE ~ 0))

darts.maindata.throws <- darts.maindata.throws %>% 
  mutate(checkout_new = case_when(ind == "pts_before_t1" & 
                                    chancew1res == 1 ~ 1,
                                  ind == "pts_before_t2" & 
                                    chancew2res == 1 ~ 1,
                                  ind == "pts_before_t3" & 
                                    chancew3res == 1  ~ 1,
                                  TRUE ~ 0))

# consider only the third throw of a turn
darts.maindata.throws <- darts.maindata.throws %>% filter(finish_new == 1, ind == "pts_before_t3")




# checkout probs for throw-level data
checkout.prob.throw <- darts.maindata.throws %>% group_by(values) %>% 
  summarize(meancheckout = mean(checkout_new)) %>% as.data.frame()
colnames(checkout.prob.throw) <- c("values", "meancheckout_throw")

darts.maindata.throws <- merge(darts.maindata.throws, checkout.prob.throw, by.x = c("values"), by.y = c("values"))



# fitting models: part 2 --------------------------------------------------
# the models fitted in this section are shown in Table 3

# fit throw-level model to all attempts
mod21 <- glmer(checkout_new ~ meancheckout_throw + checkout_opp + exp + cb
               + (1|name)
               ,family = binomial(link = "logit"),
               control = glmerControl(optimizer = 'Nelder_Mead'),
               data = darts.maindata.throws)
summary(mod21)


# fit throw-level model to non-decider legs
idx.decider <- which(darts.maindata.throws$legs_until_win == 1 & darts.maindata.throws$legs_until_win_opp == 1)

mod22 <- glmer(checkout_new ~ meancheckout_throw + checkout_opp + exp + cb
               + (1|name)
               ,family = binomial(link = "logit"),
               control = glmerControl(optimizer = 'Nelder_Mead'),
               data = darts.maindata.throws[-idx.decider, ])
summary(mod22)

# fit throw-level model to decider legs
mod23 <- glmer(checkout_new ~ meancheckout_throw + checkout_opp + exp + cb
               + (1|name)
               ,family = binomial(link = "logit"),
               control = glmerControl(optimizer = 'Nelder_Mead'),
               data = darts.maindata.throws[idx.decider, ])
summary(mod23)




# Plots -------------------------------------------------------------------
cbbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
darts.maindata$oppfin_yes <- ifelse(darts.maindata$oppfin != "no finish", 1, 0)

### Figure 2
darts.maindata %>% select(pts_before_t1, meancheckout, finish) %>% unique %>% 
  ggplot(aes(x = pts_before_t1, y = meancheckout, fill = finish)) + geom_bar(stat = "identity") + xlab("points before turn") +
  ylab("checkout proportion") + 
  scale_fill_manual(name = "finish", values = cbbPalette, labels = c("1-Dart-Finish", "2-Dart-Finish", "3-Dart-Finish"))



### Figure 4
darts.maindata$oppfin_cat <- cut(darts.maindata$checkout_opp, 
                                       breaks = c(-Inf, 0.3, 0.6, Inf), 
                                       labels = c("<= 0.3", ">0.3 & <= 0.6", "> 0.6"))

points.totake <- darts.maindata %>% filter(oppfin_yes == 1) %>% select(pts_before_t1, meancheckout, finish, oppfin_cat, checkout) %>% 
  group_by(oppfin_cat, pts_before_t1) %>% summarize(meanc = mean(checkout), count = n()) %>% 
  filter(count >= 100) %>% .[["pts_before_t1"]] %>% table %>% as.data.frame() %>% filter(Freq > 2) %>% 
  .[[1]] %>% as.character() %>%  as.numeric()


darts.maindata %>% filter(oppfin_yes == 1) %>% select(pts_before_t1, meancheckout, oppfin_cat, checkout) %>% 
  group_by(oppfin_cat, pts_before_t1) %>% summarize(meanc = mean(checkout), count = n()) %>% 
  filter(pts_before_t1 %in% points.totake) %>% 
  ggplot(aes(x = pts_before_t1, y = meanc, color = factor(oppfin_cat))) + 
  geom_pointline(size = 2) +
  scale_color_manual(name = "checkout proportion \nof opp.", values = cbbPalette, 
                     labels = c(expression(phantom(x) <= "0.3"), 
                                expression(paste(" ", phantom(x) > "0.3", " &", phantom(x) <= "0.6")), 
                                expression(phantom(x) > "0.6"))) +
  xlab("points before turn") + ylab("checkout proportion")



### Figure 5
points.totake <- darts.maindata %>% filter(oppfin_yes == 1) %>% 
  select(pts_before_t1, meancheckout, finish, checkout, legs_until_win, legs_until_win_opp) %>% 
  mutate(decider = ifelse(legs_until_win == 1 & legs_until_win_opp == 1, 1, 0)) %>% 
  group_by(decider, pts_before_t1) %>% summarize(meanc = mean(checkout), count = n()) %>% 
  filter(count >= 10) %>% .[["pts_before_t1"]] %>% table %>% as.data.frame() %>% filter(Freq > 1) %>% 
  .[[1]] %>% as.character() %>%  as.numeric()


darts.maindata %>% filter(oppfin_yes == 1) %>% 
  select(pts_before_t1, checkout, legs_until_win, legs_until_win_opp) %>% 
  mutate(decider = ifelse(legs_until_win == 1 & legs_until_win_opp == 1, 1, 0)) %>% 
  group_by(decider, pts_before_t1) %>% summarize(meanc = mean(checkout), count = n()) %>% 
  filter(pts_before_t1 %in% points.totake) %>%   
  ggplot(aes(x = pts_before_t1, y = meanc, color = factor(decider))) + 
  geom_pointline(size = 2) +
  scale_color_manual(name = "decider leg", values = cbbPalette, 
                     labels = c("no", "yes")) +
  xlab("points before turn") + ylab("checkout proportion")


### Figure 3
# import data again, since observations where the opponent had no finish are also shown in the figures
darts.maindata <- read.csv("darts_data.csv")
darts.maindata$oppfin_yes <- ifelse(darts.maindata$oppfin != "no finish", 1, 0)

points.take <- darts.maindata %>%  select(pts_before_t1, checkout, finish, oppfin_yes) %>% 
  group_by(pts_before_t1, oppfin_yes) %>% summarize(count = n()) %>% filter(count >= 100) %>% .[["pts_before_t1"]] %>% 
  table %>% as.data.frame() %>% filter(Freq > 1) %>% .[[1]] %>% as.character() %>% as.numeric()

darts.maindata %>%  select(pts_before_t1, checkout, oppfin_yes) %>% 
  filter(pts_before_t1 %in% points.take) %>% 
  group_by(pts_before_t1, oppfin_yes) %>%
  summarize(meanc = mean(checkout), count = n()) %>% 
  ggplot(aes(x = pts_before_t1, y = meanc, color = factor(oppfin_yes))) + geom_pointline(size = 2) +
  scale_color_manual(name = "opp. can finish", 
                     values = cbbPalette, labels = c("no", "yes")) + xlab("points before turn") + 
  ylab("checkout proportion")



