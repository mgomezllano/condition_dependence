pacman::p_load(tidyverse, plyr, ggplot2, car, lme4, gridExtra, emmeans, MASS)

# setwd("") SET WORKING DIRECTORY

#################################################################

######### Figure 2
dta <- read.csv("condition.csv")
head(dta)

m0 <- lm(Rel_weight ~ Sex*Treatment, data = dta)
summary(m0)
Anova(m0, type = 3)
emmeans(m0, pairwise ~ Treatment, by = "Sex")$contrasts

Dcon <- ddply(dta, .(Treatment, Sex), summarise, N = mean(Rel_weight), 
              se = sd(Rel_weight/sqrt(length(Rel_weight))))

Dcon$Treatment <- factor(Dcon$Treatment, levels=c("Wheat", "Corn", "Adapted9", "Adapted17"))

scientific_10 <- function(x) {
  parse(text=gsub("e", " %*% 10^", scales::scientific_format()(x)))
}

dta$Treatment <- factor(dta$Treatment, levels=c("Wheat", "Corn", "Adapted9", "Adapted17"))

ggplot() +
  ggbeeswarm::geom_quasirandom(data = dta, aes(Treatment, Rel_weight, 
                                              group = Sex, fill = Sex),
                               alpha = 0.3, shape = 21, width = 0.1,
                               dodge.width = 0.5) +
  geom_errorbar(data = Dcon, aes(x = Treatment, y = N, ymin = N-se, ymax = N+se, group = Sex), width = 0.05,
                position = position_dodge(width = 0.45)) + xlab("") +
  geom_point(data = Dcon, aes(Treatment, N, fill = Sex), size = 3, shape = 21,
             position = position_dodge(width = 0.45)) +
  scale_x_discrete(labels = c("Wheat", "Corn", "Selected \n(9 gen)", "Selected \n(17 gen)")) +
  scale_y_continuous(label=scientific_10, name = "Relative weight\n(gr/mm)") +
  scale_fill_manual(values = c('#7fbf7b', '#998ec3'), name = "", labels = c("Females", "Males")) +
  theme_minimal() + xlab("Population") + 
  theme(axis.text = element_text(colour = 'black'),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.key = element_blank(),
        legend.position = 'top',
        text = element_text(size = 11)) +
  annotate('text', x = 0.89, y = 0.00103, label = 'a', size = 3) +
  annotate('text', x = 0.89, y = 0.001, label = '(20)', size = 3) +
  annotate('text', x = 1.11, y = 0.00108, label = 'a', size = 3) +
  annotate('text', x = 1.11, y = 0.00105, label = '(20)', size = 3) +
  annotate('text', x = 1.89, y = 0.00099, label = 'b', size = 3) +
  annotate('text', x = 1.89, y = 0.00096, label = '(19)', size = 3) +
  annotate('text', x = 2.11, y = 0.00098, label = 'b', size = 3) +
  annotate('text', x = 2.11, y = 0.00095, label = '(20)', size = 3) +
  annotate('text', x = 2.89, y = 0.00099, label = 'bc', size = 3) +
  annotate('text', x = 2.89, y = 0.00096, label = '(19)', size = 3) +
  annotate('text', x = 3.11, y = 0.00084, label = 'b', size = 3) +
  annotate('text', x = 3.11, y = 0.00081, label = '(19)', size = 3) +
  annotate('text', x = 3.89, y = 0.00103, label = 'ac', size = 3) +
  annotate('text', x = 3.89, y = 0.001, label = '(16)', size = 3) +
  annotate('text', x = 4.11, y = 0.00096, label = 'b', size = 3) +
  annotate('text', x = 4.11, y = 0.00093, label = '(19)', size = 3)


####################################################################

######### 

D <- read.csv("cond_dep.csv")
head(D)
str(D)
D$Offspring <- as.numeric(D$Offspring)
D$Exp.Rep <- factor(D$Exp.Rep)

D1 <- D[which(D$MF == 'CC' | D$MF == 'WW' | D$MF == 'CW' | D$MF == 'WC'), ]


fit <- lm(Offspring ~ Exp.Rep + Treatment * MF, data = D1)
summary(fit)
Anova(fit)
emmeans(fit, pairwise ~ Treatment, by = 'MF')$contrasts

Msum <- ddply(D1, .(Treatment, MF), summarise, N = mean(na.omit(Offspring)), 
              se = sd(na.omit(Offspring))/sqrt(length(na.omit(Offspring))), n = length(na.omit(Offspring)))

Msum$MF <- factor(Msum$MF, 
                  levels=c("WW", "CC", "WC", "CW"))

D1$MF <- factor(D1$MF, 
                levels=c("WW", "CC", "WC", "CW"))


ggplot() +
  ggbeeswarm::geom_quasirandom(data = D1, aes(MF, Offspring, 
                                              group = Treatment, fill = Treatment),
                               alpha = 0.3, shape = 21, width = 0.1,
                               dodge.width = 0.5) +
  geom_errorbar(data = Msum, aes(x = MF, y = N, ymin = N-se, ymax = N+se,
                                 group = Treatment), width = 0.05,
                position = position_dodge(width = 0.5)) +
  geom_point(data = Msum, aes(MF, N, fill = Treatment), size = 3, shape = 21,
             position = position_dodge(width = 0.5)) +
  ylab("Fecundity\n(offspring/day)") + xlab("") +
  scale_x_discrete(labels = c("Wheat - Wheat",
                              "Corn - Corn",
                              "Wheat - Corn",
                              "Corn - Wheat")) +
  xlab("Population\n(Males - Females)") +
  scale_fill_manual(values = c('#f1a340', '#5ab4ac'), name = "", labels = c("Monogamy", "Polyandry")) +
  scale_color_manual(values = c('#f1a340', '#5ab4ac'), name = "", labels = c("Monogamy", "Polyandry")) +
  theme_minimal() + 
  theme(axis.text = element_text(colour = 'black'),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.key = element_blank(),
        legend.position = 'top',
        text = element_text(size = 11)) +
  annotate('text', x = 1, y = 14, label = expression(italic("p = ") ~'0.015'), size = 3) +
  annotate('text', x = 2, y = 13.5, label = expression(italic("p = ") ~'0.185'), size = 3) +
  annotate('text', x = 3, y = 13, label = expression(italic("p = ") ~'0.021'), size = 3) +
  annotate('text', x = 4, y = 17, label = expression(italic("p = ") ~'0.014'), size = 3)


####################################################################

######### 

D2 <- D[which(D$MF == 'EE' | D$MF == 'E2E2'), ]


fit1 <- lm(Offspring ~ Treatment * MF, data = D2)
summary(fit1)
sumfit1 <- emmeans(fit1, pairwise ~ Treatment, by = 'MF')$contrasts

Msum1 <- ddply(D2, .(Treatment, MF), summarise, N = mean(na.omit(Offspring)), 
               se = sd(na.omit(Offspring))/sqrt(length(na.omit(Offspring))), n = length(na.omit(Offspring)))

Msum1$MF <- factor(Msum1$MF, 
                   levels=c("EE", "E2E2"))


D2$MF <- factor(D2$MF, 
                levels=c("EE", "E2E2"))

ggplot() +
  ggbeeswarm::geom_quasirandom(data = D2, aes(MF, Offspring, 
                                              group = Treatment, fill = Treatment),
                               alpha = 0.3, shape = 21, width = 0.1,
                               dodge.width = 0.5) +
  geom_errorbar(data = Msum1, aes(x = MF, y = N, ymin = N-se, ymax = N+se,
                                 group = Treatment), width = 0.05,
                position = position_dodge(width = 0.5)) +
  geom_point(data = Msum1, aes(MF, N, fill = Treatment), size = 3, shape = 21,
             position = position_dodge(width = 0.5)) +
  ylab("Fecundity\n(offspring/day)") + xlab("") +
  scale_x_discrete(labels = c("Selected\n(9 gen)",
                              "Selected\n(17 gen)")) +
  xlab("Population") +
  scale_fill_manual(values = c('#f1a340', '#5ab4ac'), name = "", labels = c("Monogamy", "Polyandry")) +
  scale_color_manual(values = c('#f1a340', '#5ab4ac'), name = "", labels = c("Monogamy", "Polyandry")) +
  theme_minimal() + 
  theme(axis.text = element_text(colour = 'black'),
        axis.line = element_line(colour = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.key = element_blank(),
        legend.position = 'top',
        text = element_text(size = 10)) +
  annotate('text', x = 1, y = 14.2, label = expression(italic("p = ") ~'0.282'), size = 3) +
  annotate('text', x = 2, y = 15.5, label = expression(italic("p = ") ~'0.087'), size = 3) 


