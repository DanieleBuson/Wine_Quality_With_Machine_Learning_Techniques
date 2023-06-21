################################################################################
########################## 1. extraction of data ###############################
################################################################################
library(tidyverse)
library(mgcv)
library(plotly)
library(GGally)
library(gridExtra)
library(DT)
library(kernlab)
library(neuralnet)
library(e1071)
library(caret)
library(FSinR)
library(nnet)
library(modelr)


red_wine <- read.csv("winequality-red.csv")
white_wine <- read.csv("winequality-white.csv")
summary(red_wine)

red_wine[which(red_wine$volatile.acidity > 2), "volatile.acidity"] <- red_wine[which(red_wine$volatile.acidity > 2), "volatile.acidity"]/1000
red_wine[which(red_wine$density > 2), "density"] <- red_wine[which(red_wine$density > 2), "density"]/1000
red_wine[which(red_wine$chlorides > 2), "chlorides"] <- red_wine[which(red_wine$chlorides > 2), "chlorides"]/1000

white_wine[which(white_wine$volatile.acidity > 2), "volatile.acidity"] <- white_wine[which(white_wine$volatile.acidity > 2), "volatile.acidity"]/1000
white_wine[which(white_wine$density > 2), "density"] <- white_wine[which(white_wine$density > 2), "density"]/1000
white_wine[which(white_wine$chlorides > 2), "chlorides"] <- white_wine[which(white_wine$chlorides > 2), "chlorides"]/1000

summary(red_wine)
summary(white_wine)

red <- red_wine
red["Col"] <- "Red"

white <- white_wine
white["Col"] <- "White"

wine <- red
wine <- add_row(wine, white)

################################################################################
########################## 2.1 General Overview #################################
################################################################################

ggpairs(wine,                 
        columns = 1:11,       
        aes(color = Col,
            alpha = 0.5)) +
  scale_fill_manual(values = c("#a2231d", "#e4d96f")) + 
  scale_color_manual(values = c("#a2231d", "#e4d96f"))

################################################################################
########################## 2.2 Acidity #########################################
################################################################################

# https://sonomawinegarden.com/is-wine-acidic/

fixed_acidity_plot <- ggplot(data = wine, aes(x = fixed.acidity, fill = Col)) +
  geom_bar(width = 0.25) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  facet_grid(~Col) +
  ggtitle("Fixed Acidity Distribution") +
  ylab("Frequency") +
  xlab("Fixed Acidity")

volatile_acidity_plot <- ggplot(data = wine, aes(x = volatile.acidity, fill = Col)) +
  geom_bar(width = 0.25) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  facet_grid(~Col) +
  ggtitle("Volatile Acidity Distribution") +
  ylab("Frequency") +
  xlab("Volatile Acidity")

citric_acid_plot <- ggplot(data = wine, aes(x = citric.acid, fill = Col)) +
  geom_bar(width = 0.05) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  facet_grid(~Col) +
  ggtitle("Citric Acid Concentration Distribution") +
  ylab("Frequency") +
  xlab("Citric Acid Concentration")

chlorides_plot <- ggplot(data = wine, aes(x = chlorides, fill = Col)) +
  geom_bar(width = 0.05) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  facet_grid(~Col) +
  ggtitle("Chlorides Distribution") +
  ylab("Frequency") +
  xlab("Chlorides")

grid.arrange(fixed_acidity_plot, volatile_acidity_plot, citric_acid_plot, chlorides_plot, ncol = 2, nrow = 2)

fixed_acidity_boxplot <- ggplot(data = wine, aes(x = Col, y = fixed.acidity, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Fixed Acidity Boxplot") +
  ylab("Fixed Acidity") +
  xlab("")

volatile_acidity_boxplot <- ggplot(data = wine, aes(x = Col, y = volatile.acidity, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Volatile Acidity Boxplot") +
  ylab("Volatile Acidity") +
  xlab("")

citric_acid_boxplot <- ggplot(data = wine, aes(x = Col, y = citric.acid, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Citrid Acid Concentration Boxplot") +
  ylab("Citrid Acid Concentration") +
  xlab("")

chlorides_boxplot <- ggplot(data = wine, aes(x = Col, y = chlorides, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Chlorides Concentration Boxplot") +
  ylab("Chlorides Concentration") +
  xlab("")

grid.arrange(fixed_acidity_boxplot, volatile_acidity_boxplot, citric_acid_boxplot, chlorides_boxplot,
             ncol = 2, nrow = 2)

################################################################################
########################## 2.3 Residual Sugar ##################################
################################################################################

# https://winefolly.com/deep-dive/what-is-residual-sugar-in-wine/

residual_sugar_plot <- ggplot(data = wine, aes(x = residual.sugar, fill = Col)) +
  geom_bar(width = 2) + 
  facet_grid(~Col) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Residual Sugar Distribution") +
  ylab("Frequency") +
  xlab("Residual Sugar")
residual_sugar_plot  

residual_sugar_boxplot <- ggplot(data = wine, aes(x = Col, y = residual.sugar, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Residual Sugar Boxplot") +
  ylab("Residual Sugar") +
  xlab("")
residual_sugar_boxplot

################################################################################
########################## 2.4 Sulfates ########################################
################################################################################

# https://sommailier.com/sulfites-in-wine/

free_dioxide_sulfur_plot <- ggplot(data = wine, aes(x = free.sulfur.dioxide, fill = Col)) +
  geom_bar(width = 5) + 
  facet_grid(~Col) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Free Sulfur Dioxide Concentration Distribution") +
  ylab("Frequency") +
  xlab("Free Sulfur Dioxide Concentration")

total_dioxide_sulfur_plot <- ggplot(data = wine, aes(x = total.sulfur.dioxide, fill = Col)) +
  geom_bar(width = 10) + 
  facet_grid(~Col) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Total Sulfur Dioxide Concentration Distribution") +
  ylab("Frequency") +
  xlab("Total Sulfur Dioxide Concentration")

sulfites_plot <- ggplot(data = wine, aes(x = sulphates, fill = Col)) +
  geom_bar(width = .05) + 
  facet_grid(~Col) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Sulfites Distribution") +
  ylab("Frequency") +
  xlab("Sulfites")

grid.arrange(free_dioxide_sulfur_plot, total_dioxide_sulfur_plot, sulfites_plot, ncol = 1, nrow = 3)

free_sulfur_dioxide_boxplot <- ggplot(data = wine, aes(x = Col, y = free.sulfur.dioxide, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Free Sulfur Dioxide Concentration Boxplot") +
  ylab("Free Sulfur Dioxide Concentration") +
  xlab("")

total_sulfur_dioxide_boxplot <- ggplot(data = wine, aes(x = Col, y = total.sulfur.dioxide, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Total Sulfur Dioxide Concentration Boxplot") +
  ylab("Total Sulfur Dioxide Concentration") +
  xlab("")

sulphates_boxplot <- ggplot(data = wine, aes(x = Col, y = sulphates, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Sulpfites Concentration Boxplot") +
  ylab("Sulfites Concentration") +
  xlab("")

grid.arrange(free_sulfur_dioxide_boxplot, total_sulfur_dioxide_boxplot, sulphates_boxplot, 
             ncol = 1, nrow = 3)

################################################################################
########################## 2.5 Other Aspects ###################################
################################################################################

# search for the density in wine (info) and ph, degree... 

density_plot <- ggplot(data = wine, aes(x = density, fill = Col)) +
  geom_bar(width = .0015) + 
  facet_grid(~Col) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Density Distribution") +
  ylab("Frequency") +
  xlab("Density")

pH_plot <- ggplot(data = wine, aes(x = pH, fill = Col)) +
  geom_bar(width = .1) + 
  facet_grid(~Col) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("pH Distribution") +
  ylab("Frequency") +
  xlab("pH")

alcohol_plot <- ggplot(data = wine, aes(x = alcohol, fill = Col)) +
  geom_bar(width = .5) + 
  facet_grid(~Col) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Alcohol Degree Distribution") +
  ylab("Frequency") +
  xlab("Alcohol Degree")

grid.arrange(density_plot, pH_plot, alcohol_plot, ncol = 1, nrow = 3)

density_boxplot <- ggplot(data = wine, aes(x = Col, y = density, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Density Boxplot") +
  ylab("Density") +
  xlab("")

pH_boxplot <- ggplot(data = wine, aes(x = Col, y = pH, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("pH Boxplot") +
  ylab("pH") +
  xlab("")

alcohol_boxplot <- ggplot(data = wine, aes(x = Col, y = alcohol, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Alcohol Degree Boxplot") +
  ylab("Alcohol Degree") +
  xlab("")

grid.arrange(density_boxplot, pH_boxplot, alcohol_boxplot, ncol = 1, nrow = 3)

################################################################################
########################## 2.6 Quality #########################################
################################################################################

quality_plot <- ggplot(data = wine, aes(x = quality, fill = Col)) +
  geom_bar(width = .5) + 
  facet_grid(~Col) +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Quality Distribution") +
  ylab("Frequency") +
  xlab("Quality")
quality_plot

quality_boxplot <- ggplot(data = wine, aes(x = Col, y = quality, fill = Col)) +
  geom_boxplot() +
  scale_fill_manual(name  = "Type of wine: ",
                    values = c("#a2231d", "#e4d96f")) +
  theme(
    legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
    legend.title = element_text(size = 10),
    plot.title = element_text(color = "black", size = 20)
  ) +
  ggtitle("Quality Boxplot") +
  ylab("Quality") +
  xlab("")
quality_boxplot

################################################################################
########################## 3.0 Quantity fix  ###################################
################################################################################
red_wine[, "quality"] <- as.numeric(red_wine[, "quality"])

for (i in 1:nrow(red_wine)){
  if (red_wine[i, "quality"] <= 5){
    red_wine[i, "quality"] = 1
  }
  else if (red_wine[i, "quality"] == 6){
    red_wine[i, "quality"] = 2
  }
  else if(red_wine[i, "quality"] > 6){
    red_wine[i,"quality"] = 3
  }
}

red_wine[, "quality"] <- as.factor(red_wine[, "quality"])
str(red_wine[, "quality"])
table(red_wine[, "quality"])


white_wine[, "quality"] <- as.numeric(white_wine[, "quality"])

for (i in 1:nrow(white_wine)){
  if (white_wine[i, "quality"] <= 5){
    white_wine[i, "quality"] = 1
  }
  else if (white_wine[i, "quality"] == 6){
    white_wine[i, "quality"] = 2
  }
  else if(white_wine[i, "quality"] > 6){
    white_wine[i,"quality"] = 3
  }
}

white_wine[, "quality"] <- as.factor(white_wine[, "quality"])
str(white_wine[, "quality"])
table(white_wine[, "quality"])

################################################################################
########################## 3.1 Linear Models  ##################################
################################################################################

### Red Wine
# linear_model_4d_rw <- lm(data = red_wine, fixed.acidity ~ citric.acid + density + pH)
# summary(linear_model_4d_rw)
linear_model_3d_one_rw <- lm(data = red_wine, density ~ pH + citric.acid)
summary(linear_model_3d_one_rw)

linear_model_3d_one_rw_al <- lm(data = red_wine, density ~ pH * citric.acid)
summary(linear_model_3d_one_rw_al)

anova(linear_model_3d_one_rw, linear_model_3d_one_rw_al)

## There is enough evidence to consider only the second model.

linear_plot_3d_one_rw <- plot_ly(data = red_wine, z= ~density, y= ~pH, x= ~citric.acid, 
                                 color= ~quality, 
                                 colors=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                                 size = 0.2)
linear_plot_3d_one_rw <- linear_plot_3d_one_rw %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Density'),
                                                                                         
                                                                                         yaxis = list(title = 'pH'),
                                                                                         
                                                                                         zaxis = list(title = 'Citric Acid Concentration')))
linear_plot_3d_one_rw

linear_model_3d_two_rw <- lm(data = red_wine, density ~ pH + fixed.acidity)
summary(linear_model_3d_two_rw)

linear_model_3d_two_rw_al <- lm(data = red_wine, density ~ pH * fixed.acidity)
summary(linear_model_3d_two_rw_al)

anova(linear_model_3d_two_rw, linear_model_3d_two_rw_al)

linear_plot_3d_two_rw <- plot_ly(data = red_wine, z= ~density, y= ~pH, x= ~fixed.acidity, 
                                 color= ~quality, 
                                 colors=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                                 size = 0.2)
linear_plot_3d_two_rw <- linear_plot_3d_two_rw %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Density'),
                                                                                         
                                                                                         yaxis = list(title = 'pH'),
                                                                                         
                                                                                         zaxis = list(title = 'Fixed Acidity')))
linear_plot_3d_two_rw

anova(linear_model_3d_one_rw, linear_model_3d_two_rw_al)

###White Wine
linear_model_3d_one_ww <- lm(data = white_wine, density ~ residual.sugar + total.sulfur.dioxide)
summary(linear_model_3d_one_ww)

linear_model_3d_one_ww_al <- lm(data = white_wine, density ~ residual.sugar * total.sulfur.dioxide)
summary(linear_model_3d_one_ww_al)

anova(linear_model_3d_one_ww, linear_model_3d_one_ww_al)

linear_plot_3d_one_ww <- plot_ly(data = white_wine, z= ~density, y= ~residual.sugar, x= ~total.sulfur.dioxide, 
                                 color= ~quality, 
                                 colors=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                                 size = 0.2)
linear_plot_3d_one_ww <- linear_plot_3d_one_ww %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Total Sulfur Dioxide'),
                                                                                         
                                                                                         yaxis = list(title = 'Residual Sugar Concentration'),
                                                                                         
                                                                                         zaxis = list(title = 'Density')))
linear_plot_3d_one_ww

linear_model_3d_two_ww <- lm(data = white_wine, density ~ alcohol + residual.sugar)
summary(linear_model_3d_two_ww)

linear_model_3d_two_ww_al <- lm(data = white_wine, density ~ alcohol * residual.sugar)
summary(linear_model_3d_two_ww_al)

anova(linear_model_3d_two_ww, linear_model_3d_two_ww_al)

# 3d representation => we have to deal in how to create the surface 
# coef <- linear_model_3d_two_ww$coefficients
# coef
# expected_z <- coef[1] + coef[2]*white_wine$alcohol + coef[3]*white_wine$residual.sugar
# white_wine_temp <- white_wine
# white_wine_temp[, "z"] = expected_z
linear_plot_3d_two_ww <- plot_ly(data = white_wine, z= ~density, y= ~residual.sugar, x= ~alcohol, 
                                 color= ~quality, 
                                 colors=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                                 size = 0.2)
linear_plot_3d_two_ww <- linear_plot_3d_two_ww %>% add_markers() %>%
  layout(scene = list(xaxis = list(title = 'Residual Sugar Concentration'),
                      yaxis = list(title = 'Alcohol'),
                      zaxis = list(title = 'Density')))
linear_plot_3d_two_ww

################################################################################
########################## 3.2 Non Linearities  ################################
################################################################################

# On the shiny app, we can create  a poly grade selector, that will be used to show how the model improve 
# its performances increasing the grade. Of course it leads to 2 problems: over-fitting and no logical interpretation.
# quadratic_model_one_rw <- lm(chlorides ~ pH + poly(sulphates, 10), data = red_wine)
# summary(quadratic_model_one_rw) 

###RED WINE
# density and (pH citric acid)
quadratic_model_one_rw_1 <- lm(density ~ pH + poly(citric.acid, 2), data = red_wine)
summary(quadratic_model_one_rw_1) 

quadratic_model_one_rw_2 <- lm(density ~ pH + I(citric.acid**2), data = red_wine)
summary(quadratic_model_one_rw_2) 

quadratic_model_one_rw_3 <- lm(density ~ poly(pH, 2) + poly(citric.acid, 2), data = red_wine)
summary(quadratic_model_one_rw_3) 

quadratic_model_one_rw_4 <- lm(density ~ poly(pH, 2) + citric.acid, data = red_wine)
summary(quadratic_model_one_rw_4) 

quadratic_model_one_rw_5 <- lm(density ~ I(pH**2) + citric.acid, data = red_wine)
summary(quadratic_model_one_rw_5) 

anova(quadratic_model_one_rw_2, quadratic_model_one_rw_3)

#density and (ph + fixed acidity)

quadratic_model_two_rw_1 <- lm(density ~ pH + poly(fixed.acidity, 2), data = red_wine)
summary(quadratic_model_two_rw_1) 

quadratic_model_two_rw_2 <- lm(density ~ pH + I(fixed.acidity**2), data = red_wine)
summary(quadratic_model_two_rw_2) 

quadratic_model_two_rw_3 <- lm(density ~ poly(pH, 2) + poly(fixed.acidity, 2), data = red_wine)
summary(quadratic_model_two_rw_3) 

quadratic_model_two_rw_4 <- lm(density ~ poly(pH, 2) + fixed.acidity, data = red_wine)
summary(quadratic_model_two_rw_4) 

quadratic_model_two_rw_5 <- lm(density ~ I(pH**2) + fixed.acidity, data = red_wine)
summary(quadratic_model_two_rw_5) 

anova(quadratic_model_two_rw_1, quadratic_model_two_rw_3)


quadratic_plot_3d_one_rw <- plot_ly(data = red_wine, z= ~density, y= ~pH**2, x= ~citric.acid**2, 
                                 color= ~quality, 
                                 colors=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                                 size = 0.2)
quadratic_plot_3d_one_rw <- quadratic_plot_3d_one_rw  %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Citric Acid Concentration'),
                                                                                         
                                                                                         yaxis = list(title = 'pH'),
                                                                                         
                                                                                         zaxis = list(title = 'Density'))) 
quadratic_plot_3d_one_rw


quadratic_plot_3d_two_rw <- plot_ly(data = red_wine, z= ~density, y= ~pH, x= ~fixed.acidity**2, 
                                    color= ~quality, 
                                    colors=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                                    size = 0.2)
quadratic_plot_3d_two_rw <-  quadratic_plot_3d_two_rw %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Fixed Acidity Concentration'),
                                                                                                
                                                                                                yaxis = list(title = 'pH'),
                                                                                                
                                                                                                zaxis = list(title = 'Density'))) 
quadratic_plot_3d_two_rw



###WHITE WINE

# density vs residual.sugar + total.sulfur.dioxide
quadratic_model_one_ww_1 <- lm(density ~ residual.sugar + poly(total.sulfur.dioxide, 2), data = white_wine)
summary(quadratic_model_one_ww_1) #.7482

quadratic_model_one_ww_2 <- lm(density ~ residual.sugar + I(total.sulfur.dioxide**2), data = white_wine)
summary(quadratic_model_one_ww_2) #.7448

quadratic_model_one_ww_3 <- lm(density ~ poly(residual.sugar, 2) + poly(total.sulfur.dioxide, 2), data = white_wine)
summary(quadratic_model_one_ww_3) #7582

quadratic_model_one_ww_4 <- lm(density ~ poly(residual.sugar, 2) + total.sulfur.dioxide, data = white_wine)
summary(quadratic_model_one_ww_4) #.7582

quadratic_model_one_ww_5 <- lm(density ~ I(residual.sugar**2) + total.sulfur.dioxide, data = white_wine)
summary(quadratic_model_one_ww_5) #.6859

anova(quadratic_model_one_ww_3, quadratic_model_one_ww_4)
## third model


# density vs residual.sugar + total.sulfur.dioxide
quadratic_model_two_ww_1 <- lm(density ~ residual.sugar + poly(alcohol, 2), data = white_wine)
summary(quadratic_model_two_ww_1) #.908

quadratic_model_two_ww_2 <- lm(density ~ residual.sugar + I(alcohol**2), data = white_wine)
summary(quadratic_model_two_ww_2) #.9079

quadratic_model_two_ww_3 <- lm(density ~ poly(residual.sugar, 2) + poly(alcohol, 2), data = white_wine)
summary(quadratic_model_two_ww_3) #9195

quadratic_model_two_ww_4 <- lm(density ~ poly(residual.sugar, 2) + alcohol, data = white_wine)
summary(quadratic_model_two_ww_4) #.9184

quadratic_model_two_ww_5 <- lm(density ~ I(residual.sugar**2) + alcohol, data = white_wine)
summary(quadratic_model_two_ww_5) #.8787

anova(quadratic_model_two_ww_3, quadratic_model_two_ww_4)
## fourth model

quadratic_plot_one_ww <- plot_ly(data = white_wine, z= ~density, y= ~residual.sugar**2, x= ~total.sulfur.dioxide**2, 
                                 color= ~quality, 
                                 colors=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                                 size = 0.2)
quadratic_plot_one_ww <- quadratic_plot_one_ww  %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Total Sulphur Dioxide Concentration squared'),
                                                                                          
                                                                                          yaxis = list(title = 'Residual Sugar Concentration squared'),
                                                                                          
                                                                                          zaxis = list(title = 'Density'))) 
quadratic_plot_one_ww

quadratic_plot_two_ww <- plot_ly(data = white_wine, z= ~density, y= ~residual.sugar**2, x= ~alcohol, 
                                 color= ~quality, 
                                 colors=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                                 size = 0.2)
quadratic_plot_two_ww <- quadratic_plot_two_ww  %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'Alcohol Degree'),
                                                                                          
                                                                                          yaxis = list(title = 'Residual Sugar Concentration squared'),
                                                                                          
                                                                                          zaxis = list(title = 'Density'))) 
quadratic_plot_two_ww

##GAMs

gam_model_one_rw <- gam(density ~ s(pH) + s(citric.acid), data = red_wine)
summary(gam_model_one_rw)

plot(gam_model_one_rw, residuals = TRUE, cex = 2)

gam_model_one_rw <- gam(density ~ s(pH) + s(fixed.acidity), data = red_wine)
summary(gam_model_one_rw)

plot(gam_model_one_rw, residuals = TRUE, cex = 2)

################################################################################
########################## 3.3 GLM  ############################################
################################################################################

###MULTINOMIAL LOGISTIC REGRESSION
###RED WINE

library(nnet)

red_wine[, "quality"] <- as.factor(red_wine[, "quality"])

set.seed(123)
indices <- createDataPartition(red_wine$quality, p = .75, list = F)
train_rw <- red_wine %>% slice(indices)

test_in_rw <- red_wine %>% slice(-indices) %>% select(-quality)
test_truth_rw <- red_wine %>% slice(-indices) %>% pull(quality)

multinomial_logistic_model_rw <- multinom(quality~ ., data = train_rw)
prediction_multinomial_model_rw <- predict(multinomial_logistic_model_rw, test_in_rw)

testing_rw_predictions <- confusionMatrix(prediction_multinomial_model_rw, test_truth_rw, mode = "everything")
testing_rw_predictions

###WHITE WINE
white_wine[, "quality"] <- as.factor(white_wine[, "quality"])

set.seed(123)
indices <- createDataPartition(white_wine$quality, p = .75, list = F)
train_ww <- white_wine %>% slice(indices)

test_in_ww <- white_wine %>% slice(-indices) %>% select(-quality)
test_truth_ww <- white_wine %>% slice(-indices) %>% pull(quality)

multinomial_logistic_model_ww <- multinom(quality~ ., data = train_ww)
prediction_multinomial_model_ww <- predict(multinomial_logistic_model_ww, test_in_ww)

testing_ww_predictions <- confusionMatrix(prediction_multinomial_model_ww, test_truth_ww, mode = "everything")
testing_ww_predictions

################################################################################
############################## 3.4 SVM Models ##################################
################################################################################

tune_out_linear <- tune(svm, quality~., data = train_rw, kernel = "linear",
                        ranges = list(cost = c(0.1,1,10,30)))
tune_out_linear$best.model

tune_out_radial <- tune(svm, quality~., data = train_rw, kernel = "radial",
                        ranges = list(cost = c(0.1,1,10),
                                      gamma = c(0.5,1,2,3,4)))
tune_out_radial$best.model
tune_out_radial$best.model$gamma

# create the svm model (LINEAR)

red_wine_svm_linear_rw <- svm(quality~ ., train_rw, kernel = "linear", 
                              scale = TRUE, cost = 1)

test_pred_linear_rw <- predict(red_wine_svm_linear_rw, test_in_rw)
table(test_pred_linear_rw)

confusion_matrix_linear_rw <- confusionMatrix(test_pred_linear_rw, test_truth_rw, mode = "everything")
confusion_matrix_linear_rw

## Second model, RADIAL

red_wine_svm_radial_rw <- svm(quality~ ., train_rw, kernel = "radial", gamma = 0.5,
                              scale = TRUE, cost = 10)

test_pred_radial_rw <- predict(red_wine_svm_radial_rw, test_in_rw)
table(test_pred_radial_rw)

confusion_matrix_radial_rw <- confusionMatrix(test_pred_radial_rw, test_truth_rw, mode = "everything")
confusion_matrix_radial_rw

## cross validation example

cross_validation_samples <- crossv_kfold(red_wine, k = 3)
train_cv_1 <- as.data.frame(cross_validation_samples$train$'1')
train_cv_2 <- as.data.frame(cross_validation_samples$train$'2')
train_cv_3 <- as.data.frame(cross_validation_samples$train$'3')

test_cv_1 <- as.data.frame(cross_validation_samples$test$'1')
test_cv_2 <- as.data.frame(cross_validation_samples$test$'2')
test_cv_3 <- as.data.frame(cross_validation_samples$test$'3')
#model 1
red_wine_svm_radial_rw <- svm(quality~ ., train_cv_1, kernel = "radial", gamma = 0.5,
                              scale = TRUE, cost = 10)

test_pred_radial_rw <- predict(red_wine_svm_radial_rw, test_cv_1[,1:11])

confusion_matrix_radial_rw <- confusionMatrix(test_pred_radial_rw, test_cv_1[,12], mode = "everything")
confusion_matrix_radial_rw
#model 2
red_wine_svm_radial_rw <- svm(quality~ ., train_cv_2, kernel = "radial", gamma = 0.5,
                              scale = TRUE, cost = 10)

test_pred_radial_rw <- predict(red_wine_svm_radial_rw, test_cv_2[,1:11])

confusion_matrix_radial_rw <- confusionMatrix(test_pred_radial_rw, test_cv_2[,12], mode = "everything")
confusion_matrix_radial_rw
#model 3
red_wine_svm_radial_rw <- svm(quality~ ., train_cv_3, kernel = "radial", gamma = 0.5,
                              scale = TRUE, cost = 10)

test_pred_radial_rw <- predict(red_wine_svm_radial_rw, test_cv_3[,1:11])

confusion_matrix_radial_rw <- confusionMatrix(test_pred_radial_rw, test_cv_3[,12], mode = "everything")
confusion_matrix_radial_rw

################################################################################
############################## 3.5 ANN Models ##################################
################################################################################

set.seed(123)
neural_net_model_rw <- neuralnet(quality~ ., train_rw, hidden = c(3,4), threshold = 0.5)

prediction_neural_net_model_rw <- compute(neural_net_model_rw, test_in_rw)
prediction_neural_net_model_rw <- apply(prediction_neural_net_model_rw$net.result, 1, which.max)
prediction_neural_net_model_rw <- factor(levels(test_truth_rw)[prediction_neural_net_model_rw], levels = levels(test_truth_rw))
confusion_matrix_nn_rw <- confusionMatrix(prediction_neural_net_model_rw, test_truth_rw,mode = "everything")
confusion_matrix_nn_rw

###WHITE WINE

set.seed(123)
neural_net_model_ww <- neuralnet(quality~ ., train_ww, hidden = c(3,4), threshold = 0.5)

prediction_neural_net_model_ww <- compute(neural_net_model_ww, test_in_ww)
prediction_neural_net_model_ww <- apply(prediction_neural_net_model_ww$net.result, 1, which.max)
prediction_neural_net_model_ww <- factor(levels(test_truth_ww)[prediction_neural_net_model_ww], levels = levels(test_truth_ww))
confusion_matrix_nn_ww <- confusionMatrix(prediction_neural_net_model_ww, test_truth_ww, mode = "everything")
confusion_matrix_nn_ww
