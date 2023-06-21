
################################################################################
########################## 1. extraction of data ###############################
################################################################################
library(shinydashboard)
library(shiny)
library(ggplot2)
library(plotly)
library(GGally)
library(tibble)
library(gridExtra)
library(mgcv)
library(magrittr)
library(nnet)
library(lattice)
library(caret)
source('project_script.R')
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
###RED WINE


red_wine[, "quality"] <- as.factor(red_wine[, "quality"])

set.seed(123)
indices <- createDataPartition(red_wine$quality, p = .75, list = F)
train_rw <- red_wine %>% slice(indices)

###WHITE WINE



white_wine[, "quality"] <- as.factor(white_wine[, "quality"])

set.seed(123)
indices <- createDataPartition(white_wine$quality, p = .75, list = F)
train_ww <- white_wine %>% slice(indices)



wine <- red
wine <- add_row(wine, white)

eda_wine_column_selection <- subset(wine, select= -c(quality,residual.sugar)) 


aliasName <- c("Fixed Acidity","Volatile Acidity","Citric Acid","Residual Sugar","Chlorides",
               "Free Sulfur Dioxide","Total Sulfur Dioxide","Density","pH","Sulphates","Alcohol","Quality","Color")

selector_alias <- c("Fixed Acidity","Volatile Acidity","Citric Acid","Chlorides",
               "Free Sulfur Dioxide","Total Sulfur Dioxide","Density","pH","Sulphates","Alcohol")



################################################################################
########################## 2.1 General Overview #################################
################################################################################

CorrelationPlot <- ggpairs(wine,                 
                        columns = 1:11,       
                        aes(color = Col,
                        alpha = 0.5)) +
                  scale_fill_manual(values = c("#a2231d", "#e4d96f")) + 
                  scale_color_manual(values = c("#a2231d", "#e4d96f"))

################################################################################
########################## 2.3 Residual Sugar ##################################
################################################################################

# https://winefolly.com/deep-dive/what-is-residual-sugar-in-wine/
##Distribution
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

###Boxplot 
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

################################################################################
########################## 3.1 Linear Models  ##################################
################################################################################

################################################################################
########################## 3.2 Non Linearities  ################################
################################################################################

################################################################################
########################## 3.3 SVM  ################################
################################################################################

library(tidyverse)
library(mgcv)
library(plotly)
library(GGally)
library(gridExtra)
library(DT)
library(neuralnet)
library(e1071)
library(caret)
library(FSinR)
library(nnet)
library(kernlab)
#####################################################################################################
########################## Shiny Dashboard Script ###################################################
#####################################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "Machine Learning 1 Project"),
  dashboardSidebar(
    
    sidebarMenu(style = "position: fixed; overflow: visible;",
      menuItem(text = "Intro and Preprocessing", tabName = "Intro_preprocessing", icon = icon("folder-open")),
      menuItem(text = "EDA", tabName = "EDA", icon = icon("folder-open")),
      menuItem(text = "Modelling", tabName = "Modelling", icon = icon("folder-open")),
      menuItem(text = "Prediction", tabName = "Prediction", icon = icon("folder-open")),
      menuItem(text = "Conclusion and Questions", tabName = "Conclusion_Business_Questions", icon = icon("folder-open")),
      menuItem(text = "About us", tabName = "About_us", icon = icon("folder-open"))
    )
  ),
  dashboardBody(
    tags$style(HTML("
      .content-wrapper {
        min-height: 600px;
      }
      .box {
        min-width: 200px;
        
      }
      .dataTable {
        min-width: 500px;
        max-width: 800px;
      }
      .plot {
        min-width: 800px;
      }
    ")),
    tabItems(
      
      #################################Introduction and Preprocessing Page and tabs#################################
      tabItem(tabName = "Intro_preprocessing",
              fluidRow(
                box(title = h2("Introduction"),style = "  min-width: 1000px",
                    div(style = "justify-content: center;  min-width: 1000px",
                        h3("Abstract:"),
                        h4("The wine industry is a significant contributor to the global economy, with numerous stakeholders, including producers, consumers, and experts, constantly seeking to improve wine quality. In this context, an anonymous wine producer asked our group to analyse and define the quality of his products, comparing them with a comprehensive dataset containing diverse wine samples and their corresponding quality ratings. To reach the purpose, a deep analysis of physicochemical aspects and their relations was fundamental. In this article, there are three suggestions about how to develop a satisfying model to assess wine quality. "),
                        h3("Introduction:"),
                        h4("Grape variety, climate, production techniques, and chemical composition are some of the factors that influence wine quality. To meet customers expectations and maintain the reputation, wine producers have to accurately assess the quality of their wine. 
                        Traditional wine quality assessment methods rely on sensory evaluations of a sommelier, which could be expensive and affected by subjectivity. Therefore, machine learning can be a promising alternative to assess wine quality based on chemical properties, providing an objective approach. 

                        The final aim of this project is to use machine learning techniques to build a predictive model that can assess the wine quality of our anonymous customer’s products. This project’s approach consists in understanding what physicochemical factors impact the most on wine quality and training three different machine learning algorithms to predict it. Using appropriate metrics, the results were evaluated. The resulting model can be used in the optimisation of the processes, as well as to assist consumers and retailers in selecting high quality wines. "),
                        h3("Context:"),
                        h4("The goal of this study is to use machine learning techniques to estimate the quality of wine based on its physicochemical characteristics. In particular, our anonymous customer was searching for an efficient way to classify some of his products, based on a chemical analysis. 
                        The development of a machine learning model to assess wine quality can offer an objective, effective, and economical method to enhance decision making for wine producers, customers and merchants. ") 
                        ),width = 10),
                    
                box(title = h2("Data Cleaning and Preprocessing"),
                    div(style = "justify-content: center;  min-width: 1000px",
                    h4("The definitive data frame is made using two datasets that represent red and white wine. These datasets are initially read using the read.csv function, with the sep argument choosing a semicolon separator. Then, these datasets are stored in the red_wine and white_wine variables.

                        Data entry errors in the fields “volatile acidity” and “density” were fixed. In fact, in both categories the data were expressed in two different measurement units and have to be changed. This procedure helps to guarantee that the data are consistently arranged for examination. The program then purges the information and adds a new column with the name Col to the datasets for both red and white wine. This column is used to store the colour of the wine, which can be Red or White. This process helps to distinguish the two varieties of wine when they are mixed in the subsequent steps.

                        At the end of this code the wine dataframe is created by joining the cleaned datasets for red and white wine. The add_row method is used to merge the rows of the two databases. The structure of the combined dataframe wine is then shown using the str() method, giving an overview of the format and structure of the data.")),
                    
                    div(style = "justify-content: center; min-height: 500px",
                        h4("Summary of Red wine data"),
                        verbatimTextOutput("terminalOutputIntro1")
                    ),
                    div(style = "justify-content: center; min-height: 500px",
                        h4("Summary of white wine data"),
                        verbatimTextOutput("terminalOutputIntro2")
                    ),width = 10)
              )
      ),
      
      
      #################################EDA Page and tabs#################################
      tabItem(tabName = "EDA",
              fluidRow(
                
                box(title = h2("Correlation"),
                    div(style = "justify-content: left; min-height: 400px",
                        h3("Correlation Plot"),
                        h4("In this section, the reader can see a matrix where each cell represents a pairwise link between two variables from the dataset. The only column that is missing is the outcome, quality. The matrix is divided into three parts, the main diagonal represents the density of each variable. In the lower triangular matrix, the reader can see all the pairwise scatterplots, while in the upper triangular matrix are stated the pairwise Pearson coefficient values."),
                        plotOutput(outputId="Correlation_Plot", height = "700px")
                        )
                    , width = 12),
                
                ######Acidities#####
                box(title = h2("Acidities"),
                    h4("In this section, the reader can examine bar plots and boxplots regarding four pivotal measurements of acidity. In particular, in this project, the studied measurements are fixed and volatile acidity, the concentration of citric acid and chloride concentration.

* Fixed Acidity: it corresponds to the set of low volatility organic acids such as malic, lactic, tartaric or citric acids. The measurement unit is g/L

* Volatile Acidity: it corresponds to the set of short-chain organic acids that can be extracted from the sample through a distillation process: formic acid, acetic acid, propionic acid and butyric acid. The measurement unit is g/L.

* Citric Acid: it is often added to wines to increase acidity in the wine-making process, complement a specific flavour or prevent ferric hazes. The measurement unit is g/L.

* Chlorides: the concentration of chloride ions is generally indicative of the presence of sodium chloride5. Sodium chloride adds to the saltiness of a wine, which can contribute to or detract from the overall taste and quality of the wine. The concentration is measured in mg/L.


The output can be examined to spot any potential variations in the chemical properties of red and white wines. For instance, the distributions of the fixed acidity and volatile acidity may differ significantly across red and white wines, although the concentrations of citric acid and chlorides may exhibit more minor variations. Combining bar plots and boxplots makes it easier to comprehend the variations in the distributions of different chemical properties, which might guide future research or feature choice in predictive modelling."),
                    fluidRow(
                      column(width = 1,
                             div(style = "display: flex; justify-content: left;",
                                 sidebarPanel(
                                   p("Select from the menu:"),
                                   style = "min-width: 200px; max-width: 100%;",
                                   #selectInput("select_Wine", "Wine:", width = 200, choices = c("White", "Red")),
                                   selectInput("select_Substance_acidity", "Substance:",
                                               width = 200, 
                                               choices = setNames(names(subset(wine, select= c(fixed.acidity ,volatile.acidity,citric.acid, chlorides))),c("Fixed Acidity","Volatile Acidity","Citric Acid","Chlorides"))),
                                   selectInput("binWidth_acidity", "Bins Width:", width = 200, choices = c(0.01, 0.1, 0.15, 0.2, 0.3, 0.4))
                                 )
                             )
                      ),
                      column(width = 11,
                             div(style = "display: flex; justify-content: center;",
                                 mainPanel(
                                   fluidRow(
                                     column(6,
                                            div(
                                              p(""),
                                              style = "min-width: 500px; max-width: 100%;",
                                              plotOutput(outputId = "wine_acidities_substances_distribution", height = "500px")
                                            )
                                     ),
                                     column(6,
                                            div(
                                              p(""),
                                              style = "min-width: 500px; max-width: 100%;",
                                              plotOutput(outputId = "wine_acidity_substances_boxplot", height = "500px")
                                            )
                                     )
                                   )
                                 )
                             )
                      )
                    ),
                    width = 12
                ),
                ######Sulphates####
                box(title = h2("Sulphates"),
                    h4("In wine-making sulphates, compounds containing sulphur, are often used to preserve the organoleptic properties of the wine. They prevent oxidation and bacterial growth, ensuring that the wine maintains its quality and freshness. 
Throughout the fermentation process, sulphates are naturally produced. However, sometimes it happens that winemakers add them in the form of potassium metabisulfite or other sulphate salts.

Although sulphates play an important role in preserving wine, their levels are regulated by law to ensure consumer safety.

In this project, the focus is on two different types of sulphates:
 
* Free Sulphur Dioxide: The free SO2 is the unreacted component of sulphur dioxide present in the wine. Chemically speaking, it is made up of mostly the molecular (SO2) and bisulfite (HSO3 -) forms. The measurement unit is mg/L.

* Total Sulphur Dioxide: Total SO2 is the total amount of SO2 added, or the sum of the free and bound fractions. The measurement unit is 

* Sulphates: In this case Sulphates states for potassium sulphates. In particular, the presence of potassium metabisulfite is fundamental in wine-making. It is an antioxidant and bactericide that releases sulphur dioxide into wine must. The measurement unit is g/L."),
                    
                      fluidRow(
                        column(width = 1,
                               div(style = "display: flex; justify-content: left;",
                                   sidebarPanel(
                                     p("Select from the menu:"),
                                     style = "min-width: 200px; max-width: 100%;",
                                     #selectInput("select_Wine", "Wine:", width = 200, choices = c("White", "Red")),
                                     selectInput("select_Substance_sulphates", "Substance:", width = 200,
                                                 choices = setNames(names(subset(wine, select= c(free.sulfur.dioxide,total.sulfur.dioxide,sulphates))),
                                                                    c("Free Sulfur Dioxide","Total Sulfur Dioxide","Sulphates"))),
                                     selectInput("binWidth_sulphates", "Bins Width:", width = 200, choices = c(0.01, 0.1, 0.15, 0.2, 0.3, 0.4))
                                   )
                               )
                        ),
                        column(width = 11,
                               div(style = "display: flex; justify-content: center;",
                                   mainPanel(
                                     fluidRow(
                                       column(6,
                                              div(
                                                p(""),
                                                style = "min-width: 500px; max-width: 100%;",
                                                plotOutput(outputId = "wine_sulphates_substances_distribution", height = "500px")
                                              )
                                       ),
                                       column(6,
                                              div(
                                                p(""),
                                                style = "min-width: 500px; max-width: 100%;",
                                                plotOutput(outputId = "wine_sulphates_substances_boxplot", height = "500px")
                                              )
                                       )
                                     )
                                   )
                               )
                        )
                      ),
                      width = 12
                ),
                #####Other#####
                box(title = h2("Other Parameters"),
                    h4("Wine sugar, alcohol and other dissolved solids inside the wine are responsible for the density value of the wine. Density in wine-making is essential to gather information regarding the fermentation progress. 

The pH scale is a universal measurement unit to define the acidity or basicity of a solution. 
Wine’s pH depends on the impact of several factors such as grape variety, soil, environment, as well as methods employed. In addition, it is a common practice to alter the acidity levels, so the pH, to develop a particular taste or style. Understanding and controlling pH levels can aid winemakers in producing wines with the right flavour character and level of acidity.
While acidity and pH are related, they are not interchangeable. A wine can have high acidity but relatively low pH if the acids present are weak, while a wine can have low acidity but high pH if the acids present are strong.

The alcohol degree is a measure of the amount of alcohol present in a given volume of wine, expressed in percentage. It is essential in determining the wine’s body, texture, and balance.
Generally, the alcohol degree ranges between the values of 5% to 20%. "),
                    fluidRow(
                      column(width = 1,
                             div(style = "display: flex; justify-content: left;",
                                 sidebarPanel(
                                   p("Select from the menu:"),
                                   style = "min-width: 200px; max-width: 100%;",
                                   #selectInput("select_Wine", "Wine:", width = 200, choices = c("White", "Red")),
                                   selectInput("select_Substance_other", "Substance:", width = 200,
                                               choices = setNames(names(subset(wine,select=c(density,pH,alcohol))),c("Density","pH","Alcohol"))),
                                   selectInput("binWidth_other", "Bins Width:", width = 200, choices = c(0.01, 0.1, 0.15, 0.2, 0.3, 0.4))
                                 )
                             )
                      ),
                      column(width = 11,
                             div(style = "display: flex; justify-content: center;",
                                 mainPanel(
                                   fluidRow(
                                     column(6,
                                            div(
                                              p(""),
                                              style = "min-width: 500px; max-width: 100%;",
                                              plotOutput(outputId = "wine_other_substances_distribution", height = "500px")
                                            )
                                     ),
                                     column(6,
                                            div(
                                              p(""),
                                              style = "min-width: 500px; max-width: 100%;",
                                              plotOutput(outputId = "wine_other_substances_boxplot", height = "500px")
                                            )
                                     )
                                   )
                                 )
                             )
                      )
                    ),
                    width = 12
                ),
                ###### Residual Sugar#####
                box(title = h2("Residual Sugar"),
                    h4("Residual sugar concentration plays a crucial role in wine sweetness. Moreover, it contributes to the overall taste and mouthfeel of wine. Different concentrations of residual sugar are categorised in different wine styles. 

Wine sweetness levels can be classified into different categories based on their residual sugar content:

* Bone dry: 0 - 1 g/L
* Dry: 1 - 17 g/L
* Off-dry: 17 - 35 g/L
* Medium Sweet: 35 - 120 g/L
* Sweet: 120 - 220+ g/L

Residual sugar is usually measured in grams per litre (g/L) or as a percentage of the wine's total volume. Wines with higher residual sugar levels are considered sweeter, while those with lower levels are considered drier. The perception of sweetness, however, can be influenced by other factors such as acidity, tannins, and alcohol content.

These categories provide a general guideline for understanding the sweetness of a wine, but the individual perception of sweetness may vary. In addition, it is necessary to specify that it is not the purpose of this project to work with different classes of residual sugar concentration. 
"),
                    fluidRow(
                      column(width = 11,
                             div(style = "display: flex; justify-content: center;",
                                 mainPanel(
                                   fluidRow(
                                     column(6,
                                            div(
                                              p(""),
                                              style = "min-width: 500px; max-width: 100%;",
                                              plotOutput(outputId = "residual_sugar_distribution", height = "500px")
                                            )
                                     ),
                                     column(6,
                                            div(
                                              p(""),
                                              style = "min-width: 500px; max-width: 100%;",
                                              plotOutput(outputId = "residual_sugar_boxplot", height = "500px")
                                            )
                                     )
                                   )
                                 )
                             )
                      )),
                    width = 12),
                #####Quality#####
                box(title = h2("Quality"),
                    h4("Wine quality is a mixture of several elements: 

* Acidity: The right balance of acidity is crucial for a wine's freshness, structure, and ageability.
* Tannins: Tannins, mainly found in red wines, contribute to the wine's structure, texture, and ageing potential.
* Sugar: The residual sugar in wine can affect its sweetness, body, and balance.
* Alcohol: The alcohol content influences the wine's body, texture, and overall balance.
* Aroma and flavour compounds: The wine's aroma and flavour profile are essential for its complexity, appeal, and distinctiveness. These compounds can come from the grapes, fermentation process, or ageing vessels.

In summary, wine quality is a result of multiple factors in the production process, from grape cultivation to winemaking techniques. Attention to detail at each stage and a focus on maintaining balance and harmony among the various elements in the wine contribute to a high-quality end product.

The provided code generates an interactive bar plot that shows the distribution of wine quality for red and white wines. A scale from 1 to 10 was used to rate the wine quality for both red and white wines. 
"),
                    fluidRow(
                      column(width = 11,
                             div(style = "display: flex; justify-content: center;",
                                 mainPanel(
                                   fluidRow(
                                     column(6,
                                            div(
                                              p(""),
                                              style = "min-width: 500px; max-width: 100%;",
                                              plotOutput(outputId = "quality_distribution", height = "500px")
                                            )
                                     ),
                                     column(6,
                                            div(
                                              p(""),
                                              style = "min-width: 500px; max-width: 100%;",
                                              plotOutput(outputId = "quality_boxplot", height = "500px")
                                            )
                                     )
                                   )
                                 )
                             )
                      )),
                      width = 12)
              )
      ),
      
      ################################# Modelling Page and tabs #################################
      tabItem(tabName = "Modelling",
              
              
              fluidRow(
                box(h3("Questions Regarding the Modelling"),
                    h4("In the modelling section, the aim is to answer our anonymous customer’s questions. The techniques that were employed to fulfil the task are various and depend on the requirements.
                        
                        
                        These are the questions:
                        
                        
                        1. “In the company, we would like to have control of the value of density during the process. Unfortunately, our machines can calculate only chemical concentrations during wine-making. Can you find an easy-to-understand model to estimate the density starting from physicochemical factors?”
                        
                        
                        2. “Can you create a model that can predict the quality of a wine, given its physicochemical properties? If it is possible, what is the precision of the model, can we rely on it?”
                        
                        In the following subsections, the reader can find our strategy to succeed in satisfying our customer needs."),width=12),
                
              ##########Linear model#########
                box(
                  title = h2("Linear Regression Model"),
                  h4("In this section, the aim is to answer the first question of our anonymous customer trying to keep it simple. In particular, linear regression can be an easy and very understandable way to address the problem. Therefore, two linear regression models were created. "),
                  width = 12,
                  fluidRow(
                    column(1,
                           div(style = "display: flex; justify-content: left;",
                               sidebarPanel(
                                 p("Select parameters of the linear model"),
                                 style = "min-width: 200px; max-width: 100%;",
                                 
                                 selectInput("WineType", "Wine Type:", width = 200, choices = c("White","Red")),
                                 selectInput("select_Substance_Y", "Substance as Y:", width = 200, choices=setNames(c("density"), c("Density")),selected = "Select"),
                                 selectInput("select_Substance_X1", "Substance as X1:", width = 200, choices = setNames(names(wine),aliasName),selected = "Select"),
                                 selectInput("select_Substance_X2", "Substance as X2:", width = 200, choices = setNames(names(wine),aliasName),selected = "Select"),
                                 fluidRow(actionButton("add_btn", "Add Row"),
                                          actionButton("delete_btn", "Delete Row"))
                                 
                               )
                           )
                    ),
                    column(6,
                           div(style = "display: flex; justify-content: right;",
                               mainPanel(
                                 
                                 style = "min-width: 300px; max-width: 100%;",
                                 dataTableOutput("mytable")
                               )
                           )
                    )
                  ),
                  
                  tabsetPanel(
                    id = "menu1_submenu1_tabs",
                    tabPanel("Model summary",
                             div(style = "justify-content: center; min-height: 700px;",
                                 h4("Linear Model Summary"),
                                 verbatimTextOutput("terminalOutputModelling1")
                             )
                             
                    ),
                    tabPanel("3D plot",
                             div(
                               style = "justify-content: left; min-height: 700px;",
                               h4("Model Plot"),
                               plotlyOutput(outputId="linearModelplot",
                                            height = 600) )
                    )
                  
                  )),
              
              ########## Non-Linear model #########
                box(title = h2("Non Linearities"), width = 12,
                    h4("In the Non-Linearities chapter, the final aim is still to answer the first question of our anonymous customer. The methodology is always the same, finding an easy linear model that can be used to estimate the density parameter based on the physicochemical properties of the wine. 

The section will follow the following structure for both wine types considered:

1. Polynomial relationship in the Linear Model

2. Regression and Smooth Splines

3. Generalised Additive Model"),
                    fluidRow(
                      column(1,
                             div(style = "display: flex; justify-content: left;",
                                 sidebarPanel(
                                   p("Select parameters of the Non linear model"),
                                   style = "min-width: 200px; max-width: 100%;",
                                   selectInput("NonLinearModelSelection", "Non-linear model:", width = 200, choices = c("1","2","3","4","5","6(GAM)")),
                                   selectInput("WineTypeNonLinear", "Wine Type:", width = 200, choices = c("White","Red")),
                                   selectInput("NL_select_Substance_Y", "Substance as Y:", width = 200, choices=setNames(c("density"), c("Density")),selected = "Select"),
                                   selectInput("NL_select_Substance_X1", "Substance as X1:", width = 200,
                                               choices = names(subset(wine, select = -c(density, quality, Col))),selected = "Select"),
                                   selectInput("NL_select_Substance_X2", "Substance as X2:", width = 200,
                                               choices = names(subset(wine, select = -c(density, quality, Col))),selected = "Select"),
                                   fluidRow(actionButton("add_btn_NL", "Add Row"),
                                            actionButton("delete_btn_NL", "Delete Row"))
                                   
                                 )
                             )
                      ),
                      column(9,
                             div(style = "display: flex; justify-content: center; width:'300px'",
                                 mainPanel(
                                   column(6,
                                          HTML("
                        <h3>Mathematical Representation of Linear Models:</h3>
                        <ol style='font-size:13px;'>
                            <li>Y = β<sub>0</sub> + β<sub>1</sub> * X<sub>1</sub> + β<sub>2</sub> * X<sub>2</sub> + β<sub>3</sub> * X<sub>2</sub><sup>2</sup> + ε</li>
                            <li>Y = β<sub>0</sub> + β<sub>1</sub> * X<sub>1</sub> + β<sub>2</sub> * X<sub>2</sub><sup>2</sup> + ε</li>
                            <li>Y = β<sub>0</sub> + β<sub>1</sub> * X<sub>1</sub> + β<sub>2</sub> * X<sub>1</sub><sup>2</sup> + β<sub>3</sub> * X<sub>2</sub> + β<sub>4</sub> * X<sub>2</sub><sup>2</sup> + ε</li>
                            <li>Y = β<sub>0</sub> + β<sub>1</sub> * X<sub>1</sub> + β<sub>2</sub> * X<sub>1</sub><sup>2</sup> + β<sub>3</sub> * X<sub>2</sub> + ε</li>
                            <li>Y = β<sub>0</sub> + β<sub>1</sub> * X<sub>1</sub><sup>2</sup> + β<sub>2</sub> * X<sub>2</sub> + ε</li>
                            <li>Y = β₀ + f₁(X₁) + f₂(X₂) + ... + fₚ(Xₚ) +ε</li>
                        </ol>
                    ")
                                   ),
                                   column(6,
                                          dataTableOutput("mytable_NonLinear")
                                   )
                                 )
                             )
                      )
                    ),
                    
                    tabsetPanel(
                      id = "menu2_submenu2_tabs",
                      tabPanel("Model summary",
                               div(style = "justify-content: center; min-height: 700px;",
                                   h4("Linear Model Summary"),
                                   verbatimTextOutput("terminalOutputModelling2")
                               )
                               
                      ),
                      tabPanel("Plot",
                               div(
                                 style = "justify-content: left; min-height: 700px;",
                                 h4("Model Plot"),
                                 plotlyOutput(outputId="NonlinearModelplot",
                                              height = 600) )
                      )
                      
                    )),
                
              ###########SVM##########
                box(title = h2("SVM"),
                    h4("Support Vector Machines (SVM) are supervised learning models with associated learning algorithms to analyse data for classification and regression.
This classifier can be used to perform linear classification, as well as non-linear classification using the kernel trick, mapping the inputs into high-dimensional feature spaces.

In this project, SVM is used to answer our anonymous customer’ second question. In particular, the algorithm will try to predict wine quality using as independent variables all the other physicochemical properties of the wine.

For what concerns the implementation, the parameters of the model were boosted using the **tune( )** function. Afterwards, the model is trained and tested on the data partition created in the section before."),
                    fluidRow(
                      column(2,
                             div(style = "display: flex; justify-content: left;",
                                 sidebarPanel(
                                   p("Select parameters of the SVM model"),
                                   style = "min-width: 200px; max-width: 100%;",
                                   
                                   selectInput("WineTypeSVM", "Wine Type:", width = 200, choices = c("White","Red")),
                                   selectInput("Kernel", "Kernel:", width = 200, choices = c("linear","radial")),
                                   selectInput("Cost", "Cost:", width = 200, choices = c("0.01","0.1","1","2","5"))
                                   
                                 )
                             )
                      ),
                      column(8,div(style = "justify-content: center; min-height: 700px;",
                                   h4("SVM Model Summary"),
                                   verbatimTextOutput("terminalOutputModelling3")
                                  )
                             )
                    ), width = 12),
              ###############ANN############
                box(title = h2("ANN"),
                    h4("For completeness, the reader can have a look at the implementation of an ANN. 
ANNs are computing systems inspired by the biological neural networks that constitute animal brains. This is a supervised learning method to analyse data for classification and regression. 

In this project, a possible implementation of the ANN model is mentioned. However, the complexity behind the scenes makes it difficult to apply and interpret the models. Therefore, this section is more about showing the reader a possible implementation of a solution rather than a real suggestion. 


To be more specific, the neural networks created in this section present three hidden neurons and four layers. However, the complexity can be exponentially increased. What is truly problematic is defining the number of layers and what is working behind the function **neuralnet ( )**.
In addition, an obstacle is finding the right threshold for the model to work. 
"),
                    fluidRow(
                  column(2,
                         div(style = "display: flex; justify-content: left;",
                             sidebarPanel(
                               p("Select parameters of the ANN model"),
                               style = "min-width: 200px; max-width: 100%;",
                               
                               selectInput("WineTypeANN", "Wine Type:", width = 200, choices = c("White","Red"))
                               
                             )
                         )
                  ),
                  column(8,div(style = "justify-content: center; min-height: 700px;",
                               h4("ANN Model Summary"),
                               verbatimTextOutput("terminalOutputModelling4")
                  )
                  )
                ), width = 12),
                width= 12)
              
      ),
      
      
      
      
      
      #######################################Prediction#####################################################
      tabItem(tabName = "Prediction", width = 12,
              fluidRow(
                fluidRow(width = 6,style = " min-height: 700px",
                box(title = h2("Prediction"),
                    fluidRow(
                      
                      box(width = 12, align = "left",
                          selectInput("Model_Selector", label = "Select Model", choices = c("Multinomial", "ANN","SVM")),
                          selectInput("Wine_Selector", label = "Select Wine", choices = c("White", "Red")),
                          selectInput("Kernel_Selector", label = "Select Kernel (SVM)", choices = c("linear", "radial")),
                          
                          numericInput("Cost_Selector", label = "cost(SVM)", value = 0.01, step = 0.01, min = 0)
                             )),
                    ######new_wine#####
                    box(style = "justify-content: center; min-height: 400px",width=12,
                        h2("Input Numeric Values"),
                        div(style = "display: flex; flex-wrap: wrap; min-height: 700",
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("Fixed Acidity"),
                                numericInput("Fixed_Acidity", label = NULL, value = 0, step = 0.01, min = 0,max=15.90)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("Volatile Acidity"),
                                numericInput("Volatile_Acidity", label = NULL, value = 0, step = 0.01, min = 0,max=1.58)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("Citric Acid"),
                                numericInput("Citric_Acid", label = NULL, value = 0, step = 0.01, min = 0,max=1)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("Residual Sugar"),
                                numericInput("Residual_Sugar", label = NULL, value = 0, step = 0.01, min = 0,max=15.5)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("Chlorides"),
                                numericInput("Chlorides", label = NULL, value = 0, step = 0.01, min = 0,max=0.61)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("Free Sulfur Dioxide"),
                                numericInput("Free_Sulfur_Dioxide", label = NULL, value = 0, step = 0.01, min = 0,max=72)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("Total Sulfur Dioxide"),
                                numericInput("Total_Sulfur_Dioxide", label = NULL, value = 0, step = 0.01, min = 0,max=289)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("density"),
                                numericInput("density", label = NULL, value = 0, step = 0.01, min = 0,max=1)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("pH"),
                                numericInput("pH", label = NULL, value = 0, step = 0.01, min = 0,max=4)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("Sulphates"),
                                numericInput("Sulphates", label = NULL, value = 0, step = 0.01, min = 0,max=2)
                            ),
                            div(style = "flex-basis: 20%; padding: 5px;",
                                h5("alcohol"),
                                numericInput("alcohol", label = NULL, value = 0, step = 0.01, min = 0,max=14.90)
                            )
                        )
                    )
                    
                ),
                box(width = 6,style = " min-height: 700px",
                       div(style = "justify-content: left; min-height: 400px",
                           h2("Prediction Results"),
                           verbatimTextOutput("PredictionOutput")
                       )
                ))
                , width = 12))
      ,
      
      #################################Developments and Future tabs and page #################################
      tabItem(tabName = "Conclusion_Business_Questions",
              fluidRow(
                box(title = "Conclusion",h4("
                                            To summarise, in this project, the aim was to implement some models to deliver the most simple and usable answers to our anonymous customers.
                                            In particular, the document goes through a different set of Machine Learning tools, from simple Linear Regression to more advanced methods, such as SVM or ANN.
                                            The project structure is divided into two parts. The first is this technical document. It is not addressed only to the customer but also to all the readers interested in how to develop an analysis using the methodology illustrated above. On the other hand, the second part of our project is a Shiny Dashboard that can be useful to our customer to dynamically answer his questions. Moreover, he can also use this dashboard to broaden and, maybe, question the solutions proposed, creating new possibilities and playing with model features.
                                            
                                            The main question of this project were: 
                                            1. “In the company, we would like to have control of the value of density during the process. Unfortunately, our machines can calculate only chemical concentrations during wine-making. Can you find an easy-to-understand model to estimate the density starting from physicochemical factors?”
                                            
                                            2. “Can you create a model that can predict the quality of a wine, given its physicochemical properties? If it is possible, what is the precision of the model, can we rely on it?”"), width = 12),
                box(title = "Question 1",h4("To solve the first questions three different methods were used: a linear regression, a linear model that was able to parametrise non linearities, and the GAM model. In each case only two predictors were chosen. This is due to the commitment to simplicity of this paper. In fact, more complex linear models were possible. However, the more the predictors, the less the interpretability. Since the aim is to provide a solution that can be easily used, the decision taken by our team is to find all the linear models that can be representable in a plane (therefore, 3D). 
                                            To answer the question, the solution is dichotomised in an approach for red wine and another one for white wine.
                                            Concerning red wine, the model selected consists of density as a response and predictors pH and fixed acidity concentration. The R2 adjusted is around 50%, and the significance of each predictor is high enough (p-value close to zero). It could be a trivial way to model the density and find an estimation. Therefore, by employing this method, our customer can have the opportunity to estimate density while producing wine.
                                            
                                            The second part of the solution consists in a model for white wines. To satisfy our customer's requests, the designated model is the quadratic model number four. This linear model structure is the following: density as a response, quadratic polynomial function on the first predictor, residual sugar, and a linear second predictor, alcohol.
                                            In this case, the results are impressive. All the p-values are around zero. Moreover, the R2 adjusted is 92%. Therefore, this model was chosen since it ideally estimates density. Nevertheless, there exists a negative aspect. This model is slightly more complicated than a linear regression model on the same predictors. The suggestion is to use the right tools for prediction, as well as the Shiny App provided. If our customer thinks it could be too complicated, there is strong evidence that even the linear regression model can accurately fit the data (low p-values and 90% of R2 adjusted).
                                            "), width = 12),
                box(title = "Question 2",
                    h4("The second question presents some difficulties that have to be mentioned before giving the solution.
                        
                        The main problems are:
                        
                        1. Quality assessments in our dataset are objective. As a result, the machine learning models based on supervised learning can have biassed results.
                        
                        2. In the document, there are only three different quality classes. It could be not enough to represent the complexity of wine quality.
                        
                        3. The dataset is strongly unbalanced, having the majority of the records in classes 1 and 2.
                        
                        4. In this project, only a subset of machine learning methods were tested. It could be crucial to address the problem with different approaches.
                        
                        For both red and white wine datasets, the radial kernel SVM is the most appropriate result. Through this model, it is possible to reach an overall F1 score of more than 70%.
                        This percentage is remarkable. Moreover, it shows that the resulting model can be employed for predicting wine quality, under the supervision of an expert. In particular, the customer can have a dynamic answer to quality assessment in the Shiny App, where there is a dedicated section to test the model.
                        "), width = 12)
              )
      ),
      #################################About us ################################
      tabItem(tabName = "About_us",
              fluidRow(
                box(img(src = "https://media.licdn.com/dms/image/C4D03AQHdsQZ43lJd0Q/profile-displayphoto-shrink_800_800/0/1619939966754?e=1686182400&v=beta&t=iIVlj2x-Li5MUFOiDqbNHi5n7GztWTvZIQ2bkSub_38",
                        align = "center", style = paste0("width: 100%; height: ", "30em", ";")),
                    width = 4),
                box(img(src = "https://media.licdn.com/dms/image/D4E03AQFsU8HHxMxfwg/profile-displayphoto-shrink_800_800/0/1681222033077?e=1691625600&v=beta&t=I40itCjn9zzrWAvAqdcu2BypAms7DV908pocPgb6GNU",
                        align = "center", style = paste0("width: 100%; height: ", "30em", ";")),
                    width = 4),
                box(img(src = "https://media.licdn.com/dms/image/D4E03AQExcpYim60THQ/profile-displayphoto-shrink_800_800/0/1667482302683?e=1691625600&v=beta&t=gMHPMl7SzleCEqg7er1eI3SFxy7NeUoYnMo6jSaGj4Y",
                        align = "center", style = paste0("width: 100%; height: ", "30em", ";")),
                    width = 4),
              ),
              fluidRow(
                column(4, align = "justify",
                       "Daniele Buson"
                ),
                column(4, align = "justify",
                       "Morteza Kiani Haftlang"
                ),
                column(4, align = "justify",
                       "Goran Nikolov"
                )
              ),
              fluidRow(
                column(4, align = "justify",
                       "daniele.buson@stud.hslu.ch"
                ),
                column(4, align = "justify",
                       "morteza.kianihaftlang@stud.hslu.ch"
                ),
                column(4, align = "justify",
                       "Goran.nikolov@stud.hslu.ch"
                )
              )
      )
    
  )
))

################################################################################
##################################Server########################################
################################################################################

server <- function(input, output, session) {
  
  ######################################################################### Introduction Tab ###############################################
  
  ################################Data Summary Output of the Terminal in Intro part
  output$terminalOutputIntro1 <- renderPrint({
    
    capturedOutput <- reactive({
      capture.output({
        # R code here
        print("Red Wine")
        print(summary(red_wine))
      })
      
      
    })
    capturedOutput()
  })
  output$terminalOutputIntro2 <- renderPrint({
  capturedOutput <- reactive({
    capture.output({
      # R code here
      print("White Wine")
      print(summary(white_wine))
    })
    
    
  })
  capturedOutput()
  })
  ########################################################################## EDA Tab ######################################################
  ################################Correlation Plot 
  output$Correlation_Plot <- renderPlot({CorrelationPlot})
  
  ################################Acidity Substances Distribution Plot  
  output$wine_acidities_substances_distribution <- renderPlot({
    ggplot(data = wine, aes_string(x = input$select_Substance_acidity, fill = "Col")) +
      geom_bar(width = as.numeric(input$binWidth_acidity)) +
      scale_fill_manual(name  = "Type of wine: ",
                        values = c("#a2231d", "#e4d96f")) +
      theme(
        legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        legend.title = element_text(size = 10),
        plot.title = element_text(color = "black", size = 20)
      ) +
      facet_grid(~Col) +
      ggtitle( " Distribution") +
      ylab("Frequency") 
      #xlab(as.character(input$select_Substance))
  })
  #paste(as.character(input$select_Substance),
  ################################ Substances Boxplot Acidity
  output$wine_acidity_substances_boxplot <- renderPlot({
    
      BoxPlotS <- ggplot(data = wine, aes_string(x = "Col", y = input$select_Substance_acidity, fill = "Col")) +
        geom_boxplot() +
        scale_fill_manual(name  = "Type of wine: ",
                          values = c("#a2231d", "#e4d96f")) +
        theme(
          legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
          legend.title = element_text(size = 10),
          plot.title = element_text(color = "black", size = 20)
        ) +
        ggtitle(" Boxplot")+ 
        #ylab(as.character(input$select_Substance)) +
        xlab("")
      
      BoxPlotS
    
  })
  
  ################################sulphates Substances Distribution Plot  
  output$wine_sulphates_substances_distribution <- renderPlot({
    ggplot(data = wine, aes_string(x = input$select_Substance_sulphates, fill = "Col")) +
      geom_bar(width = as.numeric(input$binWidth_sulphates)) +
      scale_fill_manual(name  = "Type of wine: ",
                        values = c("#a2231d", "#e4d96f")) +
      theme(
        legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        legend.title = element_text(size = 10),
        plot.title = element_text(color = "black", size = 20)
      ) +
      facet_grid(~Col) +
      ggtitle( " Distribution") +
      ylab("Frequency") 
    #xlab(as.character(input$select_Substance))
  })

  ################################ Substances Boxplot Sulphates
  output$wine_sulphates_substances_boxplot <- renderPlot({
    
    BoxPlotS <- ggplot(data = wine, aes_string(x = "Col", y = input$select_Substance_sulphates, fill = "Col")) +
      geom_boxplot() +
      scale_fill_manual(name  = "Type of wine: ",
                        values = c("#a2231d", "#e4d96f")) +
      theme(
        legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        legend.title = element_text(size = 10),
        plot.title = element_text(color = "black", size = 20)
      ) +
      ggtitle(" Boxplot")+ 
      #ylab(as.character(input$select_Substance)) +
      xlab("")
    
    BoxPlotS
    
  })
  
  ################################other Substances Distribution Plot  
  output$wine_other_substances_distribution <- renderPlot({
    ggplot(data = wine, aes_string(x = input$select_Substance_other, fill = "Col")) +
      geom_bar(width = as.numeric(input$binWidth_other)) +
      scale_fill_manual(name  = "Type of wine: ",
                        values = c("#a2231d", "#e4d96f")) +
      theme(
        legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        legend.title = element_text(size = 10),
        plot.title = element_text(color = "black", size = 20)
      ) +
      facet_grid(~Col) +
      ggtitle( " Distribution") +
      ylab("Frequency") 
    #xlab(as.character(input$select_Substance))
  })
  
  ################################ Substances Boxplot other
  output$wine_other_substances_boxplot <- renderPlot({
    
    BoxPlotS <- ggplot(data = wine, aes_string(x = "Col", y = input$select_Substance_other, fill = "Col")) +
      geom_boxplot() +
      scale_fill_manual(name  = "Type of wine: ",
                        values = c("#a2231d", "#e4d96f")) +
      theme(
        legend.background = element_rect(fill = "white", color = "black", linetype = "solid"),
        legend.title = element_text(size = 10),
        plot.title = element_text(color = "black", size = 20)
      ) +
      ggtitle(" Boxplot")+ 
      #ylab(as.character(input$select_Substance)) +
      xlab("")
    
    BoxPlotS
    
  })
  ##################################residual sugar Distibution
  output$residual_sugar_distribution <- renderPlot({
    residual_sugar_plot
  })
    
  ##################################residual sugar boxplot
  output$residual_sugar_boxplot <- renderPlot({
    residual_sugar_boxplot
  })
  ##################################quality distribution
  output$quality_distribution <- renderPlot({
    quality_plot
  })
  ##################################quality boxplot
  output$quality_boxplot <- renderPlot({
    quality_boxplot
  })
  
  ############################################################# Modelling Tab ######################################################
  
  ################################Data prep for Modelling table for linear model ########
  data <- reactiveValues(df = data.frame(WineType = character(0),
                                         select_Substance_Y = character(0),
                                         select_Substance_X1 = character(0), 
                                         select_Substance_X2 = character(0), 
                                         stringsAsFactors = FALSE))
  
  # Create aliases for input values
  inputAliases <- reactive({
    list(WineType = input$WineType,
         select_Substance_Y = input$select_Substance_Y,
         select_Substance_X1 = input$select_Substance_X1,
         select_Substance_X2 = input$select_Substance_X2)
  })
  
  observeEvent(input$add_btn, {
    newRow <- c(inputAliases()$WineType, inputAliases()$select_Substance_Y, inputAliases()$select_Substance_X1, inputAliases()$select_Substance_X2)
    data$df <- rbind(data$df, newRow)
  })
  
  observeEvent(input$delete_btn, {
    if (nrow(data$df) > 0) {
      data$df <- data$df[-nrow(data$df), ]
    }
  })
  ################################Summary of the linear model for wine in terminal output
  ######################Linear Model##################
  output$terminalOutputModelling1 <- renderPrint({
    capturedOutput <- reactive({
      capture.output({
        # Assign reactive expressions to variables
        y_var <- input$select_Substance_Y
        x1_var <- input$select_Substance_X1
        x2_var <- input$select_Substance_X2
        
        # R code here
        if (input$WineType == "Red") {
          linear_model_3d_one <- lm(data = red, as.formula(paste(y_var, "~", x1_var, "+", x2_var)))
        } else {
          linear_model_3d_one <- lm(data = white, as.formula(paste(y_var, "~", x1_var, "+", x2_var)))
        }
        
        print(summary(linear_model_3d_one))
      })
    })
    capturedOutput()
    })

  # 3D Plot for linear model
  output$linearModelplot <- renderPlotly({
    # Use req() to ensure inputs are available before using them
    req(input$select_Substance_Y, input$select_Substance_X1, input$select_Substance_X2, input$WineType)
    
    # Assign reactive expressions to variables
    y_var <- input$select_Substance_Y
    x1_var <- input$select_Substance_X1
    x2_var <- input$select_Substance_X2
    
    if (input$WineType == "Red") {
      plotdata <- red_wine
    } else {
      plotdata <- white_wine
    }
    
    linear_plot_3d_one_rw <- plot_ly(data = plotdata) %>%
      add_trace(type = "scatter3d", mode = "markers",
                x = ~get(x2_var), y = ~get(x1_var), z = ~get(y_var),
                color = ~quality,
                colors = c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                marker = list(size = 2)) %>%
      layout(scene = list(xaxis = list(title = x2_var),
                          yaxis = list(title = x1_var),
                          zaxis = list(title = y_var)))
    
    linear_plot_3d_one_rw
  })
  ##################################Modelling Table
  output$mytable <- renderDataTable({
    data$df
  }, options = list(pageLength = 8, lengthMenu = c(8, 16, 32, 64)))
  

  ##################################3D Plot for Modelling
  output$myplot <- renderPlot({
    if (nrow(data$df) > 0) {} 
    else {}
  })
  ################################Data prep for Modelling table for Non-linearlinear model ########
  data_NL <- reactiveValues(df = data.frame(NL_WineType = character(0),
                                            NL_model= character(0),
                                            NL_Y = character(0),
                                            NL_X1 = character(0), 
                                            NL_X2 = character(0), 
                                            stringsAsFactors = FALSE))
  
  # Create aliases for input values
  inputAliases_NL <- reactive({
    list(NL_WineType = input$WineTypeNonLinear,
         NL_model=input$NonLinearModelSelection,
         NL_Y = input$NL_select_Substance_Y,
         NL_X1 = input$NL_select_Substance_X1,
         NL_X2 = input$NL_select_Substance_X2)
  })
  
  observeEvent(input$add_btn_NL, {
    newRow_NL <- c(inputAliases_NL()$NL_WineType,
                   inputAliases_NL()$NL_model,
                   inputAliases_NL()$NL_Y,
                   inputAliases_NL()$NL_X1,
                   inputAliases_NL()$NL_X2)
    
    data_NL$df <- rbind(data_NL$df, newRow_NL)
  })
  
  observeEvent(input$delete_btn_NL, {
    if (nrow(data_NL$df) > 0) {
      data_NL$df <- data_NL$df[-nrow(data_NL$df), ]
    }
  })
  ################################Summary of the Non-linear model for wine in terminal output
  ###################### Non-Linear Model##################
  
  
  output$terminalOutputModelling2 <- renderPrint({
    
      
  
    # Assign reactive expressions to variables
    
    modelselector <- input$NonLinearModelSelection
    NL_y_var <- input$NL_select_Substance_Y
    NL_x1_var <- input$NL_select_Substance_X1
    NL_x2_var <- input$NL_select_Substance_X2
    #print(NL_y_var)
    #print(NL_x1_var)
    #print(NL_x2_var)
    # R code here
    if (input$WineTypeNonLinear == "Red") {
      if(modelselector=='1')    {quadratic_model<- lm( red[,NL_y_var]~ red[,NL_x1_var] + poly(red[,NL_x2_var],2) )}
      
      else if(modelselector=='2'){quadratic_model<- lm( red[,NL_y_var]~ red[,NL_x1_var] + I(red[,NL_x2_var]**2) )}
      else if(modelselector=='3'){quadratic_model<- lm( red[,NL_y_var]~ poly(red[,NL_x1_var],2) + poly(red[,NL_x2_var],2) )}
      else if(modelselector=='4'){quadratic_model<- lm( red[,NL_y_var]~ poly(red[,NL_x1_var],2) + red[,NL_x2_var] )}
      else if(modelselector=='5') {quadratic_model<- lm( red[,NL_y_var]~ I(red[,NL_x1_var]**2) + red[,NL_x2_var] )}
      else {quadratic_model <- gam(red[,NL_y_var]~ s(red[,NL_x1_var]) + s(red[,NL_x2_var]) )}
    } 
    else {
      if(modelselector=='1')    {quadratic_model<- lm( white[,NL_y_var]~ white[,NL_x1_var] + poly(white[,NL_x2_var],2) )}
      else if(modelselector=='2'){quadratic_model<- lm( white[,NL_y_var]~ white[,NL_x1_var] + I(white[,NL_x2_var]**2) )}
      else if(modelselector=='3'){quadratic_model<- lm( white[,NL_y_var]~ poly(white[,NL_x1_var],2) + poly(white[,NL_x2_var],2) )}
      else if(modelselector=='4'){quadratic_model<- lm( white[,NL_y_var]~ poly(white[,NL_x1_var],2) + white[,NL_x2_var] )}
      else if(modelselector=='5') {quadratic_model<- lm( white[,NL_y_var]~ I(white[,NL_x1_var]**2) + white[,NL_x2_var] )}
      else {quadratic_model <- gam(white[,NL_y_var]~ s(white[,NL_x1_var]) + s(white[,NL_x2_var]) )}
    }
    
    print(summary(quadratic_model))
    
  })
   
  # 3D Plot for Non linear model
  output$NonlinearModelplot <- renderPlotly({
    # Use req() to ensure inputs are available before using them
    req(input$NL_select_Substance_Y, input$NL_select_Substance_X1, input$NL_select_Substance_X2, input$WineTypeNonLinear, input$NonLinearModelSelection)
    
    # Assign reactive expressions to variables
    NL_y_var_plot <- input$NL_select_Substance_Y
    NL_x1_var_plot <- input$NL_select_Substance_X1
    NL_x2_var_plot <- input$NL_select_Substance_X2
    
    if (input$WineTypeNonLinear == "Red") {
      plotdata <- red_wine
    } else {
      plotdata <- white_wine
    }
    
    quadratic_plot_3d<- plot_ly(data = plotdata, z= ~plotdata[,NL_y_var_plot], y= ~plotdata[,NL_x1_var_plot]*2, x= ~plotdata[,NL_x2_var_plot]*2, 
                                color= ~quality, 
                                colors=c('#ffffb2','#fed976','#feb24c','#fd8d3c','#f03b20','#bd0026'),
                                size = 0.2)
    quadratic_plot_3d <- quadratic_plot_3d  %>% add_markers() %>% layout(scene = list(xaxis = list(title = 'X2'),
                                                                                      
                                                                                      yaxis = list(title = 'X1'),
                                                                                      
                                                                                      zaxis = list(title = 'Y'))) 
    
    quadratic_plot_3d
  })
  ##################################Modelling Table
  output$mytable_NonLinear <- renderDataTable({
    data_NL$df
  }, options = list(pageLength = 8, lengthMenu = c(8, 16, 32, 64)))
  
  
  # ################################## Terminal output for SVM ##########
  
   
  output$terminalOutputModelling3 <- renderPrint({
    # Assign reactive expressions to variables
    
    SVM_Wine_type <- input$WineTypeSVM
    SVM_cost <- input$Cost
    SVM_Kernel <- input$Kernel
    
    if (SVM_Wine_type=='Red'){SVMWine <- red_wine }
    else  {SVMWine <- white_wine}
    
    SVMWine[, "quality"] <- as.factor(SVMWine[, "quality"])
    
    set.seed(123)
    indices <- createDataPartition(SVMWine$quality, p = .75, list = F)
    train_svm <- SVMWine %>% slice(indices)
    
    test_in_svm <- SVMWine %>% slice(-indices) %>% select(-quality)
    test_truth_svm <- SVMWine %>% slice(-indices) %>% pull(quality)
    
    red_wine_svm_linear_rw <- svm(quality~ ., train_svm, kernel = SVM_Kernel, 
                                  scale = TRUE, cost = as.numeric(SVM_cost))
    
    test_pred_linear_rw <- predict(red_wine_svm_linear_rw, test_in_svm)
    table(test_pred_linear_rw)
    
    confusion_matrix_linear_rw <- confusionMatrix(test_pred_linear_rw, test_truth_svm, mode = "everything")
    confusion_matrix_linear_rw
    print("Model summary")
    print(summary(red_wine_svm_linear_rw))
    print("Confusion Matrix")
    print(confusion_matrix_linear_rw)
  })
  # ################################## Terminal output for ANN ##########
  
  
  output$terminalOutputModelling4 <- renderPrint({
    # Assign reactive expressions to variables
    
    ANN_Wine_type <- input$WineTypeANN
    
    if (ANN_Wine_type=='Red'){PredictData <- red_wine }
    else  {PredictData <- white_wine}
    
    PredictData[, "quality"] <- as.factor(PredictData[, "quality"])
    
    set.seed(123)
    indices <- createDataPartition(PredictData$quality, p = .75, list = F)
    train_svm <- PredictData %>% slice(indices)
    
    test_in_svm <- PredictData %>% slice(-indices) %>% select(-quality)
    test_truth_svm <- PredictData %>% slice(-indices) %>% pull(quality)
    
    
    neural_net_model_rw <- neuralnet(quality~ ., train_svm, hidden = c(3,4), threshold = 0.5)
    
    prediction_neural_net_model_rw <- compute(neural_net_model_rw, test_in_svm)
    prediction_neural_net_model_rw <- apply(prediction_neural_net_model_rw$net.result, 1, which.max)
    prediction_neural_net_model_rw <- factor(levels(test_truth_svm)[prediction_neural_net_model_rw], levels = levels(test_truth_svm))
    confusion_matrix_nn_rw <- confusionMatrix(prediction_neural_net_model_rw, test_truth_svm,mode = "everything")
    confusion_matrix_nn_rw
    
    print("Model summary")
    print(summary(prediction_neural_net_model_rw))
    print("Confusion Matrix")
    print(confusion_matrix_nn_rw)
  })
  ################################Prediction section###################################################################################
  output$PredictionOutput <- renderPrint({
    
    
    Kernel_selector <- input$Kernel_Selector 
    Cost_selector <- input$Cost_Selector
    Model_selector <- input$Model_Selector
    wine_selector_models <- input$Wine_Selector
    
    new_wine <- red_wine[1,1:11]
  
    new_wine[1,1] = input$Fixed_Acidity
    new_wine[1,2] = input$Volatile_Acidity
    new_wine[1,3] = input$Citric_Acid
    new_wine[1,4] = input$Residual_Sugar
    new_wine[1,5] = input$Chlorides
    new_wine[1,6] = input$Free_Sulfur_Dioxide
    new_wine[1,7] = input$Total_Sulfur_Dioxide
    new_wine[1,8] = input$density
    new_wine[1,9] = input$pH
    new_wine[1,10] = input$Sulphates
    new_wine[1,11] = input$alcohol
  
    
    print(new_wine)
    print('####################################')
    
    if (wine_selector_models=="White"){PredictData <- train_ww}
    else {PredictData <- train_rw}
    
    
    #####Glm####
    if(Model_selector=="Multinomial"){prediction_model <- multinom(quality~ ., data = PredictData)
      result <- predict(prediction_model, newdata = new_wine)  
    }
    #####SVM####
    else if(Model_selector=='SVM') {
      
      SVM_cost <- Cost_selector
      SVM_Kernel <- Kernel_selector
      
      SVMWine <- PredictData
      
      SVMWine$quality <- as.factor(SVMWine$quality)
      
      set.seed(123)
      indices <- createDataPartition(SVMWine$quality, p = .75, list = F)
      train_svm <- SVMWine[indices,]
      
      if(SVM_Kernel=='radial')
        {prediction_model <- svm(quality~ ., data= train_svm, kernel = SVM_Kernel, scale = TRUE, gamma = 0.5, cost = as.numeric(SVM_cost))}
      else
        {prediction_model <- svm(quality~ ., data= train_svm, kernel = SVM_Kernel, scale = TRUE, cost = as.numeric(SVM_cost))}
      
      result <- predict(prediction_model, newdata = new_wine)
      
    }
    #####ANN####
    else{
      # Assign reactive expressions to variables
      if(wine_selector_models=='Red'){
        red_wine[, "quality"] <- as.factor(red_wine[, "quality"])
        set.seed(123)
        indices <- createDataPartition(red_wine$quality, p = .75, list = F)
        train_rw <- red_wine %>% slice(indices)
      
        test_in_rw <- red_wine %>% slice(-indices) %>% select(-quality)
        test_truth_rw <- red_wine %>% slice(-indices) %>% pull(quality)}
      
      else{white_wine[, "quality"] <- as.factor(white_wine[, "quality"])
      set.seed(123)
      indices <- createDataPartition(white_wine$quality, p = .75, list = F)
      train_rw <- white_wine %>% slice(indices)
      
      test_in_rw <- white_wine %>% slice(-indices) %>% select(-quality)
      test_truth_rw <- white_wine %>% slice(-indices) %>% pull(quality)}

      
      PredictData$quality <- as.factor(PredictData$quality)
      
      set.seed(123)
      
      neural_net_model_rw <- neuralnet(quality~ ., data =PredictData, hidden = c(3,4), threshold = 0.5)
      
      prediction_neural_net_model_rw <- compute(neural_net_model_rw, new_wine)
      prediction_neural_net_model_rw <- apply(prediction_neural_net_model_rw$net.result, 1, which.max)
      prediction_neural_net_model_rw <- factor(levels(test_truth_rw)[prediction_neural_net_model_rw], levels = levels(test_truth_rw))

      result <- as.character(prediction_neural_net_model_rw)
      
      #result <- predict(neural_net_model_rw, newdata = new_wine)
    }
    
    if(result=='1') {print('Lower Quality')}
    else if(result=='2') {print('Good Quality')}
    else{print('Higher Quality')}
    print(result)
    
    
    
  })
  
  
}


shinyApp(ui, server)
  