library(shiny)
require(shinyBS)
require(shinydashboard)
require(shinyjs)
require(caret)
require(plyr)
require(dplyr)
require(tidyr)
#require(Cairo)
require(raster)
require(gstat)
require(wesanderson)
require(nnet)
require(randomForest)
require(data.table)
require(kernlab)

# car, foreach, methods, plyr, nlme, reshape2, stats, stats4, utils, grDevices

# Get data
proper=function(s) sub("(.)", ("\\U\\1"), tolower(s), pe=TRUE)
prisons <- fread("~/projects/Crime_in_prisons/01 Data/20170209_Prisons_lookup_table_v2.csv") %>% mutate(PrisonName = `Prison Name`)
data <- fread("~/projects/Crime_in_prisons/01 Data/From police.uk/01-Apr2017/CrimeDat201204-201704_v2.csv") %>% dplyr::select(1:10)
data <- data %>% mutate(month = as.Date(paste(month,"-01",sep=""))) %>% mutate(Offence = proper(gsub("-", " ", Offence)))
prisonpop <- fread("~/projects/Crime_in_prisons/01 Data/From prisons team/Prison_population_full.csv")
regressors <- fread("~/projects/Crime_in_prisons/01 Data/Regressors/Regressors.csv")%>% mutate(month = as.Date(month))

# Join prison population data on
prisonpop <- prisonpop %>% mutate(month=as.Date(month))
data <- data %>% left_join(prisonpop, by=c("PrisonName", "month"))

# Filter data to date period with population
data <- data %>% filter(month >= '2012-06-01')

# Get monthly total population
prisonpop <- prisonpop %>%
  group_by(month) %>%
  dplyr::summarise(population = sum(population, na.rm = TRUE))

# Create time series data
data.ts <- data %>%
  group_by(month) %>%
  dplyr::summarise(numcrimes= n()) %>%
  left_join(prisonpop, by = "month")

# Get data for those not under investigation
pctinvestigating <- data %>%
  filter(OutcomeCategory != "Under investigation") %>%
  group_by(month) %>%
  dplyr::summarise(numcompleted=n())

# Get percent charged
data.ts2 <- data %>%
  filter(OutcomeCategory != "Under investigation") %>%
  mutate(outcome = case_when(OutcomeCategory %in% c("Unable to prosecute suspect",
                                                    "Further investigation is not in the public interest",
                                                    "Formal action is not in the public interest",
                                                    "Investigation complete; no suspect identified",
                                                    "Local resolution") ~ "No charge",
                             OutcomeCategory=="Status update unavailable" ~ "Unknown",
                             TRUE ~ "Charged")) %>%
  group_by(month, outcome) %>%
  dplyr::summarise(num = n()) %>%
  inner_join(data.ts, by = "month") %>%
  inner_join(pctinvestigating, by= "month") %>%
  filter(outcome == "Charged") %>% 
  mutate(numcharged = num,
         pctcharged = num/numcompleted,
         pctcompleted = numcompleted/numcrimes) %>%
  filter(pctcompleted > 0.6) %>%
  dplyr::select(month, population, numcrimes, pctcharged)

# Add on other regressors and finish processing
data.ts3 <- data.ts2 %>%
  inner_join(regressors, by = "month") %>%
  mutate(crimesprop = 1000 * numcrimes/population) %>%
  dplyr::select(month, population, crimesprop, pctcharged, staff, propviolence)

# fwrite(data.ts3, 'data.csv')

dmnds <- diamonds#[sample(1:nrow(diamonds),1e3),]

# leaf <- read.csv('/Users/davesteps/Desktop/kaggle_data/leaf/train.csv')

datasets <- list(
  'iris'=iris,
  'cars'=mtcars,
  'diamonds'=data.frame(dmnds),
  'prisons'=as.data.frame(data.ts3)
  # 'leaf'=leaf
  # 'midwest'=data.frame(midwest),
  # 'mpg'=data.frame(mpg),
  # 'msleep'=data.frame(msleep),
  # 'txhousing'=data.frame(txhousing)
)

tuneParams <- list(
  'svmLinear'=data.frame(C=c(0.01,0.1,1)),
  'svmPoly'= expand.grid(degree=1:3,scale=c(0.01,0.1),C=c(0.25,0.5,1)),
  'nnet'=expand.grid(size=c(1,3,5),decay=c(0.01,0.1,1)),
  'rf'=data.frame(mtry=c(2,3,4)),
  'knn'=data.frame(k=c(1,3,5,7,9)),
  'nb'=expand.grid(usekernel=c(T,F),adjust=c(0.01,0.1,1),fL=c(0.01,0.1,1)),
  'glm'=NULL#data.frame()
)


mdls <- list('svmLinear'='svmLinear',
             'svmPoly'='svmPoly',
             'Neural Network'='nnet',
             'randomForest'='rf',
             'k-NN'='knn',
             'Naive Bayes'='nb',
             'GLM'='glm',
             'GAM'='gam')
#multinom

mdli <- list(
  'Regression'=c(T,T,T,T,T,F,T,F),
  'Classification'=c(T,T,T,T,T,T,F,F)
)  

reg.mdls <- mdls[mdli[['Regression']]]
cls.mdls <- mdls[mdli[['Classification']]]


# 
pal <- c('#b2df8a','#33a02c','#ff7f00','#cab2d6','#b15928',
         '#fdbf6f','#a6cee3','#fb9a99','#1f78b4','#e31a1c')
set.seed(3)
pal <- sample(pal,length(mdls),F)
names(pal) <- mdls

modelCSS <-   function(item,col){
  tags$style(HTML(paste0(".selectize-input [data-value=\"",item,"\"] {background: ",col," !important}")))
}


tableCSS <- function(model,col){
  paste0('if (data[6] == "',model,'")
         $("td", row).css("background", "',col,'");')
}  

label.help <- function(label,id){
  HTML(paste0(label,actionLink(id,label=NULL,icon=icon('question-circle'))))
}



