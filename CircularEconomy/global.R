## global.R ##
library(shinydashboard)
library(shinythemes)
library(plotly)
library(DT)
library(shiny)
library(googleVis)
library(ggplot2)
library(dplyr)
library(forcats)
library(bbplot)
library(chorddiag)
library(hrbrthemes)
options(scipen=999)
# read csv
plastic <- read.csv("./plastic.csv", stringsAsFactors = FALSE)
plastic$year <- NULL
plastic$X <- NULL
plastic <- plastic[-c(232),]
plastic <- plastic %>% 
  rename(
         Mismanaged="Mismanaged.Plastic.Waste.Total",
         Coastal.Ppl = "Coastal.Population",
         Ppl = "Population",
         Inade.S = "Inadequately.Managed.Waste.Share",
         Mismgt.S = "Mismanaged.Plastic.Waste.Share",  
         Mismgt.PC = "Mismanaged.Plastic.Waste.Per.Capita.",
         GDP.PC = "GDP.Per.Capita",
         PWaste.G = "Plastic.Waste.Generation.Total",
         PWaste.PC = "Plastic.Waste.Per.Capita",
         Avg3Y.GDPG="Avg.3Y.GDP.Growth"
  )

Dict = matrix(
  c('PWaste.G','Plastic Waste Generation','PWaste.PC','Plastic Waste Per Capita','Mismanaged','Mismanaged Plastic Generation','Mismgt.PC','Mismanaged Plastic Per Capita','Mismgt.S','Mismanaged Plastic Share','Inade.S','Inadequately Managed Plastic Waste Share(Highest Risk)','Avg3Y.GDPG','2008-2010 Avg GDP Per Capita Growth Rate','Ppl','Population', 'Coastal.Ppl','Coastal Population','GDP.PC','GDP Per Capita'),
  10,2,byrow = T)

choice <- colnames(plastic)[c(-1,-2,-3,-4,-10,-16)]
choice <- choice[c(4,5,6,7,8,9,10,1,2,3)]
plastic[is.na(plastic)] <- 0
#intro pic
gpp <- read.csv("./global-plastics-production.csv", stringsAsFactors = FALSE)
gpp <- rename(gpp, Tons = "Global.plastics.production..million.tonnes...tonnes.")
gpp
##text
text_df <- data.frame('Ppl' = 'Population of Countries',
                      'Coastal.Ppl'='Coastal Population of Countries', 
                      'GDP.PC'='Per Capita GDP of Countries',
                      'PWaste.G'='It seems that coastal countries with a large population have higher Plastic Waste Generation',
                      'PWaste.PC'='5 out of the top 8 countries that have the highest per capita plastic waste are small island contries.The other 3 top countries are high-income countries. Based on these observertions, geographical feature(inland or coastal) and economic level(GDP Per Capita) might be the factors that impact plastic pollution the most. These should be further discovered.',
                      'Mismanaged'='The top 8 countries with the highest mismanaged plastic waste are all developing countries.',
                      'Mismgt.PC'='Surprisingly, developed countries have noticeablylow mismanaged plastic waste per person and share.',
                      'Mismgt.S'='Surprisingly, developed countries have noticeably low mismanaged plastic waste per person and share.',
                      'Inade.S'='It seems that developing countries have significantly higher share of inadequately managed plastic waste.',
                      'Avg3Y.GDPG'='Developing countries have higher growing speed of GDP per capita',
                      stringsAsFactors = F)
