shinyUI(
  dashboardPage(
    #header + skin + sidebar panel#### 
    skin = 'blue',
    dashboardHeader(
      title = "Circular Economy"
    ),
    #sidebar panel 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction", tabName = "intro", icon = icon("dove")),
        menuItem("Data", tabName = "data", icon = icon("table")),
        menuItem("Visualization", tabName = "explore", icon = icon("map")),
        menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
        menuItem("Conclusion", tabName = "conc", icon = icon("box-open"))
        )
    ),
    
    #Introduction####
    dashboardBody(
      tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
      tabItems(
        #Intro
        tabItem(
          tabName = 'intro',
          fluidPage(
            theme = shinytheme("united"),
            tags$head(tags$style(HTML("
                                h2 {
                                  text-align: center;
                                }
                                h3 {
                                  text-align: center;
                                  margin-bottom: 15px;
                                  margin-top: 15px;
                                }
                                tags {
                                  font-size: 15px;
                                  margin-bottom: 15px;
                                  margin-top: 15px;
                                }
                                div.box-h2 {
                                  text-align: center;
                                }
                                div.box-h3 {
                                  text-align: center;
                                  margin-bottom: 15px;
                                  margin-top: 15px;
                                }
                                div.box-tags {
                                  font-size: 15px;
                                  margin-bottom: 15px;
                                  margin-top: 15px;
                                }
                                "))),
            box(
              h2('Data Analysis: Plastic Production and Pollution'),
              h3('Plastic pollution has exploded globally over the last 70 years'),
              tags$p("In 1950, the world produced 2 million tons of plastic per year. In 2015 it produced 381 million tons, a 381x increase. 
                     The sheer waste from this production is breathtaking - an estimated 8.3 billion tons of plastic have been produced since 1950, and only 9%
                     of that volume has been recycled. It is estimated that 60% of that total volume has ended up in the landfill or natural environment. 
                     An estimated 8.3 billion tons of plastic have been produced since the 1950s — that’s equivalent to the weight of more than 800,000 Eiffel Towers."),
              div(plotlyOutput("lineplot", height = "600px"), align = "center"),
              width = "100%"
              ),
            box(
              h3('Plastic Pollution Facts'),
              tags$p("- 2 million plastic bags are used every minute worldwide."),
              tags$p("- The average American throws away approximately 185 pounds of plastic per year."),
              tags$p("- Plastic accounts for around 10 percent of the total waste we generate."),
              tags$p("- The average time that a plastic bag is used for is 12 minutes, while it takes around 500 years to bio-degrade in the ocean."),
              tags$p("- Plastic is killing more than 1.1 million seabirds and animals every year and suffering astronomical number of animals."),
              tags$p("- The pollution eventually return to us - The average person eats 70,000 microplastics each year."),
              tags$p("- Enough plastic is thrown away each year to circle the earth four times each year."),
              div(img(src="https://ourworldindata.org/exports/decomposition-rates-marine-debris_v2_850x600.svg"
                  ,width="50%"), align = "center"),
              h3('Video: Plastic Ocean'),
              div(tags$iframe(src="https://www.youtube.com/embed/ju_2NuK5O-E",
                          width="653",height="367"), align = "center"),
              h3('Plastic Waste has a Log-Linear Relationship with GDP'),
              div(tags$iframe(src="https://ourworldindata.org/grapher/per-capita-plastic-waste-vs-gdp-per-capita?time=latest", width = "100%", height = "600px"), 
                  align = "center"),
              div(tags$b('**Proceed to next page with the sidebar.**'), align = "center"),
              width = 12
              )
            )
          ),
        #Data####
        tabItem(tabName = "data",
                fluidPage(
                  box(
                    h3('Dataset Variables:'),
                    DT::dataTableOutput("variablesTable"),
                    tags$p('Colors in table represent magnitude: Darker (purple) is higher and lighter (yellow) is lower.'),
                    h4('Notes:'),
                    tags$p("Mismanaged waste: Material that is either littered or inadequately disposed (the sum of littered and inadequately disposed waste), 
                     which could eventually enter the ocean via inland waterways, wastewater outflows, and transport by wind or tides and has much higher 
                     risk of entering the ocean and contaminating the environment."),
                    tags$p("Inadequately managed waste: Waste is not formally managed and includes disposal in dumps or open, uncontrolled landfills, 
                     where it is not fully contained. Inadequately managed waste has high risk of polluting rivers and oceans. This does not include 'littered' 
                     plastic waste, which is approximately 2% of total waste."),
                    width = "100%"
                  )
                )),
        #Explore####
        tabItem(tabName = "explore",
                fluidPage(
                  box(
                    h2('Explore Global Plastic Pollution'),
                fluidRow(column(4, 
                                selectizeInput(inputId = "selected", 
                                               label = "Select a variable to explore",
                                               choices = Dict[,2], 
                                               selected = TRUE))),
                fluidRow(column(h3("Observation"),
                                tags$p(textOutput("text1")),
                                    width = 12)), 
                fluidRow(column(7,
                         box(htmlOutput("map"),
                             height = 500,width = 12)),
                column(5,
                         box(title = "Top 10 Countries",
                             plotlyOutput("barChart1"),
                             height = 500, width = 12)
                          
                        ))
                ,width = 12)
                    )
                  ),
        #Analysis####
        tabItem(tabName = "analysis",
                tabsetPanel(type = 'tabs',
                            tabPanel('Analysis about Plastic Waste',
                                     fluidRow(box(
                                              h2("Economic development status and growth have significant impact on plastic waste, 
                                                 and generally a positive association"),
                                              plotlyOutput("bubbleGDPWastePC"),
                                              tags$p("Note: x-axis takes log10 as base."),
                                              h3("The plastic waste per person in developed countries are significantly greater than developing countries"),
                                              div(plotlyOutput("boxplot4"), align = "center"),
                                              width=12))),
                            tabPanel('Analysis about Mismanaged Plastic Waste',
                                     fluidRow(box(
                                              h2("Mismanaged plastic waste tends to be higher in industrialized middle-income and fast-growing developing countries"),
                                              tags$p("Waste management infrastructure has failed to keep pace with rapid industrial and manufacturing growth."),
                                              tags$p("Therefore, development of effective waste management infrastructure in middle-income and 
                                              growing lower-income countries is 
                                                     essential in tackling the issue of plastic pollution."),
                                              plotlyOutput("bubbleGDPWastePC2"),
                                              tags$p("X-axis is log10 base."),
                                              div(plotlyOutput("boxplot3"), align = "center"),
                                              h2("Fast-growing countries seem to have less mismanaged plastic waste on average"),
                                              tags$p("There are several possibilities, might be due to most of them being either landlocked 
                                                     oil-rich countries or low-income countries whose demand have not surged yet"),
                                              div(plotlyOutput("boxplot2"), align = "center"),
                                              tags$p("Note: Per Capita GDP is from 2008-2010. When it is greater than 3%: Inflation, greater than 0% and lower than 3%: Normal, and less than 0%: Recession"),
                                              width=12))),
                            tabPanel('Analysis about Coastal Population and Geographical Feature',
                                     fluidRow(box(
                                              h2("Costal population has a positive correlation with mismanaged plastic waste across counties."),
                                              plotlyOutput("bubbleGDPWastePC3"),
                                              tags$p("Note: Coastal population is measured as the population within 50 kilometres of a coastline"),
                                              h2("Coastal countries have much higher mismanaged plastic waste per person than landlocked countries"),
                                              div(plotlyOutput("boxplot1"), align = "center"),
                                              width=12)
                                              )
                                     )
                            )
                ),
        #Conclusion####
        tabItem(tabName = "conc",
                fluidRow(
                  box(
                  h2("Main Takeaways for the Circular Economy"),
                  tags$p("Economic development status and growth have significant impact on plastic waste."),
                  tags$p(" "),
                  tags$p("There are two levels to the circular economy here, one involves intra-country waste management where waste is simply being dumped into the ocean instead of re-used in any meaningful way. The other is inter-country, there is a lot of waste going between countries, developed economies tend to dump off onto developing countries leading to a very linear relationship in waste that oftentimes ends in the ocean. Mismanaged plastic waste is also higher in industrialized middle-income and quickly-growing developed countries because their waste management infrastructure development has not kept pace with rapid industrial and manufacturing growth. While importing massive quantities of plastic from developed countries, or producing them in the case of industrializing countries, they do not balance the other side of the equation with waste management."),
                  tags$p(" "),
                  tags$p("Coastal populations tend to have more mismanaged plastic waste than other countries 
                         than landlocked countries, this is primarily because it is easier to dump waste into the ocean,
                         an important question moving forward is whether that waste is primarily sourced from
                         within the country itself or if it is from landlocked countries, and whether countries
                         are simply finding other, less easily tracked ways of dumping when oceans are not available
                         receptacles."),
                  width = 12),
                  box(
                    # div(img(src="CircEcon.png", width="50%"), align = "centered"),
                    # div(img(src="plastic-fate-605x550.png", width="50%"), align = "centered"),
                    HTML('<left><img src="CircEcon.png" width="550"></left>'),
                    HTML('<right><img src="plastic-fate-605x550.png" width="550"></right>'),
                    width = 12
                  ),
                  box(
                    div(tags$p("Update Coming Soon."), align = "center"),
                    br(),
                    br(),
                    radioButtons('select_market',"Select Market",inline = TRUE,
                                 choices = c("East","West","South","North"),
                                 selected = 'East'),
                    
                    chorddiagOutput("distPlot", height = 600),
                    width = 12
                    )
                  
                )
              )
        )
    )
  )
)
