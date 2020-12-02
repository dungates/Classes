shinyUI(
  dashboardPage(
    #header + skin + sidebar panel#### 
    skin = 'green',
    dashboardHeader(
      title = "Plastic Pollution"
    ),
    #sidebar panel 
    dashboardSidebar(
      sidebarMenu(
        menuItem("Introduction", tabName = "intro", icon = icon("dove")),
        menuItem("Visualization", tabName = "explore", icon = icon("map")),
        menuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
        menuItem("Conclusion", tabName = "conc", icon = icon("box-open"))
        )
    ),
    
    #Introduction####
    dashboardBody(
      tabItems(
        #Intro
        tabItem(
          tabName = 'intro',
          fluidPage(
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
              background = 'light-blue',
              h2('Exploratory Data Analysis: Plastic Production and Pollution'),
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
              tags$p("- Enough plastic is thrown away each year to circle the earth four times."),
              div(img(src="https://ourworldindata.org/exports/decomposition-rates-marine-debris_v2_850x600.svg"
                  ,width="50%"), align = "center"),
              h3('Video: Plastic Ocean'),
              div(tags$iframe(src="https://www.youtube.com/embed/ju_2NuK5O-E",
                          width="653",height="367"), align = "center"),
              h3('Plastic Waste has a Log-Linear Relationship with GDP'),
              div(tags$iframe(src="https://ourworldindata.org/grapher/per-capita-plastic-waste-vs-gdp-per-capita?time=latest", width = "100%", height = "600px"), 
                  align = "center"),
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
              tags$p('**Proceed to next page with the sidebar.**'),
              width = 12
              )
            )
          ),
        #Explore####
        tabItem(tabName = "explore",
                fluidPage(
                  box(
                    background = 'light-blue',
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
                                              background = 'light-blue',
                                              h2("Plastic waste tends to increase as people and countries get richer"),
                                              plotlyOutput("bubbleGDPWastePC"),
                                              tags$p("Note: x-axis takes log10 as base."),
                                              tags$p("Note**: 0 represents missing value"),
                                              h3("The plastic waste per person in developed countries are significantly greater than developing countries"),
                                              div(plotlyOutput("boxplot4"), align = "center"),
                                              tags$p("Note: 0 represents missing value"),
                                              width=12))),
                            tabPanel('Analysis about Mismanaged Plastic Waste',
                                     fluidRow(box(
                                              background = 'light-blue',
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
                                              background = 'light-blue',
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
                  background = 'light-blue',
                  h2("Main Takeaways for the Circular Economy"),
                  tags$p("- Plastic waste tends to increase as people and countries get richer"),
                  tags$p(" "),
                  tags$p("- Mismanaged plastic waste tends to be higher in industrialized middle-income and fast-growing developing countries. Because their waste management infrastructure has failed to keep pace with their rapid industrial and manufacturing growth. And they import massive quantities of plastic trashes from developed countries which is also the reason why developed countries have such a small amount of mismanaged plastic waste while having a great amount of plastic waste. Therefore, development of effective waste management infrastructure in middle-income and growing lower-income countries is crucial to tackling the issue of plastic pollution. And developing countries should reject receiving trashes from developed countries to prompt them to cut down waste and improve recycling."),
                  tags$p(" "),
                  tags$p("- Costal population has a positive correlation with mismanaged plastic waste across counties. And coastal countries have much higher mismanaged plastic waste per person than landlocked countries. Because waste generated in coastal region has higher risk of entering the ocean and producing severe environmental damage."),
                  width = 12),
                  box(
                    br(),
                    br(),
                    radioButtons('select_market',"Select Market",inline = TRUE,
                                 choices = c("East","West","South","North"),
                                 selected = 'East'),
                    
                    div(chorddiagOutput("distPlot", height = 600), align = "center") # Make this shit center
                  )
                  
                )
              )
        )
    )
  )
)
