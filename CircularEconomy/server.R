server <-function(input, output){
  
  output$lineplot <- renderPlotly({
    global_plastic_production <- gpp %>% 
      mutate(Tons=round(Tons/1000000,0)) %>% 
      ggplot(aes(x=Year,y=Tons)) +
      geom_line(colour = "#007f7f", size = 1) +
      geom_point(aes(color=Tons)) +
      bbplot::bbc_style() +
      ggtitle('Global Plastic Production') +
      scale_y_continuous(labels = function(y) paste(y, "Million Tons")) +
      theme(legend.position = "none",
            axis.text.y = element_text()) +
    labs(subtitle = "1950-2015 in Millions of Tons")
    ggplotly(global_plastic_production) %>% 
      layout(width="100%")
  })
  
  VDict = reactive({
    f = which(Dict==input$selected, arr.ind = T)
    return(Dict[f[1],1])
  })

  output$map <- renderGvis({
    gvisGeoChart(plastic, "Country", VDict(), #state.name tells this column contains the state name
                 options=list(region="world",
                              displayMode="regions",
                              gvis.editor="Other Charts",
                              projection="kavrayskiy-vii",
                              # width="600px", height="300px",
                              width="100%",height="120%",
                              # height = "[{keepAspectRatio:'TRUE'}]",
                              # width = "[{keepAspectRatio:'TRUE'}]",
                              colorAxis="{colors:['white', '#FF6666']}"))
  })
  
  output$bubbleGDPWastePC <- renderPlotly({
    
    bubble_GDP_WastePC <- plastic %>% filter(Continent != 0) %>%
      mutate(GDP.PC=round(GDP.PC,0)) %>%
      mutate(Ppl=round(Ppl/1000000,2)) %>%
      mutate(PWaste.PC=round(PWaste.PC,2)) %>%
      arrange(desc(Ppl)) %>%
      mutate(Country = factor(Country, Country)) %>%
      mutate(text = paste("Country: ", Country, "\nPopulation (M): ", Ppl, "\nPlastic Waste Per Capita: ", PWaste.PC, "\nGDP per capita: ", GDP.PC, sep="")) %>%
      ggplot(aes(log10(x = GDP.PC), y = PWaste.PC, color = Continent, size = Ppl, text=text),tooltip="text") + 
      geom_point(alpha = 0.5) +
      ylim(0, 0.69) +
      scale_color_manual(values = c("#00AFBB", "#FC4E07", "#E7B800","#CC6666", "#9999CC", "#66CC99","#FF6666")) +
      scale_size(range = c(0.5, 10)) +  # Adjust the range of points size
      xlab('GDP per capita, PPP') + 
      ylab('Per capita plastic waste') +
      labs(color = "Continent") +
      ggtitle('Per capita plastic waste vs. GDP per capita, 2010') +
      hrbrthemes::theme_ipsum()
    
    ggplotly(bubble_GDP_WastePC, tooltip="text")
  })

  output$bubbleGDPWastePC2 <- renderPlotly({
    
    bubble_GDP_WastePC2 <- plastic %>% filter(Continent !=0) %>%
      mutate(GDP.PC=round(GDP.PC,0)) %>%
      mutate(Ppl=round(Ppl/1000000,2)) %>%
      arrange(desc(Ppl)) %>%
      mutate(Country = factor(Country, Country)) %>%
      mutate(text = paste("Country: ", Country, "\nPopulation (M): ", Ppl, "\nPer Capita Mismanaged Plastic Waste: ", Mismgt.PC, "\nGDP per capita: ", GDP.PC, sep="")) %>%
      ggplot(aes(log10(x = GDP.PC), y = Mismgt.PC, color = Continent, size = Ppl, text=text),tooltip="text") + 
      geom_point(alpha = 0.5) +
      ylim(0, 0.31) +
      scale_color_manual(values = c("#00AFBB", "#FC4E07", "#E7B800","#CC6666", "#9999CC", "#66CC99","#FF6666")) +
      scale_size(range = c(0.5, 10)) +  # Adjust the range of points size
      xlab('GDP per capita, PPP') + 
      ylab('Per capita mismanaged plastic waste') +
      labs(fill = "Continent") +
      ggtitle('Per capita mismanaged plastic waste vs. GDP per capita, 2010') +
      hrbrthemes::theme_ipsum()
    
    ggplotly(bubble_GDP_WastePC2, tooltip="text")
  })
  
  output$bubbleGDPWastePC3 <- renderPlotly({
    
    bubble_GDP_WastePC3 <- plastic %>% filter(Continent != 0) %>%
      mutate(Coastal.Ppl=round(Coastal.Ppl/1000000,2)) %>%
      mutate(Ppl=round(Ppl/1000000,2)) %>%
      arrange(desc(Ppl)) %>%
      mutate(Country = factor(Country, Country)) %>%
      mutate(text = paste("Country: ", Country, "\nPopulation (M): ", Ppl, "\nMismanaged plastic waste: ", Mismanaged, "\nCoastal population: ", Coastal.Ppl, sep="")) %>%
      ggplot(aes(log10(x = Coastal.Ppl), y = log(Mismanaged, base = exp(10)), color = Continent, size = Ppl, text=text),tooltip="text") + 
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm") +
      ylim(0.40, 1.6) +
      scale_color_manual(values = c("#00AFBB", "#FC4E07", "#E7B800","#CC6666", "#9999CC", "#66CC99","#FF6666")) +
      scale_size(range = c(0.5, 10)) +  # Adjust the range of points size
      xlab('Coastal population') + 
      ylab('Mismanaged plastic waste') +
      labs(fill = "Continent") +
      ggtitle('Mismanaged plastic waste vs. coastal population, 2010') +
      hrbrthemes::theme_ipsum()
    
    ggplotly(bubble_GDP_WastePC3, tooltip="text")
  })

  ####Reactive used by bar chart####
  dataset = reactive({
  if (input$selected == 'Population'){
      plastic %>% 
      select(Country, Ppl) %>% 
      arrange(desc(Ppl)) %>% 
      top_n(10) %>% 
      select(2,1)  
  }else if (input$selected == 'Coastal Population'){
      plastic %>% 
      select(Country, Coastal.Ppl) %>% 
      arrange(desc(Coastal.Ppl)) %>% 
      top_n(10) %>% 
      select(2,1)
  }else if (input$selected == 'GDP Per Capita'){
      plastic %>% 
      select(Country, GDP.PC) %>% 
      arrange(desc(GDP.PC)) %>% 
      top_n(10) %>% 
      select(2,1)
  }else if (input$selected == 'Plastic Waste Generation'){
      plastic %>% 
      select(Country, PWaste.G) %>% 
      arrange(desc(PWaste.G)) %>% 
      top_n(10) %>% 
      select(2,1)
  }else if (input$selected == 'Plastic Waste Per Capita'){
      plastic %>% 
      select(Country, PWaste.PC) %>% 
      arrange(desc(PWaste.PC)) %>% 
      top_n(10) %>% 
      select(2,1)
  }else if (input$selected == 'Mismanaged Plastic Generation'){
      plastic %>% 
      select(Country, Mismanaged) %>% 
      arrange(desc(Mismanaged)) %>% 
      top_n(10) %>% 
      select(2,1)
  }else if (input$selected == 'Mismanaged Plastic Per Capita'){
      plastic %>% 
      select(Country, Mismgt.PC) %>% 
      arrange(desc(Mismgt.PC)) %>% 
      top_n(10) %>% 
      select(2,1)
  }else if (input$selected == 'Mismanaged Plastic Share'){
      plastic %>% 
      select(Country, Mismgt.S) %>% 
      arrange(desc(Mismgt.S)) %>% 
      top_n(10) %>% 
      select(2,1)
  }else if (input$selected == 'Inadequately Managed Plastic Waste Share(Highest Risk)'){
      plastic %>% 
      select(Country, Inade.S) %>% 
      arrange(desc(Inade.S)) %>% 
      top_n(10) %>% 
      select(2,1)
  }else if (input$selected == '2008-2010 Avg GDP Per Capita Growth Rate'){
      plastic %>% 
      select(Country, Avg3Y.GDPG) %>% 
      arrange(desc(Avg3Y.GDPG)) %>% 
      top_n(10)} %>% 
      select(2,1)
  })
  ####bar chart####
  output$barChart1 <- renderPlotly({
    bar_chart_1 <-
      ggplot(data= dataset(), aes_string(x = 'Country', y = VDict())) +
      geom_col(fill = "#FF6666") +
      coord_flip() +
      scale_y_continuous(labels = scales::comma) +
      hrbrthemes::theme_ipsum() +
      theme(axis.title.y = element_blank())
    ggplotly(bar_chart_1) %>%
      layout(autosize = T)
  })
  ####box plot####
  output$boxplot1 <- renderPlotly({
    box_plot_1 <- plastic %>% filter(Development != 0) %>%
      ggplot(aes(x=Land,y=Mismgt.PC,fill=Land)) +
      geom_boxplot() +
      # ylim(0, 3300000) +
      ylab('Per capita mismanaged plastic waste(tons)') +
      xlab('Country geographic feature') +
      ggtitle('Mismanaged Plastic Waste: Coastal vs Land') +
      hrbrthemes::theme_ipsum()
      ggplotly(box_plot_1) %>%
      layout(width="100%")
  })
  
  output$boxplot2 <- renderPlotly({
    box_plot_2 <- plastic %>% filter(Eco != 0) %>%
      ggplot(aes(x=Eco,y=Mismgt.PC,fill=Eco)) +
      geom_boxplot() +
      # ylim(0, 3300000) +
      ylab('Per capita mismanaged plastic waste(tons)') +
      xlab('Economic growth') +
      ggtitle('Mismanaged Plastic Waste by Economic Status') +
      labs(color = "Economy") +
      hrbrthemes::theme_ipsum()
    ggplotly(box_plot_2) %>%
      layout(width="100%")
  })
  
  output$boxplot3 <- renderPlotly({
    box_plot_3 <- plastic %>% filter(Development != 0) %>%
      ggplot(aes(x=Development,y=Mismgt.PC,fill=Development)) +
      geom_boxplot() +
      # ylim(0, 3300000) +
      ylab('Per capita mismanaged plastic waste(tons)') +
      xlab('Development Status of Country') +
      ggtitle('Plastic Waste: Developed and Developing') +
      hrbrthemes::theme_ipsum()
    ggplotly(box_plot_3) %>%
      layout(width="100%")
  })
  
  output$boxplot4 <- renderPlotly({
    box_plot_4 <- plastic %>% filter(Development != 0) %>%
      ggplot(aes(x=Development,y=PWaste.PC, fill=Development)) +
      geom_boxplot() +
      ylim(0, 0.7) +
      ylab('Per Capita Plastic Waste(tons)') +
      xlab('Development Status of Country') +
      ggtitle('Plastic Waste: Developed vs. Developing') +
      hrbrthemes::theme_ipsum()
    ggplotly(box_plot_4) %>%
      layout(width="100%")
  })
  ####text####
  output$text1 <- renderText({
    text_df[,VDict()]
  })
  variables <- readr::read_csv("plastic.csv")
  variables <- variables %>% select(-X1, -year, -Code) %>% rename(`Coastal Population` = Coastal.Population, 
                                                    `GDP Per Capita` = GDP.Per.Capita, 
                                                    `Plastic Waste Generation Total` = Plastic.Waste.Generation.Total, 
                                                    `Plastic Waste Per Capita` = Plastic.Waste.Per.Capita,
                                                    `Mismanaged Plastic Waste Total` = Mismanaged.Plastic.Waste.Total, 
                                                    `Mismanaged Plastic Waste Per Capita` = Mismanaged.Plastic.Waste.Per.Capita.,
                                                    `Mismanaged Plastic Waste Share` = Mismanaged.Plastic.Waste.Share, 
                                                    `Inadequately Managed Waste Share` = Inadequately.Managed.Waste.Share, 
                                                    `Avg 3 Year GDP Growth` = Avg.3Y.GDP.Growth, 
                                                    `Economic Status` = Eco) %>%
    relocate(`Plastic Waste Per Capita`, .after = `GDP Per Capita`) %>%
    mutate(`GDP Per Capita` = round(`GDP Per Capita`, digits = 2),
           `Avg 3 Year GDP Growth` = round(`Avg 3 Year GDP Growth`, digits = 2))
  brks1 <- quantile(variables$`GDP Per Capita`, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs1 <- viridis::viridis(n=length(brks1)+1, alpha=.5, direction = -1)
  
  brks2 <- quantile(variables$`Plastic Waste Per Capita`, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs2 <- viridis::viridis(n=length(brks2)+1, alpha=.5, direction = -1)
  
  brks3 <- quantile(variables$`Plastic Waste Generation Total`, probs = seq(.05, .95, .05), na.rm = TRUE)
  clrs3 <- viridis::viridis(n=length(brks2)+1, alpha=.5, direction = -1)
  
  output$variablesTable <- DT::renderDataTable({
    datatable(variables, rownames = F) %>%
      formatStyle('Country', background="skyblue", fontWeight='bold') %>%
      formatStyle('GDP Per Capita', backgroundColor = styleInterval(brks1, clrs1)) %>%
      formatStyle('Plastic Waste Per Capita', backgroundColor = styleInterval(brks2, clrs2)) %>%
      formatStyle('Plastic Waste Generation Total', backgroundColor = styleInterval(brks3, clrs3)) %>%
      formatCurrency('GDP Per Capita', currency = "$") %>%
      formatCurrency(c('Plastic Waste Generation Total', 'Mismanaged Plastic Waste Total', 'Population'), currency = "", interval = 3, digits = 0) %>%
      formatPercentage('Avg 3 Year GDP Growth', interval = 2, mark = ".")
  })
  
  
  set.seed(1)   # for reproducibility
  df_east = matrix(sample(seq(100, 5000, 50), 49), ncol = 7)
  row.names(df_east) = c("Company A" ,"Company B" ,"Company C" ,"Company D","Company E", "Company F" ,"Company G")
  colnames(df_east) = row.names(df_east)
  
  set.seed(2)  
  df_west = matrix(sample(seq(100, 5000, 50), 49), ncol = 7)
  colnames(df_west) = row.names(df_east)
  row.names(df_west) = row.names(df_east)
  
  
  set.seed(3)   
  df_south = matrix(sample(seq(100, 5000, 50), 49), ncol = 7)
  colnames(df_south) = row.names(df_east)
  row.names(df_south) = row.names(df_east)
  
  set.seed(4)  
  df_north = matrix(sample(seq(100, 5000, 50), 49), ncol = 7)
  colnames(df_north) = row.names(df_east)
  row.names(df_north) = row.names(df_east)
  
  output$distPlot <- renderChorddiag({
    
    if(input$select_market =="East"){
      chorddiag(df_east, showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
    }else if(
      input$select_market =="West"){
      chorddiag(df_west, showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
    } else if(
      input$select_market =="South"){
      chorddiag(df_south, showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
    }else{
      chorddiag(df_north, showTicks = F, groupnameFontsize = 14, groupnamePadding = 10, margin = 90)
    }
    
  })
  
  # mat <- variables %>% select(Country, Continent, `Plastic Waste Generation Total`) %>% drop_na()
  # chordDiagram(mat, annotationTrack = "grid")
  # circos.track(track.index = 1, panel.fun = function(x, y) {
  #   circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
  #   facing = "clockwise", niceFacing = TRUE, adj = c(-0.5, 0.5))
  #   }, bg.border = NA)
}