
library("shiny")
library(dplyr)
library(ggplot2)
library(plotly)

map_year_choices <- c(1997:2016)
map_age_choices <- list("All ages" = "all_ages", "15 to 19" = "15to19", "15 to 17" = "15to17", "18 to 19" = "18to19",
                        "20 to 24" = "20to24", "25 to 29" = "25to29", "30 to 34" = "30to34", "35 to 39" = "35to39", "40 to 44" = "40to44")

#----------------------UI----------------------------
ui <- navbarPage(title = "Abortions", id = "navbar",
                 
                 #About Page
                 tabPanel(title = "About", value = "tab1",
                          fluidPage(fluidRow(
                            column(10,
                                   h1("About/ Why / IDK")),
                            column(2,
                                   icon('question-circle', class='fa-2x helper-btn'),
                                   tags$div(class="helper-box", style="display:none",
                                            p('This is about page')),
                                   actionLink('abtleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                   actionLink('abtright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                   
                            )
                          )),
                          sidebarLayout(
                            sidebarPanel(
                              
                              h3("Data Resources"),
                              h3("Aditional Information")
                            ),
                            mainPanel(
                              h2("About"),
                              p("This website is designed to provide its viewers SOMETHING IDK PUT IT HERE THO
                                <br>"),
                              h3("Limitations"),
                              p("One of the greatest barriers we encountered while constructing this website and 
                                doing our research was the scattered and incomplete data available on abortions
                                in the US. The CDC requests information from states but their compliance is purely
                                voluntary, along with any publication of the data themselves. Therefore, we struggled
                                at times to find up-to-date information regarding abortion, or statistics that could
                                be found for every state.")
                          )
                 )),
                 
                 #Tab1
                 navbarMenu(
                   "TabOne",
                   
                   #Tab1 Page1
                   tabPanel(title = "T1P1", value = "tab2",
                            
                            fluidPage(
                              fluidRow(
                                column(10,
                                       h1("t1p1")),
                                column(2,
                                       icon('question-circle', class='fa-2x helper-btn'),
                                       tags$div(class="helper-box", style="display:none",
                                                p("First page of tab 1")),
                                       actionLink('t2left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                       actionLink('t2right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                       
                                                )
                                       )
                                )
                          ),
                   
                   #tab1Page2
                   tabPanel(title= "tab1page2", value="tab3",
                            
                            fluidPage(
                              fluidRow(
                                column(10,
                                       h1("tab1page2")),
                                column(2,
                                       icon('question-circle', class='fa-2x helper-btn'),
                                       tags$div(class="helper-box", style="display:none",
                                                p("tab1page2aboutt")),
                                       actionLink('t3left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                       actionLink('t3right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                       
                                                )
                                       )
                            ))
                              ),
                 
                 #Tab2
                 navbarMenu("Tab2",
                            
                            #t2p1
                            tabPanel(title = "t2p1", value = "tab4",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("t2p1")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("about")),
                                                actionLink('t4left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t4right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                                         )
                                                )
                                         )
                                     ),
                            
                            #t2p2
                            tabPanel(title = "t2p2", value = "tab5",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("t2p2")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("Aboutt")),
                                                actionLink('t5left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t5right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                                         )
                                                )
                                         )
                                     )
                            ),
                 #Tab3
                 navbarMenu("WA Counties",
                            
                            #t3p1
                            tabPanel(title = "County and Age", value = "tab6",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("WA State Abortions per County and by Age Group")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("A map displaying WA State abortion data per county
                                                           and by age group")),
                                                actionLink('t6left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t6right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                         )
                                       )
                                     ), 
                                     mainPanel(
                                       h2("Background"),
                                       p("With the current political climate leading some states to implement increasingly stricter
                                         laws regarding abortion rights and access, we thought it would be interesting to zoom in on
                                         a more liberal state that respects abortion rights, to see how their abortion rates have been
                                         changing through the years. \n"),
                                       p("Using the interactive map below, you can explore how the rates of abortions per 1,000 women
                                         have changed between 1997 and 2016 in Washington state. You can further filter the results by age
                                         to see how any specific age bracket of women have changed through the years."),
                                       selectInput('mapagevar', label = 'Age to Map', choices = map_age_choices),
                                       selectInput('mapyear', label = 'Year to Map', choices = map_year_choices),
                                       plotlyOutput('map'),
                                       h2("Analysis"),
                                       p(paste("One immediately apparent result from looking through this map is the decreasing rate of
                                         abortions across all age groups since 1997. In 1997 the abortion rate for the whole state
                                               was sitting at <b>", cleaned_data[761,2], "</b>. By the year 2016 however, the overall
                                               rate fell to <b>", cleaned_data[1,2], "</b>.")),
                                       p("\n Another point to note is the interesting choice in data collection by the Washington 
                                         State Department of Health to mark any abortion rates where the occurences of abortions 
                                         were less than 5 as 'NA'. This makes it somewhat difficult to be precise with rates, though
                                         in instances with such low counts of abortions the corresponding rate would not be very high
                                         anyways.")
                                     )
                            ),
                            
                            #t3p2
                            tabPanel(title = "t3p2", value = "tab7",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("t3p2")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("Aboutt")),
                                                actionLink('t7left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t7right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                         )
                                       )
                                     )
                            )
                 ),
                 
                 #Tab4
                 navbarMenu("Tab4",
                            
                            #t4p1
                            tabPanel(title = "t4p1", value = "tab8",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("t4p1")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("about")),
                                                actionLink('t8left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t8right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                         )
                                       )
                                     )
                            ),
                            
                            #t4p2
                            tabPanel(title = "t4p2", value = "tab9",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("t4p2")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("Aboutt")),
                                                actionLink('t9left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t9right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                         )
                                       )
                                     )
                            )
                 ),
                 inverse = TRUE,
                 tags$head(
                   tags$script(type="text/javascript", src="alert.js"),
                   tags$link(rel="stylesheet", type="text/css",href="style.css")
                 )
        )

#-------------------------------------------------------Server----------------------------------------------------------#

server <- function(input, output, session) {
  #update active tab in navbar when arrows are clicked
  leftarrowclicks <- reactive({
    input$abtleft+input$t2left+input$t3left+input$t4left+input$t5left+input$t6left+input$t7left+input$t8left+input$t9left
  })
  rightarrowclicks <- reactive({
    input$abtright+input$t2right+input$t3right+input$t4right+input$t5right+input$t6right+input$t7right+input$t8right+input$t9right
  })
  observe({
    if(leftarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6', 'tab7', 'tab8', 'tab9')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current-1])
  })
  observe({
    if(rightarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab3', 'tab4', 'tab5', 'tab6', 'tab7', 'tab8', 'tab9')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current+1])
  })
  
  output$map <- renderPlotly({ #Chloropleth Map
    
    map_selected_data <- filter(cleaned_data, substring(county_name, nchar(county_name) - 3) == input$mapyear)
    map_selected_data$county_name <- substring(map_selected_data$county_name, 0, nchar(map_selected_data$county_name) - 4)
    map_selected_data$county_name <- paste0(map_selected_data$county_name, " County")
    
    map_selected_data <- select(map_selected_data, county_name, input$mapagevar)
    colnames(map_selected_data) <- c("county_name", "rate")
    map_selected_data <- left_join(wa_county_data, map_selected_data, by = "county_name")
    map_selected_data$text <- paste("<i> Abortion rate: </i><b>", map_selected_data$rate, "</b> \n <i>Year:</i> <b>", input$mapyear, "</b>")
    
    p <- ggplot() +
      geom_sf(map_selected_data, mapping = aes(fill = as.numeric(rate), text = text), color="#FFFFFF") +
      labs(fill = "Abortion Rate \n per 1,000 Women") +
      scale_fill_gradientn(limits = c(0,65), colors = c("lightblue", "darkorchid1", "purple"))
      
    return(
      ggplotly(p, height = 400, width = 1000, tooltip = c("text")) %>%
      style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")
    )
  })
  
}

shinyApp(ui, server)
