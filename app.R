
library("shiny")

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
                 navbarMenu("Tab3",
                            
                            #t3p1
                            tabPanel(title = "t3p1", value = "tab6",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("t3p1")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("about")),
                                                actionLink('t6left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t6right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                         )
                                       )
                                     )
                            ),
                            
                            #t2p2
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
}

shinyApp(ui, server)
