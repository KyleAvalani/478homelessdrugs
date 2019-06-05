
library("shiny")
library(ggplot2)
library(dplyr)
library(ggplot2)
library(plotly)
source('wa_county_age_analysis.R')
source("Child_Health.R")
data <- read.csv("data/abortion_data.csv", stringsAsFactors = FALSE)
map_year_choices <- c(1997:2016)
map_age_choices <- list("All ages" = "all_ages", "15 to 19" = "15to19", "15 to 17" = "15to17", "18 to 19" = "18to19",
                        "20 to 24" = "20to24", "25 to 29" = "25to29", "30 to 34" = "30to34", "35 to 39" = "35to39", "40 to 44" = "40to44")

#----------------------UI----------------------------
ui <- navbarPage(title = "Abortions", id = "navbar",
                 
                 #About Page
                 tabPanel(title = "About", value = "tab1",
                          fluidPage(fluidRow(
                            column(10,
                                   h1("An Overview of U.S. Abortions")),
                            column(2,
                                   icon('question-circle', class='fa-2x helper-btn'),
                                   tags$div(class="helper-box", style="display:none",
                                            p("This the is 'About' page.")),
                                   actionLink('abtleft', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                   actionLink('abtright', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                   
                            )
                          )),
                          sidebarLayout(
                            sidebarPanel(
                              
                              h3("Data Resources"),
                              p("Data for national rates of abortion came from the CDC's Abortion Surveillance Report."),
                              tags$li(tags$a(href="https://www.cdc.gov/mmwr/volumes/67/ss/ss6713a1.htm", "CDC Abortion Surveillance 2015")),
                              tags$li(tags$a(href="https://www.kff.org/womens-health-policy/state-indicator/abortion-rate/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D", "Rates of Legal Abortion, 2015")),
                              p(),
                              p("Data for Washington abortion rates originates from the Washington State Department of Health."),
                              tags$li(tags$a(href="https://l.messenger.com/l.php?u=https%3A%2F%2Fwww.doh.wa.gov%2FDataandStatisticalReports%2FHealthStatistics%2FAbortionPregnancy%2FAbortionPregnancyTablesbyTopic&h=AT0Ab66Ax0ng03Uoc7m4N5BqDP5YxoYSXxWQygkxO9ejzB_xGvCJ1Rp0gfNTSlsqYo6SMywjgHpEX3Kpa3m0cCcH_fpUmBffwwPLZvGgaxoKVhi6Df0-3dlOotuBkbd4T_aQAaZsaUc", "Washington State Department of Health")),
                              tags$li(tags$a(href="https://www.doh.wa.gov/DataandStatisticalReports/HealthStatistics/AbortionPregnancy/AbortionPregnancyTablesbyTopic?fbclid=IwAR3SEcS15GcxrUd4cTiVUundpuwUt_YyNdoV0wIiHU5A--9hK4QfSLDt1xM", "Weeks of Gestation by Age of Woman in WA State")),
                              p(),
                              p("Data on the restrictiveness of abortion laws (by state) comes from the Guttmacher Institute."),
                              tags$li(tags$a(href="https://www.guttmacher.org/state-policy/explore/overview-abortion-laws", "An Overview of Abortion Laws")),
                              p(),
                              p("Data on Levels of Abortion Access"),
                              tags$li(tags$a(href="https://ballotpedia.org/Abortion_regulations_by_state", "Ballotpedia - Levels of Abortion Access")),
                              p(),
                              p("Data on other Child Health Concerns"),
                              tags$li(tags$a(href="https://www.usnews.com/news/best-states/rankings/education", "US news - State Education Rankings")),
                              tags$li(tags$a(href="https://www.acf.hhs.gov/cb/resource/trends-in-foster-care-and-adoption", "Children's Bureau - Adoptions and Foster Care")),
                              tags$li(tags$a(href="https://www.cdc.gov/nchs/pressroom/sosmap/infant_mortality_rates/infant_mortality.htm","CDC - Infant Mortality Rates")),
                              tags$li(tags$a(href="https://www.census.gov/data/tables/time-series/demo/popest/2010s-state-total.html", "Census population data")),
                              tags$li(tags$a(href="https://www.cdc.gov/nchs/pressroom/sosmap/teen-births/teenbirths.htm", "CDC - Teen Birth Rates")),
                              h3("Additional Information"),
                              p("The team behind this project consists of students Mackenzie Hutchison, Maddi Cummins, Kyle Avalani, and Tyler Marcinyshyn.")
                            ),
                            mainPanel(
                              h2("About"),
                              p("The legality and restrictiveness placed on abortion is a controversial topic in the US. The purpose of this app is to help the general public better understand the statistical trends pertaining to abortion. In doing so, we hope to help individuals better evaluate the necessity of various abortion-related laws. The following lists our areas of interest for this analysis, as well as what insight we hope to gain from said topics: 
                                "),
                              tags$ol(
                                tags$li("Does the age of a woman influence how late they get their abortion? If a woman's age may determine when they are able to get an abortion, we can then ask if it's fair to restrict women from having abortions at any point during their pregnancy."),
                                tags$li("How does the accessibility of abortion correlate to indicators of child health? With this information, we can explore the fairness of placing restrictions on abortion in areas with significant child health problems."),
                                tags$li("How have abortion rates changed in a liberal state (Washington)? We can use the answer to explore how abortion rates have changed in liberal areas in times of increasingly stricter laws on abortion."),
                                tags$li("Does required parental involvement correlate to rates of abortion among minors? Does mandated counseling before abortion is performed correlate to rates of abortion? With this information, we can evaluate the effectiveness of these restrictive abortion laws.")
                              ),
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
                   "Pregnancy Duration by Age",
                   
                   #Tab1 Page1
                   tabPanel(title = "Term of Abortion by Age", value = "tab2",
                            
                            fluidPage(
                              fluidRow(
                                column(10,
                                       h1("Duration of Pregnancy by Age of Woman in WA State")),
                                column(2,
                                       icon('question-circle', class='fa-2x helper-btn'),
                                       tags$div(class="helper-box", style="display:none",
                                                p("A distribution of when women get abortions during their pregnancy, depending on their age.")),
                                       actionLink('t2left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                       actionLink('t2right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                       
                                )
                              ),
                              sidebarLayout(
                                sidebarPanel(
                                  selectInput(
                                    inputId = "Year",
                                    label = strong("Year"),
                                    choices = c("2011", "2012", "2013", "2014", "2015", "2016")
                                  ),
                                  checkboxGroupInput(
                                    inputId = "Age",
                                    label = strong("Select the Age of the Woman"),
                                    choices = c("Under 15", "15-19", "20-24", "25-29", "30-34",
                                                "35-39", "40-44", "45 and Over", "Unknown"),
                                    selected = c("Under 15", "15-19", "20-24", "25-29", "30-34",
                                                 "35-39", "40-44", "45 and Over", "Unknown")
                                  ) ,
                                  checkboxGroupInput(
                                    inputId = "weeks_of_gestation",
                                    label = strong("Weeks of Gestation"),
                                    choices = c("Under 9", "9-12", "13-15", "16-19", "Over 20", "Unknown"),
                                    selected = c("Under 9", "9-12", "13-15", "16-19", "Over 20", "Unknown")
                                  ),
                                  selectInput(
                                    inputId = "Measure",
                                    label = strong("Unit of Measurement"),
                                    choices = c("Count", "Percentage")
                                  )
                                ),
                                mainPanel(
                                  plotOutput(outputId = "weeks_pregnant_by_age"),
                                  h2("Research Question"),
                                  p("Does the age of the woman influence how late they get their abortion (in Washington)?"),
                                  h2("Background"),
                                  p("Many laws aim to restrict access to late term abortions. However, it is important to consider age as a contextual variable in influencing a woman's access to an abortion. Older adults may have more independence in such a decision, which younger women do not have. From here, we can analyze whether a woman's age may determine when they are able to get an abortion. From here we can ask, is it fair to restrict women from having abortions at any point during their pregnancy?"),
                                  h2("Methodology"),
                                  tags$li("Pregnancies by term calculated by percent: With this, we are able to compare when women in Washington tend to get abortions, across all ages. This eliminates the influence of the fact that older women tend to have more pregnancies, therefore more abortions."),
                                  tags$li("Pregnancies by term calculated by raw count: With this, we are able to view how which age groups tend to get the most abortions in Washington, and when."),
                                  h2("Results"),
                                  p("Below is the highest measure for each gestation period, along with their respective years and age groups. It is important to note that the most abortions, in terms of raw count, across all gestation periods occurs for 20-24 year olds. However, when analyzing in terms of percentage of total abortions for an age group, the under 15 age group has the highest percentage of abortions occurring late term, out of all of the age groups. Although it may be more common for a 20-24 year old to have an abortion, there is a hgiher frequency of late term abortions in women under 15."),
                                  tableOutput(outputId = "highest_percent"),
                                  tableOutput(outputId = "highest_count")
                                )
                              )
                            )
                   )
                              ),
                 
                 #Tab2
                 navbarMenu("Compared to Other Child Health",
                            
                            
                            tabPanel(title = "Abortion Access vs Other Child Health Concerns", value = "tab4",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("Abortion Access vs Other Child Health Concerns")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("This page compares how states treat other child health issues grouped by their level of abortion access. To change the issue use the input tab on the left side panel. The boxplots are colored based on their mean values. The colors range from blue to red indicating which groups are better or worse for the issue.")),
                                                actionLink('t4left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t4right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                         )
                                       )
                                     ),
                                     sidebarLayout(
                                       sidebarPanel(
                                         selectInput("other_child_health_issue", "Children's Health Issues", c("Infant Mortality", "Education Rank", "Birth Rate", "Teen Birth Rate", "Adoptions", "Foster Children", "Adoptions per Foster Child")),
                                         textOutput("child_health_issue_explain")
                                       ),
                                       mainPanel(
                                         plotOutput("child_health_issue_boxplot"),
                                         h2("Research Question"),
                                         p("Does the level of abortion access (severely restricted, restricted, some, protected, or strongly protected) of a state relate to how they address other child health issues?"),
                                         h2("Background"),
                                         p("A main argument for restrictive access to abortions is a claim to be caring for the child. We were curious to see if states with restrictive access to abortions also extend their child care beyond the womb. The child health issues we decided to make our comparison against were education rank, infant mortality, rate of births, rate of teen births, rate of adoptions, foster kids per capita, and adoptions per child in foster care."),
                                         h2("Methodology"),
                                         p("In order to make these comparisons we first group them by their level of abortion access and then plotted the issue specific values for each state in the level with a boxplot. Our main interest was comparing the medians which box plots easily show. The boxplots are also colored by their median, access level's with medians that are better for the issue are colored in blue, while access levels with worse medians for the issue are colored in red. Graphing this as a boxplot also allows us to see the spread and quartiles of the data to help understand how realilistic the median is at representing the data."),
                                         h2("Results"),
                                         p("Below are the median values for each child health issue grouped by their level of access."),
                                         tableOutput("child_health_issue_table"),
                                         tags$li("med_ed: Each state was ranked from 1 (best) to 50 (worst), the median represents the average education ranking for each level of access. Based on the medians states with some access or either level of protected access tend to be ranked higher in education. However, the spread is pretty large for most of the levels of access so it is hard to determine if this is actually a good indicator."),
                                         tags$li("med_ad_rate: The median adoptions per million people for each level of access. There didn't seem to be an indication that places with different levels of restrictions were more or less likely to adopt."),
                                         tags$li("med_f_rate: The median number of foster children per million people for each level of access. There didn't seem to be an indication that places with different levels of restrictions were more or less likely to have higher numbers of foster kids."),
                                         tags$li("med_b_rate: The median births per million people for each level of access. There didn't seem to be an indication that places with different levels of restrictions were more or less likely to have higher birth rates. Except for in severly restricted areas which are higher than the others."),
                                         tags$li("med_inf_mort: The infant mortality rate for each level of access. The medians and spread both indicated that places with restricted and severly restricted access to abortions have higher infant mortality rates."),
                                         tags$li("med_ad_f: The median adoptions per child in foster care for each level of access. There didn't seem to be an indication that places with different level of restrictions were more or less likely to have higher rates of adoptions per foster child."),
                                         tags$li("med_teen: The median teen birth rate by level of access. States with higher restrictions tended to have higher rates of teen births.")
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
                                                           and by age group.")),
                                                actionLink('t6left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t6right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                         )
                                       )
                                     ), 
                                     mainPanel(
                                       h2("Research Question"),
                                       p("How have abortion rates changed in a liberal state?"),
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
                                       h2("Methodology"),
                                       p('We took a look at abortion rates pertaining to each county in WA and then
                                         conducted the following steps:'),
                                       tags$ol(
                                         tags$li("We merged every available table of county data."),
                                         tags$li("We then cleaned up the data and added summary statistics."),
                                         tags$li("Finally, we merged the data with a list of shapefile data for WA counties and plotted it all to a map.")
                                       ),
                                       h2("Results"),
                                       p(paste("One immediately apparent result from looking through this map is the decreasing rate of
                                         abortions across all age groups since 1997. In 1997 the abortion rate for the whole state
                                               was sitting at ", cleaned_data[761,2], ". By the year 2016 however, the overall
                                               rate fell to ", cleaned_data[1,2], ".")),
                                       p("\n Another point to note is the interesting choice in data collection by the Washington 
                                         State Department of Health to mark any abortion rates where the occurences of abortions 
                                         were less than 5 as 'NA'. This makes it somewhat difficult to be precise with rates, though
                                         in instances with such low counts of abortions the corresponding rate would be quite low
                                         anyways. These occurences are recorded as 0 in the above map.")
                                     )
                            )
                 ),
                 
                 #Tab4
                 navbarMenu("Legislature",
                            
                            #t4p1
                            tabPanel(title = "Parental Involvement", value = "tab8",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("Parental Involvement")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("Does required parental involvement correlate to rates of abortion among minors?")),
                                                actionLink('t8left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t8right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                         )
                                       ),
                                       selectInput("s_age",
                                                   "Select a viewing mode:",
                                                   list("Individual Age Groups", "Altogether")),
                                       plotOutput("plot_involve"),
                                       h2("Research Question:"),
                                       p("Does required parental involvement correlate to rates of abortion among minors?"),
                                       h2('Background'),
                                       p("37 states require some form of parental involvement in a minor's decision to have an abortion. Of these states, 26 require that one or both parents consent to the procedure. 11 require a parent to at least be notified. 5 states have parental involvement permanently enjoined by court order, but with no any law in effect."),
                                       h2('Methodology'),
                                       p('To produce our results, we looked at three variables pertaining to each state:'),
                                       tags$ul(
                                         tags$li("Number of abortions by age group (for year 2015)"),
                                         tags$li("Total number of abortions (across all ages, 2015)"),
                                         tags$li("Requirement of parental involvement for minors")
                                       ),
                                       p("For our first variable, the age groups we examine are specifically those of minors: 'Under 15', '15', '16' and '17' year olds."),
                                       tags$ol(
                                         tags$li("Using the number of abortions by age group and total number of abortions, we calculated the abortion rate of each age group for each state." ),
                                         tags$li("We categorized each state into a grouping based on what type of parental involvement they require: 'consent', 'notice', 'consent and notice', 'enjoined', or 'none'."),
                                         tags$li("For each grouping, we calculated the average percent of total abortions each age group had contributed to.")
                                         ),
                                       h2('Results'),
                                       tableOutput('p_table'),
                                       p("Notably, at 2.85%, the 'Consent and Notice' grouping boasts the lowest average percent that minors contributed to total abortions. Similary, at 3.22%, the 'Consent' grouping boasts the second lowest percentage. An average of .78% more minors contributed to total abortions in the 'None' grouping than in the 'Consent and Notice' grouping. From this information, it's safe to say that required parental involvement does correlate to lower rates of abortion among minors."),
                                       p("Interestingly, the 'Enjoined' grouping is associated with the highest percent of abortion among minors. This may speak to a low effectiveness of enjoining some action by court order without the backing of law.")
                                     )
                            ),
                            
                            #t4p2
                            tabPanel(title = "Mandated Counseling", value = "tab9",
                                     
                                     fluidPage(
                                       fluidRow(
                                         column(10,
                                                h1("Mandated Counseling")),
                                         column(2,
                                                icon('question-circle', class='fa-2x helper-btn'),
                                                tags$div(class="helper-box", style="display:none",
                                                         p("Does mandated counseling before abortion is performed correlate to rates of abortion?")),
                                                actionLink('t9left', class = 'larrow', icon=icon('arrow-left', class='fa-2x'), label=NULL),
                                                actionLink('t9right', class = 'rarrow', icon=icon('arrow-right', class='fa-2x'), label=NULL)
                                                
                                         )
                                       ),
                                       plotOutput("plot_counsel"),
                                       h2('Research Question'),
                                       p("Does mandated counseling before abortion is performed correlate to rates of abortion?"),
                                       h2('Background'),
                                       p("18 states mandate that women be given counseling prior to an abortion. For 5 states, this counseling includes information on the alleged link between abortion and breast cancer. For 13 states, this counseling includes info on the ability of a fetus to feel pain."),
                                       h2('Methodology'),
                                       p('We looked at three variables pertaining to each state:'),
                                       tags$ul(
                                         tags$li("The rate of legal abortions per 1,000 women aged 15-44 (year 2015)"),
                                         tags$li("Whether the state mandates counseling on fetal pain"),
                                         tags$li("Whether the state mandates counseling on the link between breast cancer and abortion")
                                       ),
                                       p("We then conducted the following steps:"),
                                       tags$ol(
                                         tags$li("We categorized each state into a grouping based on the types of counseling they require."),
                                         tags$li("We used the abortion rates of each grouping to generate a box plot. We then found the average abortion rate for each grouping.")
                                       ),
                                       h2('Results'),
                                       tableOutput('counsel_avg_rates'),
                                       p("The average abortion rate was 2.17 lower for states that require 'breast cancer link' counseling than in states that don't require counseling. The average abortion rate was 2.37 lower for states that require 'fetal pain' counseling than in states that don't require counseling. Overall, the states which require counseling correlate to lower rates of abortion. However, this could be because states with mandated counseling are more likely to have other restrictive abortion laws in place."),
                                       p("Notably, we consider the state of 'New York' an outlier for the groupings of 'No Counseling' and 'No FP Counseling'. New York holds an abortion rate of 23 per every 1000 women. If this value was included in our 'no counseling' groupings, then abortion rates associated with these groupings would stand even greater.")
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

source('prep_data.R')

server <- function(input, output, session) {
  #update active tab in navbar when arrows are clicked
  leftarrowclicks <- reactive({
    input$abtleft+input$t2left+input$t4left+input$t6left+input$t8left+input$t9left
  })
  rightarrowclicks <- reactive({
    input$abtright+input$t2right+input$t4right+input$t6right+input$t8right+input$t9right
  })
  observe({
    if(leftarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab4', 'tab6', 'tab8', 'tab9')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current-1])
  })
  observe({
    if(rightarrowclicks() == 0) {return()}
    tabOptions <- c('tab1', 'tab2', 'tab4', 'tab6', 'tab8', 'tab9')
    current <- isolate(which(input$navbar==tabOptions))
    updateTabsetPanel(session, 'navbar', selected=tabOptions[current+1])
  })
  output$plot_involve <- renderPlot({
    if (input$s_age == 'Individual Age Groups') {
      ggplot(p_inv, aes(x=p_involvement, y=rate, fill=age_group)) +
        geom_bar(stat="identity", position=position_dodge()) +
        ylab('Avg. Percent of Total Abortions') +
        xlab('Required Parental Involvement') +
        labs(fill='Age Group', title="Required Parental Involvement vs. Average Abortion Rate Among Minors")
    } else if (input$s_age == 'Altogether') {
      ggplot(p_involve, aes(x=p_involvement, y=rate.minor, fill=p_involvement)) +
        geom_bar(stat="identity") +
        ylab('Avg. Percent of Total Abortions') +
        xlab('Required Parental Involvement') +
        labs(fill='Age Group', title="Required Parental Involvement vs. Average Abortion Rate Among Minors") +
        theme(legend.position = "none")
    }
  })
  output$plot_counsel <- renderPlot({
    ggplot(counsel, aes(x=Counseling, y=Abortion_Rate, fill=Counseling)) +
      geom_boxplot() +
      ylab('Abortion Rate') +
      labs(title="Type of Mandated Counseling Vs. Abortion Rates")
  })
  output$counsel_avg_rates <- renderTable(c_avgs)
  output$p_table <- renderTable(p_involve_t)
  
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
      scale_fill_gradientn(limits = c(0,65), colors = c("lightblue", "darkorchid1", "purple")) + 
      geom_sf_text(map_selected_data, mapping = aes(label = substring(county_name, 0, nchar(county_name) - 7)), colour = "black", size = 2) +
      theme(axis.title.x = element_blank(), axis.title.y = element_blank())
      
    return(
      ggplotly(p, height = 600, width = 800, tooltip = c("text")) %>%
      style(hoverlabel = list(bgcolor = "white"), hoveron = "fill")
    )
  })
  
  output$weeks_pregnant_by_age <- renderPlot({
    plot_data <- data %>% 
      filter(Year == input$Year) %>% 
      filter(Metric == input$Measure) %>% 
      filter(Age %in% input$Age) %>% 
      filter(weeks_pregnant %in% input$weeks_of_gestation)
    plot <- ggplot(plot_data) +
      geom_col(mapping = aes(x = Age, y = value, fill = weeks_pregnant), position = "dodge") +
      labs(
        title = "Weeks of Gestation by Age",
        x = "Age",
        y = input$Measure,
        fill = "Weeks Pregnant"
      )
    #ggplotly(plot)
    return(plot)
  })
  
  output$highest_percent <- renderTable({
    results_data_percent <- data %>% 
      group_by(weeks_pregnant) %>% 
      filter(Metric == "Percentage") %>% 
      filter(value == max(value)) %>% 
      select(Age, Year, weeks_pregnant, value)
    colnames(results_data_percent) <- c("Age", "Year", "Weeks Pregnant", "Percent")
    return(results_data_percent)
  })
  
  output$highest_count <- renderTable({
    results_data_count <- data %>% 
      group_by(weeks_pregnant) %>% 
      filter(Metric == "Count") %>% 
      filter(value == max(value)) %>% 
      select(Age, Year, weeks_pregnant, value)
    colnames(results_data_count) <- c("Age", "Year", "Weeks Pregnant", "Count")
    return(results_data_count)
  })
  
  output$child_health_issue_boxplot <- renderPlot({
    access_boxplot(input$other_child_health_issue)
  })
  
  output$child_health_issue_explain <- renderText({
    issue_text(input$other_child_health_issue)
  })
  
  output$child_health_issue_explain <- renderTable({
    issue_table
  })
}

shinyApp(ui, server)
