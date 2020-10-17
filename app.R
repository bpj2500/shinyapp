#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny) 
library(shinydashboard) 
library(plotly)
library(dplyr) 
library(readr) 
library(ggplot2) 
library(shiny) 
library(shinydashboard)
library(tidyr)

college_data <- read_csv("college_data.csv") 
avgs_2014 <- college_data %>% 
    filter(year == 2014) %>% 
    group_by(type) %>% 
    summarise(yearly_cost = mean(net_cost)) %>% 
    select(yearly_cost) %>% 
    unlist() %>% 
    unname()






# Define UI for application that draws a histogram
ui <- 
    
    dashboardPage(
        dashboardHeader(title = "EDA of College Data"),
        dashboardSidebar(
            sidebarUserPanel("Author : Brian Perez"), 
            
            sidebarMenu( 
                menuItem("General Trends in the USA", 
                         tabName = "intro", icon = icon("book-reader"),
                         menuSubItem("Demographics 2014", tabName = "subitem1"), 
                         menuSubItem("Countrywide Yearly Trends", tabName = "subitem2"), 
                         menuSubItem("Tuition Costs & Income Levels", tabName = "subitem3"))
                )
            
        ),
        
        
#########        
        dashboardBody( 
            tabItems(      
                tabItem(tabName = "subitem1", h2("College Demographics in 2014"), 
                        fluidRow( 
                            box(plotlyOutput("plot1"), width = 4), 
                            box(plotlyOutput("plot2"), width = 6)
                        )
                ),  
                tabItem(tabName = "subitem2", h2("Tuition & Financial Aid"), 
                        fluidRow( 
                            box(plotlyOutput("plot3"), width = 4), 
                            box(plotlyOutput("plot4"), width = 6), 
                            box(plotlyOutput("plot5"), width = 5)
                            )
                ), 
                tabItem(tabName = "subitem3", h2("Income Levels & Financial Costs"), 
                        fluidRow( 
                            box(plotOutput("plot6"), width = 5), 
                            box(plotOutput("plot7"), width = 5),
                            box(plotlyOutput("plot8"), width = 5), 
                            box(plotlyOutput("plot9"), width = 5)
                            )
                        )
                
                )
        )
)














# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlotly( 
        college_data %>% 
            filter(category != "Total Minority" & category != "Unknown") %>% 
            group_by(type, category) %>%
            summarise(group_sum = sum(enrollment)) %>% 
            ungroup() %>% 
            group_by(category) %>% 
            mutate(total_population = sum(group_sum)) %>% 
            ungroup() %>% 
            mutate(percentage = group_sum / total_population *100) %>% 
            ggplot(mapping = aes(x = category, y = percentage, fill = type)) + 
            geom_col(position = 'stack') + 
            theme_bw() +
            theme(axis.text = element_text(angle =90, hjust = 1)) 
        )
    output$plot2 <- renderPlotly( 
        college_data %>% 
            filter(category != "Total Minority" & category != "Unknown") %>% 
            group_by(type, category) %>%
            summarise(group_sum = sum(enrollment)) %>% 
            ungroup() %>% 
            group_by(category) %>% 
            mutate(total_population = sum(group_sum)) %>% 
            ungroup() %>%  
            mutate(tuition_spent = ifelse(type == 'For Profit', group_sum*avgs_2014[1], 
                                          ifelse(type == "Private", group_sum * avgs_2014[2], group_sum * avgs_2014[3])
            ),
            proportion_of_money = ifelse(type == 'For Profit', (group_sum*avgs_2014[1]) / (total_population*avgs_2014[1]), 
                                         ifelse(type == "Private", (group_sum * avgs_2014[2]) / (total_population*avgs_2014[2]), 
                                                (group_sum * avgs_2014[3]) / (total_population*avgs_2014[3]))
            ) 
            ) %>% 
            arrange(desc(group_sum)) %>%
            ggplot( aes(x = category, y = proportion_of_money, fill = type)) + 
            geom_col(position = "dodge") + 
            coord_flip() + 
            theme_bw()
        )
    output$plot3 <- renderPlotly(  
        college_data %>% 
            group_by(type, year) %>% 
            summarise(full_tuition = median(total_price)) %>% 
            arrange(desc(full_tuition)) %>% 
            ggplot(aes(x = year, y = full_tuition)) + 
            geom_point(aes(color = type)) + 
            geom_line(aes(color = type)) +
            theme(axis.text = element_text(angle =0, hjust = 1)) + 
            coord_cartesian(xlim = c(2010,2018)) + 
            theme_bw()
        )
    output$plot4 <- renderPlotly( 
        college_data %>% 
            group_by(type, year) %>% 
            summarise(student_cost = median(net_cost)) %>% 
            arrange(desc(student_cost)) %>% 
            ggplot(aes(x = year, y = student_cost)) + 
            geom_point(aes(color = type)) + 
            geom_line(aes(color = type)) +
            theme(axis.text = element_text(angle =0, hjust = 1)) + 
            coord_cartesian(xlim = c(2010,2018)) + 
            theme_bw()
        )
    output$plot5 <- renderPlotly(
        college_data %>% 
            group_by(type, year) %>% 
            summarise(yearly_aid = median(total_price) - median(net_cost)) %>% 
            arrange(desc(yearly_aid)) %>% 
            ggplot(aes(x = year, y = yearly_aid)) + 
            geom_point(aes(color = type)) + 
            geom_line(aes(color = type)) +
            theme(axis.text = element_text(angle =0, hjust = 1)) + 
            coord_cartesian(xlim = c(2010,2018)) + 
            theme_bw()
    )
    output$plot6 <- renderPlot( 
        college_data %>% 
            group_by(year, income_lvl, type) %>% 
            summarise(med_cost = median(net_cost)) %>% 
            ggplot(aes(x = income_lvl, y = med_cost, color = type)) + 
            geom_point(size = 4) + 
            facet_wrap(~ year) + 
            theme_bw() +
            theme(axis.text = element_text(angle =45, hjust = 1))  
        )
    output$plot7 <- renderPlot( 
        college_data %>% 
            group_by(year, income_lvl, type) %>% 
            summarise(med_aid = median(total_price) - median(net_cost)) %>% 
            ggplot(aes(x = income_lvl, y = med_aid, color = type)) + 
            geom_point(size = 4) + 
            facet_wrap(~ year) + 
            theme_bw() +
            theme(axis.text = element_text(angle =45, hjust = 1)) 
        ) 
    
    output$plot8 <- renderPlotly( 
        college_data %>% 
            filter(year == 2018) %>% 
            group_by(income_lvl, type) %>% 
            summarise(med_cost = median(net_cost)) %>% 
            ggplot(aes(x = income_lvl, y = med_cost, color = type)) + 
            geom_point(size = 5) + 
            theme_bw() +
            theme(axis.text = element_text(angle =45, hjust = 1)) + 
            scale_color_brewer(type = "qual", palette = "Set1", limits = c('For Profit', 'Private', 'Public'))
        )
    
    output$plot9 <- renderPlotly( 
        college_data %>% 
            filter(year == 2018) %>% 
            group_by(income_lvl, type) %>% 
            summarise(med_aid = median(total_price) - median(net_cost)) %>% 
            ggplot(aes(x = income_lvl, y = med_aid, color = type)) + 
            geom_point(size = 5) + 
            theme_bw() +
            theme(axis.text = element_text(angle =45, hjust = 1)) + 
            scale_color_brewer(type = "qual", palette = "Set1", limits = c('For Profit', 'Private', 'Public'))
        )
}

    
shinyApp(ui, server)