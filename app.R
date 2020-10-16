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
                         menuSubItem("Countrywide Yearly Trends", tabName = "subitem2"))
                )
            
        ),
        
        
#########        
        dashboardBody( 
            tabItems(      
                tabItem(tabName = "subitem1", h2("General Trends in the United States"), 
                        fluidRow( 
                            box(plotlyOutput("plot1"), width = 4), 
                            box(plotlyOutput("plot2"), width = 6)
                        )
                ),  
                tabItem(tabName = "subitem2", h2("Trends in the United States"), 
                        fluidRow( 
                            box(plotlyOutput("plot3"), width = 4), 
                            box(plotlyOutput("plot4"), width = 6)
                            ))
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
    
}

    
shinyApp(ui, server)