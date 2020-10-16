#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- 
    
    dashboardPage(
        dashboardHeader(title = "EDA of College of Data"),
        dashboardSidebar(
            sidebarUserPanel("Author : Brian Perez"), 
            
            sidebarMenu( 
                menuItem("Overview", 
                         tabName = "intro", icon = icon("book-reader"))
                )
            
        ),
        
        
#########        
        dashboardBody(
            tabItem(tabName = "intro", h2("General Trends in the United States"), 
                fluidRow( 
                    box(plotOutput("plot1"), width = 4), 
                    box(plotOutput("plot2"), width = 6)
                    )
                )
        )
)














# Define server logic required to draw a histogram
server <- function(input, output) {
    output$plot1 <- renderPlot( 
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
            theme(axis.text = element_text(angle =90, hjust = 1)) 
        )
    output$plot2 <- renderPlot( 
        combined_data %>% 
            filter(category != "Total Minority" & category != "Unknown") %>% 
            group_by(type, category) %>%
            summarise(group_sum = sum(enrollment)) %>% 
            ungroup() %>% 
            group_by(category) %>% 
            mutate(total_population = sum(group_sum)) %>% 
            ungroup() %>%  
            mutate(tuition_spent = ifelse(type == 'For Profit', group_sum*avgs_2014[1] / 10**9, 
                                          ifelse(type == "Private", group_sum * avgs_2014[2] / 10**9, group_sum * avgs_2014[3] / 10**9)
            ),
            proportion_of_money = ifelse(type == 'For Profit', (group_sum*avgs_2014[1]) / (total_population*avgs_2014[1]), 
                                         ifelse(type == "Private", (group_sum * avgs_2014[2]) / (total_population*avgs_2014[2]), 
                                                (group_sum * avgs_2014[3]) / (total_population*avgs_2014[3]))
            ) 
            ) %>% 
            arrange(desc(group_sum)) %>%
            ggplot( aes(x = category, y = proportion_of_money, fill = type)) + 
            geom_col(position = "dodge") + 
            coord_flip()
        
        
        
        
        
        )
}

    
shinyApp(ui, server)