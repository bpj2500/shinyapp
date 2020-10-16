#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

function(input, output) { 
    output$tuition <- renderPlot( 
        college_data %>% 
        filter(name == input$name) %>%
        group_by(year) %>% 
        summarise(total_tuition =  mean(total_price),
                      student_cost = mean(net_cost), 
                      financial_aid = mean(total_price) - mean(net_cost)) %>% 
        pivot_longer(cols = c(total_tuition, student_cost, financial_aid), 
                     names_to = "Finance", 
                     values_to = "Money") %>%     
        ggplot(aes(x = year, y = Money, fill = Finance)) + 
        geom_col(position = 'dodge') + 
        coord_flip()
        )
    }