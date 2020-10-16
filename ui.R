#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


navbarPage("EDA of College Tuition", 
           tabPanel("Trends over the Years", 
                    sidebarPanel(
                      selectizeInput(inputId = 'name', 
                                     label = 'Name of School', 
                                     choices = unique(college_data$name), 
                                     options= list(maxOptions = 2000))
                    ),
                    mainPanel(plotOutput("tuition"))
                    
                    )
           )

