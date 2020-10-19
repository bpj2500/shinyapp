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
library(usmap)

college_data <- read_csv("college_data.csv") 
avgs_2014 <- college_data %>% 
    filter(year == 2014) %>% 
    group_by(type) %>% 
    summarise(yearly_cost = median(net_cost)) %>% 
    select(yearly_cost) %>% 
    unlist() %>% 
    unname()






# Define UI for application that draws a histogram
ui <- 
    
    dashboardPage(
        dashboardHeader(title = "EDA of College Tuition"),
        dashboardSidebar(
            sidebarUserPanel("Author : Brian Perez"), 
            
            sidebarMenu(
                menuItem("Overview", 
                         tabName = "overview"),
                menuItem("General Trends in the USA", 
                         tabName = "intro", icon = icon("book-reader"),
                         menuSubItem("Countrywide Yearly Trends", tabName = "subitem1"), 
                         menuSubItem("Examining Income Level", tabName = "subitem2"), 
                         menuSubItem("Demographics in 2014", tabName = "subitem3"), 
                         menuSubItem("Distribution of University Types", tabName = "subitem4")), 
                menuItem("Interactive Tools",  
                         tabName = "Tools", icon = icon("book-reader")
                         )
                )
            
        ),
        
#########        
        dashboardBody( 
            tabItems(  
                tabItem(tabName = "overview", 
                      fluidRow(   
                          column( 
                              width = 8, offset = 2, align = "center", 
                              box(width = 12, h1(tags$b("Exploratory Data Analysis of College Tuition, Financial Support, & Group Demographics")), 
                                  br(), 
                                  br(), 
                                  img(src = "college_campus2.jpg"), 
                                  br(), 
                                  br(), 
                                  h4("Throughout their history, colleges and universities within the United States have proven to be ladders of economic and social mobility. More often than naught, the pursuance of a bachelor's degree, or other higher accreditations, at these institutions grants students a wide range of unique career and job opportunities that they otherwise might not have had access to. In fact, for many professions and industries, a bachelor's degree is now seen as a minimum requirement for many entry-level positions, and further career advancement may also be dependent on additional post-baccalaureate education. Because of this, more people than ever before are enrolling in college and universities for the hope of seeking education and training in their potential professions; however, the cost of higher education is also higher than it has ever been with many students often using student loans to help supplement their tuition payments. Given the environment as it currently is, understanding how tuition costs have changed in the recent years and how certain demographic groups are moving throughout the college education system may be key to informing education policies, informing businesses and industries on where/how they should search for talent, and also how individuals should weigh the costs of pursing a college degree.  
                                    "), 
                                  br(), 
                                  h4("The goal of the project is to characterize some of trends regarding college tuition prices over the years and how they relate to students of various income levels and historically underrepresented demographic groups in order to gain insights and generate further questions in assessing the economics of colleges and universities."
                                     ),
                                  br(), 
                                  h1("Conclusions & Further Question"), 
                                  br(), 
                                  br(), 
                                  h4("Based on the data present thus far, in examining the general trends occurring across university campuses; the overall price of college has increased over the past eight years. The increase is most notable in private institution, which in the span of eight years saw a tuition rise of nearly $10000 dollars. For profit and public institutions have seen much slower tuition rises. The net costs for students are also substantially among higher among private and for-profit schools. Unlike public and for-profit institutions, private institutions provide substantially more financial support to students than the other two institution types. However, despite that, public institutions overall bear the lowest cost for attendance. Even across income levels, although private institutions give significantly more financial support to lower income students than public or for-profit institutions. However, despite the relatively larger financial support, in 2014, across various demographic groups, over 70% of the student population of these groups enrolled themselves in public institutions. Moreover, a large majority of tuition dollars are being spent in public institutions rather than private or for-profit institutions.  Overall, what these data indicate is that public institutions seem  drawing in larger swaths of the populations, likely because of the lower economic costs. As a result, public institutions may serve as the primary means through which more diverse and economically disadvantaged groups obtain higher degrees."),
                                  br(), 
                                  br(), 
                                  h4("The variables for this dataset are very limiting. A further investigation on this topic could look at information on incomes/salaries before and after graduating college would help examine outcomes along with information on student majors. Having information on the economic data on the students would also help determine what economic classes compose the student bodies at these institutions."),
                                  h5("Presentation by Brian Perez")
                                  )
                              
                              )
                          )
                        
                        ),
                
                
                tabItem(tabName = "subitem1", h2("Yearly Trends in Tuition Prices, Cost, & Aid"), 
                        fluidRow( 
                            box(plotlyOutput("plot3"), width = 5), 
                            box(plotlyOutput("plot4"), width = 5), 
                            box(plotlyOutput("plot5"), width = 5), 
                            box(h4("The following line graphs show the median total price, the median net cost, and the median financial support of universities in the United States from 2010 to 2018. Each of the graphs has three lines each depicting a different institution type: a for profit schools, private schools, and public schools. Overall, private schools have the highest total tuition and financial support. Furthermore, costs of private schools have increased the most over the whole eight years. For profit schools, have higher student costs for most years compared to the other two school types. Public schools overall have the lowest tuition prices, student cost, and financial support. ", 
                                   width = 2))
                        )
                ),  
                tabItem(tabName = "subitem2", h2("Variations in Costs by Income Level"), 
                        fluidRow( 
                            box(plotOutput("plot6"), width = 5), 
                            box(plotOutput("plot7"), width = 5), 
                            box(plotlyOutput("plot8"), width = 5), 
                            box(plotlyOutput("plot9"), width = 5), 
                            box(h4(" The top two dot plots show the median net cost, and financial support of universities in the United States by student income level faceted from 2010 to 2018. As the years increase, the net cost and aid increased overall; however, based on the two dot plots below that are zoom-ins of the year 2018, moving up income levels, the net cost increased and the financial support decreased, meaning that students with low income levels receive more financial support than higher income students. ", 
                                   width = 5))
                            )
                ), 
                tabItem(tabName = "subitem3", h2("College Demographics in 2014"), 
                        fluidRow( 
                            box(plotlyOutput("plot1"), width = 6),
                            box(plotlyOutput("plot2"), width = 6), 
                            box(h4("The first bar plot is a stacked bar plot that shows the proportion of students for each demographic group that enrolled in type of school in the year 2014. The bar plot shows that across, all groups, the majority of students enrolled in public schools. The second bar plot shows the proportion of tuition dollars spent by each demographic group. Similar to the last graph, the majority of tuition dollars spent by each demographic group went largely to public institutions. ", 
                                   width = 5))
                            )
                        ),
                tabItem(tabName = "subitem4", h2("Map of the Number of Universities across the US"), 
                        fluidRow( 
                            box(plotlyOutput("plot10"), width = 5), 
                            box(plotlyOutput("plot11"), width = 5), 
                            box(h4("The two map sets show the distribution of private and public universities throughout the United  States. Across the United States, there are more public universities than there are private universities. More of the private universities in the United States lie along the east coast. Except for Texas and California, there are comparatively fewer private institutions in the South and western coast. Public institutions are more dispersed throughout the country.", 
                                   width = 5))
                            )
                        ), 
                
                tabItem(tabName = "Tools", h2("Plots"),  
                        fluidRow( 
                            box(plotOutput("plot12"), width = 6), 
                            box(plotlyOutput("plot13"), width = 6), 
                            box(selectizeInput(inputId = "State", 
                                               label = "State Name", 
                                               choices = unique(college_data$state)), width = 4))
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
            theme(axis.text = element_text(hjust = 1)) + 
            coord_flip() + 
            xlab("Demographic Group") + 
            ylab("Percent") + 
            labs(fill = "School Type")
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
            theme_bw() + 
            xlab("Demographic Group") + 
            ylab("Percent") + 
            labs(fill = "School Type")
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
            theme_bw() + 
            xlab("Year") + 
            ylab("Median Total Tuition") + 
            ggtitle("Total Tuition Prices by School Type") + 
            labs(color = "School Type") + 
            scale_color_brewer(type = "qual", palette = "Set1", limits = c('For Profit', 'Private', 'Public')) 
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
            theme_bw() + 
            xlab("Year") + 
            ylab("Median Student Cost") + 
            ggtitle("Student Cost by School Type") + 
            labs(color = "School Type") + 
            scale_color_brewer(type = "qual", palette = "Set1", limits = c('For Profit', 'Private', 'Public'))
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
            theme_bw() + 
            xlab("Year") + 
            ylab("Median Financial Support") + 
            ggtitle("Financial Support School Type") + 
            labs(color = "School Type") + 
            scale_color_brewer(type = "qual", palette = "Set1", limits = c('For Profit', 'Private', 'Public'))
    )
    output$plot6 <- renderPlot( 
        college_data %>% 
            group_by(year, income_lvl, type) %>% 
            summarise(med_cost = median(net_cost)) %>% 
            ggplot(aes(x = income_lvl, y = med_cost, color = type)) + 
            geom_point(size = 4) + 
            facet_wrap(~ year) + 
            theme_bw() +
            theme(axis.text = element_text(angle =45, hjust = 1)) +  
            xlab("Year") + 
            ylab("Median Student Cost") + 
            ggtitle("Student Cost by School Type") + 
            labs(color = "School Type") + 
            scale_color_brewer(type = "qual", palette = "Set1", limits = c('For Profit', 'Private', 'Public'))
        )
    output$plot7 <- renderPlot( 
        college_data %>% 
            group_by(year, income_lvl, type) %>% 
            summarise(med_aid = median(total_price) - median(net_cost)) %>% 
            ggplot(aes(x = income_lvl, y = med_aid, color = type)) + 
            geom_point(size = 4) + 
            facet_wrap(~ year) + 
            theme_bw() +
            theme(axis.text = element_text(angle =45, hjust = 1)) +  
            xlab("Year") + 
            ylab("Median Financial Support") + 
            ggtitle("Financial Support School Type") + 
            scale_color_brewer(type = "qual", palette = "Set1", limits = c('For Profit', 'Private', 'Public'))
        ) 
    
    output$plot8 <- renderPlotly( 
        college_data %>% 
            filter(year == 2018) %>% 
            group_by(income_lvl, type) %>% 
            summarise(med_cost = median(net_cost)) %>% 
            ggplot(aes(x = income_lvl, y = med_cost, color = type)) + 
            geom_point(size = 4) + 
            theme_bw() +
            theme(axis.text = element_text(angle =45, hjust = 1)) + 
            scale_color_brewer(type = "qual", palette = "Set1", limits = c('For Profit', 'Private', 'Public')) + 
            xlab("Income Level") + 
            ylab("Median Student Cost") + 
            ggtitle("Student Cost by Income Level in 2018")
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
            scale_color_brewer(type = "qual", palette = "Set1", limits = c('For Profit', 'Private', 'Public')) + 
            xlab("Income Level") + 
            ylab("Median Financial Support") + 
            ggtitle("Financial Support by Income Level in 2018")
        )
    
    output$plot10 <- renderPlotly({ 
        priv <-college_data %>% 
            group_by(year, state, type) %>% 
            summarise(university_totals = n_distinct(name)) %>% 
            ungroup() %>%
            filter(year == 2018) %>% 
            pivot_wider(names_from = type, values_from = university_totals) %>% 
            select(state, Private) 
        
            plot_usmap(data = priv, values = "Private") + 
            scale_fill_continuous(name = "Number of Universities", 
                                  low = "white", high = "red") + 
            theme(legend.position = "right") + 
            ggtitle("Number of Private Universities in America")
        })  
    
    output$plot11 <- renderPlotly({ 
        pub <- college_data %>% 
            group_by(year, state, type) %>% 
            summarise(university_totals = n_distinct(name)) %>% 
            ungroup() %>%
            filter(year == 2018) %>% 
            pivot_wider(names_from = type, values_from = university_totals) %>% 
            select(state, Public) 
        
            plot_usmap(data = pub, values = "Public") + 
            scale_fill_continuous(name = "Number of Universities", 
                                  low = "white", high = "red") + 
            theme(legend.position = "right") + 
            ggtitle("Number of Public Universities in America")
        })
    
    output$plot12 <- renderPlot(  
        college_data %>% 
            filter(state == input$State & total_price <= 70000) %>% 
            group_by(year, type) %>% 
            summarise(total_tuition =  median(total_price),
                      student_cost = median(net_cost), 
                      financial_aid = total_tuition - student_cost) %>% 
            pivot_longer(cols = c(total_tuition, student_cost, financial_aid), 
                         names_to = "Finance", 
                         values_to = "Money") %>%     
            ggplot(aes(x = year, y = Money, color = Finance)) + 
            geom_point(size = 4) + 
            geom_line() + 
            theme_bw() + 
            facet_wrap(~ type) + 
            ggtitle("Plots of Yearly Rates by State and Institution Type") + 
            xlab("Annual Year")
        
        )
    
    output$plot13 <- renderPlotly(  
        college_data %>% 
            filter(category != "Total Minority" & category != "Unknown" & year == 2014 & state == input$State) %>% 
            group_by(type, category) %>%
            summarise(group_sum = sum(enrollment)) %>% 
            ungroup() %>% 
            group_by(category) %>% 
            mutate(total_population = sum(group_sum), 
                   group_proportion = group_sum / total_population) %>% 
            ggplot(aes(x = category, y = group_proportion)) + 
            geom_col(aes(fill = type)) + 
            coord_flip() + 
            theme_bw()
        )
}

    
shinyApp(ui, server)