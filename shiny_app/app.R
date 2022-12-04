

library("shiny")
library("readxl")
library("dplyr")
library("ggplot2")

#Preprocessing

df <- read_excel("foreign_students.xlsx")
colnames(df) <- c("university_name","university_type","province","nationality","man_student_count","woman_student_count","total_student_count")
df$man_student_count <- as.integer(df$man_student_count)
df$woman_student_count <- as.integer(df$woman_student_count)
df$total_student_count <- as.integer(df$total_student_count)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Foreign Students"),

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
          selectInput("agg_column",
                      "Aggregation Column",
                      choices = c("university_type","province","university_name"),
                      multiple = FALSE),
            selectInput("nationality",
                        "Nationalities",
                        choices = unique(df$nationality),
                        multiple = TRUE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
        if ( input$agg_column == 'university_type') {
          # generate bins based on input$bins from ui.R
          data <- df%>%filter(`nationality` %in% input$nationality)%>%group_by(nationality,university_type)%>%summarise(count=sum(total_student_count))
          
          # draw the histogram with the specified number of bins
          ggplot(data,aes(x = data$nationality, y =data$count, fill = data$university_type)) +
            geom_bar(stat = "identity", position = "dodge") + ylab("Student Count") + labs(x="Nationality",y="Student Count" , fill = "University Type")
        } else if ( input$agg_column == 'province') {
          # generate bins based on input$bins from ui.R
          data <- df%>%filter(`nationality` %in% input$nationality)%>%
            group_by(nationality,province)%>%
            summarise(count=sum(total_student_count))%>%
            arrange(desc(count))%>%
            top_n(10)
          
          # draw the histogram with the specified number of bins
          ggplot(data,aes(x = data$nationality, y =data$count, fill = data$province)) +
            geom_bar(stat = "identity", position = "dodge") + ylab("Student Count") + labs(x="Nationality",y="Student Count" , fill = "Province")
        } else if ( input$agg_column == 'university_name') {
          data <- df%>%filter(`nationality` %in% input$nationality)%>%
            group_by(nationality,university_name)%>%
            summarise(count=sum(total_student_count))%>%
            arrange(desc(count))%>%
            top_n(10)
          
          # draw the histogram with the specified number of bins
          ggplot(data,aes(x = data$nationality, y =data$count, fill = data$university_name)) +
            geom_bar(stat = "identity", position = "dodge") + ylab("Student Count") + labs(x="Nationality",y="Student Count" , fill = "University Name")
        }

        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
