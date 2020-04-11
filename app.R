#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
    
#libraries
    library(shiny)
    library(titanic)
    library(caret)
    library(rpart)
# read dataset
    df<-titanic_train
    test<-titanic_test
    i<-vector()
    txt <- names(df)

# set dummy value
    df$Sex[df$Sex=="male"]<-0
    df$Sex[df$Sex=="female"]<-1  
    df$Sex<-as.numeric(df$Sex)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Decision tree with titanic data set"),
    h3("What was the improtatnt factors to be suvived in the taitanic acdident?"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4("Select the factor which you want to take into the algorism."),
            checkboxInput("checkbox1", label = "Age", value = TRUE), 
            checkboxInput("checkbox2", label = "Sex", value = TRUE), 
            checkboxInput("checkbox3", label = "Fare", value = TRUE), 
            checkboxInput("checkbox4", label = "Pclass", value = TRUE),
            checkboxInput("checkbox5", label = "SibSp", value = TRUE),
            checkboxInput("checkbox6", label = "Parch", value = TRUE),
            h4("Next, push the submit bottom below, it takes a few second."),
            submitButton("submit"),
                    ),

        # Show a plot of the generated graph
        mainPanel(
           plotOutput("graph1"),
           h4("In the bottom box, the logical valuve indicats that 0: dead 1:survived "),
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$graph1 <- renderPlot({ 
    # set searching itmes
        i<-grep("Survived",txt)
        {if (input$checkbox1==TRUE) i<-append(i,grep("Age", txt))  }
        {if (input$checkbox2==TRUE) i<-append(i,grep("Sex", txt))  }
        {if (input$checkbox3==TRUE) i<-append(i,grep("Fare", txt))  }
        {if (input$checkbox4==TRUE) i<-append(i,grep("Pclass", txt))  }
        {if (input$checkbox5==TRUE) i<-append(i,grep("SibSp", txt))  }
        {if (input$checkbox6==TRUE) i<-append(i,grep("Parch", txt))  }
            df2<-df[,i]
        
    # remove the NA value
        df3<-subset(df2,complete.cases(df))

     # remove the zero convariance
        preObj<-preProcess(df3,method = c("center","scale"))
        nsv<-nearZeroVar(df3,saveMetrics = T)
        nsv2<-subset(nsv,zsv="FALSE")
        df4<-df3[,rownames(nsv2)]
        
    # Decision tree
        decisionTreeMod1 <- rpart(Survived ~ ., data=df4, method="class",cp=0.02)
        fancyRpartPlot(decisionTreeMod1)
        
        
                            })
         
                                    
}
    


# Run the application 
shinyApp(ui = ui, server = server)
