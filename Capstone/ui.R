#
# This is the user-interface definition of a Shiny web application for the
# Coursera Capstone Project. 
# You can run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application 
shinyUI(fluidPage(

    # Application title
    titlePanel("Capstone Project - Get a predicted next word"),

        navbarPage("Navigation",
            tabPanel("Prediction",
                sidebarLayout(
                    sidebarPanel(
                        textInput("inputString","Enter your text",value = ""),
                        h4("Informations"),
                        verbatimTextOutput('information'),
                        h4("Used for prediction"),
                        textOutput('clean')
                    ),
                    mainPanel(
                        h2("Predicted word"),
                        textOutput('predict'),
                        tags$head(tags$style("#predict{color: red;
                                 font-size: 25px;
                                 font-style: italic;
                                 }")
                        )
                    )
                )
            ),
            navbarMenu("Most common N-Grams",
                       tabPanel("Unigram",
                                img(src='ngram1.png')),
                       tabPanel("Bigram",
                                img(src='ngram2.png')),
                       tabPanel("Triagram",
                                img(src='ngram3.png')),
                       tabPanel("Quadragram",
                                img(src='ngram4.png'))
            ),
            navbarMenu("Used Data",
                       tabPanel("Unigram",
                                DT::dataTableOutput('unigramdata')),
                       tabPanel("Bigram",
                                DT::dataTableOutput('bigramdata')),
                       tabPanel("Triagram",
                                DT::dataTableOutput('triagramdata')),
                       tabPanel("Quadragram",
                                DT::dataTableOutput('quadragramdata'))
            ),
            tabPanel("How to use",
                 h3("About this app"),
                 br(),
                 div("This is a Shiny app that uses a text
                     prediction algorithm to predict the next word
                     based on text entered by a user."),
                 br(),
                 div("1. Choose Prediction in the Navigation."),
                 div("2. Enter your text"),
                 div("3. The predicted next word is displayed."),
                 br(),
                 div("If no prediction is possible based on the database, 
                     you`ll get the result 'sorry, no prediction possible'."),
                 br(),
                 h3("Further Informations"),
                 br(),
                 div("You`ll get further informations about the Prediction:"),
                 div("1. Which ngram is used to predict"),
                 div("2. Some characters need to be removed from the input 
                     to make a prediction. therefore, the possibly changed 
                     input is displayed"),
                 br(),
                 div("You can also view the most common combinations via 
                     selcting 'Most common N-grams'."),
                 br(),
                 div("Last but not least, you can look at the data used by 
                     selecting 'Used Data'")
            ),
            tabPanel("About this project",
                 div("This app is the final work for the",
                    a(target = "_blank", 
                    href="https://www.coursera.org/specializations/jhu-data-science", 
                    "Coursera Data Science Specialization Course")),
                 br(),
                 div("Components of this final work:"),
                 div("1. This app", a(target = "_blank", 
                     href="https://frankoelker.shinyapps.io/Capstone/", 
                     "Capstone Shiny App")),
                 div("2. This slide deck", a(target = "_blank", 
                     href="https://www.coursera.org/specializations/jhu-data-science", 
                     "Data Science Specialization"))
            )
        )
    )
) 