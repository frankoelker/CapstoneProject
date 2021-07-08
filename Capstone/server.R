#
# This is the server logic of a Shiny web application for the
# Coursera Capstone Project. 
# You can run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tm)
library(stringr)
library(stringi)

quadragram <- readRDS("quadragram.RData");
triagram <- readRDS("triagram.RData");
bigram <- readRDS("bigram.RData");
unigram <- readRDS("unigram.RData")
info <- "default"

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    output$clean <- renderText({
        clean(input$inputString)
    })
    output$predict <- renderText({
        result <- predict(clean(input$inputString))
        output$information <- renderText({info})
        result
    })
    output$unigram <- renderPrint({
        unigram
    })
    output$unigramdata <- DT::renderDataTable(
        DT::datatable(unigram, options = list(pageLength = 25))
    )
    output$bigramdata <- DT::renderDataTable(
        DT::datatable(bigram, options = list(pageLength = 25))
    )
    output$triagramdata <- DT::renderDataTable(
        DT::datatable(triagram, options = list(pageLength = 25))
    )
    output$quadragramdata <- DT::renderDataTable(
        DT::datatable(quadragram, options = list(pageLength = 25))
    )
})

predict <- function(x) {
    split <- strsplit(x, " ")[[1]] 
    
    if (length(split) >= 3) {
        split <- tail(split, 3)
        if (identical(character(0),
                      head(quadragram[
                          quadragram$unigram == split[1] & 
                          quadragram$bigram == split[2] & 
                          quadragram$triagram == split[3], 4],1))) {
            predict(paste(split[2], split[3], sep=" "))
        } else {
            info <<- "quadragram prediction"; 
            head(quadragram[
                quadragram$unigram == split[1] & 
                quadragram$bigram == split[2] & 
                quadragram$triagram == split[3], 4],1)
        }
    } else if (length(split) == 2){
        split <- tail(split, 2)
        if (identical(character(0),
                      head(triagram[
                          triagram$unigram == split[1] & 
                          triagram$bigram == split[2], 3],1))) {
            predict(split[2])
        } else {
            info <<- "triagram prediction"; 
            head(triagram[triagram$unigram == split[1] & 
                          triagram$bigram == split[2], 3],1)
        }
    } else if (length(split) == 1) {
        split <- tail(split, 1)
        if (identical(character(0),
                      head(bigram[bigram$unigram == split[1], 2],1))) {
            info <<- "Can`t predict a word.";
            head("sorry, no prediction possible",1)
        }
        else {
            info <<- "bigram prediction"; 
            head(bigram[bigram$unigram == split[1],2],1)
        }
    }
}

clean <- function(x) {
    cleaninput <- removeNumbers(removePunctuation(tolower(x)))
    cleaninput
}

