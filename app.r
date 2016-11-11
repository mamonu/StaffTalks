library(bubbles)
library(ggplot2)
library(shiny)
library(syuzhet)
library(dplyr)
library(ggradar)
library(shinydashboard)
library(scales)
library(grid)

textareaInput <- function(inputID, label, value="", rows=10) {
  HTML(paste0('<div class="form-group shiny-input-container">
              <label for="', inputID, '">', label,'</label>
              <textarea class="form-control" id="', inputID, 
              '" rows="', rows, '">', value, '</textarea></div>'))
}



server <- function(input, output) {
 
  
  uwordcount <- function () {
    
    words.list<-strsplit(input$text, "\\W+", perl=TRUE)
    words.vector<-unlist(words.list)
    wordcount<-nrow(as.data.frame(unique(words.vector)))
  }
  
  wordcount <- function () {
    
    words.list<-strsplit(input$text, "\\W+", perl=TRUE)
    words.vector<-unlist(words.list)
    wordcount<-nrow(as.data.frame(words.vector))
  }
  

  sentencecount <- function() {
    sentencecount <- nrow(as.data.frame(get_sentences(input$text)))
  }
  
  
  polaritycount <- function() {
    #liking <-get_nrc_sentiment(desc$V6)
    pnum <- get_nrc_sentiment(input$text)
    pnum$negative <- (- pnum$negative)
    pnumz <- data.frame(colSums(pnum[,c(9:10)]))
    polaritcount<-colSums(pnumz)
  }
  
  
   output$distPlot <- renderPlot({

    frustrations <- get_nrc_sentiment(input$text)
    
    
    sentimentTotals <- data.frame(colSums(frustrations[,c(1:8)]))
    names(sentimentTotals) <- "count"
    sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
    rownames(sentimentTotals) <- NULL
    
    ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
      geom_bar(aes(fill = sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiment") + ylab("Total Count") + ggtitle("Sentiment Score")
  
    
  })
  
  
  output$plcount <- renderValueBox({
     valueBox(
       value = polaritycount(),
       subtitle = "polarity:",
       icon = icon("exclamation-triangle"),
       color = if (polaritycount() < 0 ) "red" else "aqua"
     )
   })
  
  output$count <- renderValueBox({
    valueBox(
      value = sentencecount(),
      subtitle = "# of sentences:",
      icon = icon("list")
    )
  })
  
  output$uwcount <- renderValueBox({
    valueBox(
      value = uwordcount(),
      subtitle = "# of unique words:",
      icon = icon("list")
    )
  })
  
  output$wcount <- renderValueBox({
    valueBox(
      value = wordcount(),
      subtitle = "# of  words:",
      icon = icon("list")
    )
  })

  output$distPlotNarrative <- renderPlot({
  
    
    
    #frustrations <- get_nrc_sentiment(input$text)

    
    s_v <- get_sentences(input$text)
    s_v_sentiment <- get_sentiment(s_v)
    plot(
      s_v_sentiment, 
      type="b", 
      main="Plot Trajectory", 
      xlab = "Narrative Time (sentence)", 
      ylab= "Emotional Valence (pos/neg)"
    )
    
   
  
    
  })
  
    
  output$distPlotPolarity <- renderPlot({
    
    
    
    
    #liking <-get_nrc_sentiment(desc$V6)
    polarity <- get_nrc_sentiment(input$text)
    
    
    sentimentTotals <- data.frame(colSums(polarity[,c(9:10)]))
    
    names(sentimentTotals) <- "count"
    sentimentTotals <- cbind("sentiment" = rownames(sentimentTotals), sentimentTotals)
    rownames(sentimentTotals) <- NULL
    
    ggplot(data = sentimentTotals, aes(x = sentiment, y = count)) +
      geom_bar(aes(fill = sentiment), stat = "identity") +
      theme(legend.position = "none") +
      xlab("Sentiment") + ylab("Total Count") + ggtitle("Polarity")
    
    
    
    
    
    
    # hist(rnorm(input$obs), col = 'darkgray', border = 'white')
    
    
    
  })
  
  
  
  
  output$LDA <-  renderFormattable      ({
    
    
    
    inFile <- input$file1
    
    #if (is.null(inFile))
    # return (NULL)
    
    #read.csv(inFile$datapath, header = input$header,
    #         sep = input$sep, quote = input$quote)
    
    data <-read.csv2(inFile$datapath, sep="\n", stringsAsFactors=FALSE,header = FALSE)
    
    
    
    
    
    #######
    
    desc <- gsub("'", "", data)  # remove apostrophes
    desc <- gsub("[[:punct:]]", "", desc)  # replace punctuation with space
    desc <- gsub("[[:cntrl:]]", "", desc)  # replace control characters with space
    desc <- gsub("[[:digit:]]+", "", desc) # remove numbers
    
    desc <- gsub("^[[:space:]]+", "", desc) # remove whitespace at beginning of documents
    desc <- gsub("[[:space:]]+$", "", desc) # remove whitespace at end of documents
    desc <- tolower(desc)  #
    doc.list <- strsplit(desc, "[[:space:]]+")
    #stem.list <- lapply (doc.list, wordStem )
    
    
    
    
    # compute the table of terms:
    term.table <- table(unlist(doc.list))
    term.table <- sort(term.table, decreasing = TRUE)
    
    # remove terms that are stop words or occur fewer than 5 times:
    stop_words <- stopwords("SMART")
    del <- names(term.table) %in% stop_words | term.table < 5 
    
    term.table <- term.table[!del]
    
    
    
    
    
    
    
    vocab <-  (names(term.table))
    
    
    
    
    
    
    get.terms <- function(x,vocab) {
      index <- match(x, vocab)
      index <- index[!is.na(index)]
      rbind(as.integer(index - 1), as.integer(rep(1, length(index))))
    }
    
    ###### simple laplly  
    
    
    documents <- lapply(doc.list, get.terms,vocab)
    
    
    
    
    
    
    # Compute some statistics related to the data set:
    D <- length(documents)  # number of documents  
    W <- length(vocab)  # number of terms in the vocab  
    doc.length <- sapply(documents, function(x) sum(x[2, ])) 
    N <- sum(doc.length)  
    term.frequency <- as.integer(term.table)
    ########
    
    
    
    
    
    # MCMC and model tuning parameters:
    K <- input$clust
    G <- input$iter
    
    alpha <- input$ialpha
    eta <- input$ieta
    
    inptop <-input$top
    
    # Fit the model:
    
    set.seed(357)
    #t1 <- Sys.time()
    
    fit <- lda.collapsed.gibbs.sampler(documents = documents, K = K, vocab = vocab,
                                       num.iterations = G, alpha = alpha,
                                       eta = eta, initial = NULL, burnin = 0,
                                       compute.log.likelihood = TRUE)
    
    
    #t2 <- Sys.time()
    #t2 - t1  
    
    
    
    topwords <- (top.topic.words(fit$topics, inptop, by.score=TRUE))
    tw<-data.frame (topwords)
    
    
    
    
    
    formattable(tw,list())
    
    # 
    # theta <- t(apply(fit$document_sums + alpha, 2, function(x) x/sum(x)))
    # phi <- t(apply(t(fit$topics) + eta, 2, function(x) x/sum(x)))
    # 
    # lda_description <- list(phi = phi,
    #                      theta = theta,
    #                      doc.length = doc.length,
    #                      vocab = vocab,
    #                      term.frequency = term.frequency)
    # 
    # 
    # json <- createJSON(phi = lda_description$phi, 
    #                    theta = lda_description$theta, 
    #                    doc.length = lda_description$doc.length, 
    #                    vocab = lda_description$vocab, 
    #                    term.frequency = lda_description$term.frequency)
    # 
    # 
    # 
    # #export the json file! 
    # #write(json, "exported.json")
    # 
    # serVis(json, out.dir = 'vis2', open.browser = TRUE)
    
    
    
  })
  
  
  
  
}




#[----------------- ui ---------------------------]

ui <- dashboardPage( dashboardHeader(title = "Sentiment Analysis based on NRC lexicon", 
                                     titleWidth = 650,disable = TRUE),
                     dashboardSidebar(disable = TRUE),
    dashboardBody(
      fluidRow( 
      valueBoxOutput("count",width=3),valueBoxOutput("wcount",width=3),
      valueBoxOutput("uwcount",width=3),
      valueBoxOutput("plcount",width=3)
      
      
      ),
      fluidRow(
      tags$head(tags$style(HTML('
      .main-header .logo {
                                font-family: "Georgia", Times, "Times New Roman", serif;
                                font-weight: bold;
                                font-size: 24px;
                                }
                                '))),


      box(title = "Sentiment ", status = "warning", solidHeader = TRUE,
          collapsible = TRUE,
        plotOutput("distPlot")),

  
      box( title = "Narrative", status = "warning", solidHeader = TRUE,
           collapsible = TRUE,
      plotOutput("distPlotNarrative")),
      
      box( title = "Polarity", status = "warning", solidHeader = TRUE,
           collapsible = TRUE,
           plotOutput("distPlotPolarity")),
      
      box( title = "Text Input", status = "primary", solidHeader = TRUE,
           collapsible = FALSE,  textInput("text", "Text:",
                                           "My birthday is coming soon. I like cats.")),
      box(title = "Sentiment ", status = "success", solidHeader = TRUE,
          collapsible = TRUE,
          textareaInput("localtext","text"))
      
      
      
      
      )
    
    
   
    
    
  )
)

shinyApp(ui = ui, server = server)