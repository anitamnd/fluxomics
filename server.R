library(ggplot2)
library(dplyr)

function(input, output, session) {
  # Load example data
  #TODO add "load example" button instead
  ex_dat <- read.csv("June2023palbolabelingmerged2.csv")
  ex_seq <- read.csv("Palbo sequence.csv")
  
  updateSelectInput(session, "metabolite", choices = unique(ex_dat[, 'Analyte']))
  updateSelectInput(session, "sample", choices = levels(factor(ex_dat[, 'Analysis'])))
  updateSelectInput(session, "metabolite1", choices = unique(ex_dat[, 'Analyte']))
  updateSelectInput(session, "metabolite2", choices = unique(ex_dat[, 'Analyte']))
  updateSelectInput(session, "time", choices = sort(unique(
    ex_seq[, 'time'], incomparables = NA, na.rm = TRUE
  )))
  
  # Reactive values
  rv <- reactiveValues(dat = NULL,
                       seq = NULL,
                       selected = NA)
  
  ###
  
  observeEvent(input$in_file, {
    rv$dat <-
      read.csv(
        input$in_file$datapath,
        header = 1,
        stringsAsFactors = F,
        check.names = FALSE
      )
    
    updateSelectInput(session, "metabolite", choices = unique(rv$dat[, 'Analyte']))
    updateSelectInput(session, "sample", choices = levels(factor(rv$dat[, 'Analysis'])))
    updateSelectInput(session, "metabolite1", choices = unique(rv$dat[, 'Analyte']))
    updateSelectInput(session, "metabolite2", choices = unique(rv$dat[, 'Analyte']))
  })
  
  observeEvent(input$in_seq, {
    rv$seq <-
      read.csv(input$in_seq$datapath,
               header = 1,
               stringsAsFactors = F)
    updateSelectInput(session, "time", choices = sort(unique(
      rv$seq[, 'time'], incomparables = NA, na.rm = TRUE
    )))
    
  })
  
  observeEvent(input$button, {
    rv$selected <-
      rv$dat[rv$dat[, 'Analyte'] == input$metabolite &
               rv$dat[, 'Analysis'] == input$sample, 5:11]
    sel <- t(rv$selected)
    sel <- data.frame(cbind(rownames(sel), as.numeric(sel)))
    colnames(sel) <- c("X1", "X2")
    
    output$ggPlot <- renderPlot({
      ggplot(sel, aes(X1, X2)) +
        geom_bar(stat = "identity") +
        labs(x = "Labels", y = "Quantity")
    })
  })
  
  observeEvent(input$button1, {
    #TODO alert if there's no meta file
    rv$selected <- rv$dat[rv$dat[, 'Analyte'] == input$metabolite1, 5:11]
    
    class <- c()
    x <- c()
    names <- c()
    
    for (i in 1:ncol(rv$selected)) {
      class <- append(class, rv$seq[, 'class'])
      x <- append(x, rv$selected[, i])
      names <- append(names, rep(colnames(rv$selected)[i], length(rv$selected[, i])))
    }
    
    sel <- data.frame(names, class, as.numeric(x))
    groups <- unique(rv$seq[, 'class'])
    
    output$ggPlot <- renderPlot({
      ggplot(sel, aes(fill = names, x = class, y = x)) +
        geom_bar(position = "stack", stat = "identity") +
        labs(x = "Labels", y = "Quantity")
    })
  })
  
  # Metabolite and time (multiple groups)
  
  observeEvent(input$button2, {
    metabolite <- ex_dat[ex_dat[, 'Analyte'] == input$metabolite2,]
    time <- input$time
    
    # Select 'Analysis' corresponding to selected time
    seq <- ex_seq[ex_seq[, 'time'] == time, colSums(is.na(ex_seq)) != nrow(ex_seq)]
    seq <- seq[complete.cases(seq),]
    
    dat <- metabolite[metabolite[, 'Analysis'] %in% seq[, 'sample'], colSums(is.na(metabolite)) != nrow(metabolite)]
    
    # Remove irrelevant columns
    valid_column_names <- grep("^[A-Z]\\.[0-9]{1,2}$", names(dat), value = TRUE)
    sel <- dat[, valid_column_names]
    
    # Normalize
    sel <- sel %>%
      rowwise() %>%
      mutate(across(everything(), ~ . / sum(c_across(everything()))))
    
    # 
    class <- c()
    x <- c()
    names <- c()
    
    for (i in 1:ncol(sel)) {
      class <- append(class, seq[, 'class'])
      x <- append(x, unlist(sel[, i]))
      names <-
        append(names, rep(colnames(sel)[i], length(sel[, i])))
    }

    final_dat <- data.frame(names, class, as.numeric(x))
    

    output$ggPlot <- renderPlot({
      ggplot(final_dat, aes(x = class, y = x, fill = names)) +
        geom_bar(stat = "identity", position = "stack") +
        labs(x = "Group", y = "Concentration")
    })
  })
}