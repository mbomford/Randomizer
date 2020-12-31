
# This file is a generated template, your changes will not be overwritten

RCBDClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "RCBDClass",
    inherit = RCBDBase,
    private = list(
        .run = function() {

       ready <- TRUE
       if (is.null(self$options$trt) || length(self$options$trt) <1)
	 ready <- FALSE

       if (ready) 
       {

            a <- expand.grid(self$data)
            b <- c()
            levels <- c()
            for (i in colnames(a)) 
               {
               b <- c(b, paste0('na.omit(a$',i,')'))
               levels <- c(levels, length(unique(eval(parse(text=paste0('na.omit(self$data$',i,')'))))))
               }
            levels <- paste(levels, collapse="x")
            b <- toString(b)
            c <- paste0('paste(',b,')')
            treatments <- unique(eval(parse(text=c)))

            reps <- self$options$reps
                                 
            if(self$options$seed == 0) 
               {seed <- sample(1:9999,reps)} else
               {seed <- self$options$seed}
            set.seed(seed)
            
            factors <- ncol(self$data)
            treatNo <- length(treatments)
            randomization <- replicate(eval(parse(text=reps)), sample(treatments), simplify=FALSE)
            randomization <- as.character(unlist(randomization))

            plots <- length(randomization)

            if(self$options$blockBy == 'byRow')
               {results <- data.frame(matrix(randomization, ncol=treatNo, nrow=reps, byrow=TRUE))
               x <- rep(1:treatNo, reps)
               y <- rep(1:reps, each = treatNo)
               block <- y}
            if(self$options$blockBy == 'byColumn')
               {results <- data.frame(matrix(randomization, ncol=reps, nrow=treatNo, byrow=FALSE))
               y <- rep(1:treatNo, reps) 
               x <- rep(1:reps, each = treatNo)
               block <- x}
            if(self$options$blockBy == 'separate')
               {results <- data.frame(matrix(randomization, ncol=reps, nrow=treatNo, byrow=FALSE))
               block <- rep(1:reps, each = treatNo)
               x <- c()
               y <- c()
               d <- 0
               for(i in 1:treatNo) { 
                  x <- c(x, ceiling(i/ceiling(sqrt(treatNo))))
                  d <- d+1
                  if(d > ceiling(sqrt(treatNo))) {d <- 1}
                  y <- c(y, d)
                   }}
               
            if(self$options$properties==TRUE)
               {
               properties <- data.frame("Factors" = factors, "Levels" = levels, "Treatments" = treatNo, "Blocks" = reps, "Plots" = plots, "ID"=seed)
               self$results$properties$setRow(rowNo=1, properties[1,])
               self$results$properties$setVisible(visible=TRUE)
               } else
               { self$results$properties$setVisible(visible=FALSE)}
           

            plotData <- data.frame(block=block, x=x, y=y, level=randomization)  

            if(self$options$mapGraph==TRUE)
               {
               image <- self$results$plot
               image$setState(plotData)
               self$results$plot$setVisible(visible=TRUE)
               } else
               {self$results$plot$setVisible(visible=FALSE)}

#           if(self$options$mapText==TRUE)
#               {
#               self$results$text$setContent(results)
#               self$results$text$setVisible(visible=TRUE)
#               } else
#               {self$results$text$setVisible(visible=FALSE)}
               

           if(self$options$plotList==TRUE)
               {
               for(row in 1:nrow(plotData))
                  {
                  content<-c("Plot"=row, "Block"=plotData[row,1], "Column"=plotData[row,2], "Row"=plotData[row,3], "Treatment"=toString(plotData[row,4]))
                  self$results$plots$addRow(rowKey=row, content)
                  self$results$plots$setVisible(visible=TRUE)
                  }
               self$results$plots$setVisible(visible=TRUE)  
               } else
               {self$results$plots$setVisible(visible=FALSE)}
           
            TotalDF <- plots-1
            FactorNames <- c(colnames(self$data))
            FactorDF <- c()
            for(n in colnames(self$data)) 
               {
               FactorDF <- c(FactorDF, length(unique(eval(parse(text=paste0('na.omit(self$data$',n,')')))))-1) 
               }
            interactions <- c()
            interactionDF <- c()
             
            if(ncol(self$data) == 2)
                 { 
                 interactions <- paste0(colnames(self$data)[1], ' X ', colnames(self$data)[2])
                 interactionDF <- FactorDF[1] * FactorDF[2]
                 } 
               if(ncol(self$data) == 3)
                 { 
                 interactions <- c(paste0(colnames(self$data)[1], " X ", colnames(self$data)[2]), 
                     paste0(colnames(self$data)[1], " X ", colnames(self$data)[3]), 
                     paste0(colnames(self$data)[2], " X ", colnames(self$data)[3]),
                     paste0(colnames(self$data)[1], " X ", colnames(self$data)[2], " X ", colnames(self$data)[3])) 
                 interactionDF <- c(FactorDF[1] * FactorDF[2],
                     FactorDF[1] * FactorDF[3],
                     FactorDF[2] * FactorDF[3],
                     FactorDF[1] * FactorDF[2] * FactorDF[3]) 
                 }
               if(ncol(self$data) == 4)
                 { 
                 interactions <- c(paste0(colnames(self$data)[1], " X ", colnames(self$data)[2]), 
                     paste0(colnames(self$data)[1], " X ", colnames(self$data)[3]), 
                     paste0(colnames(self$data)[1], " X ", colnames(self$data)[4]), 
                     paste0(colnames(self$data)[2], " X ", colnames(self$data)[3]),
                     paste0(colnames(self$data)[2], " X ", colnames(self$data)[4]),
                     paste0(colnames(self$data)[3], " X ", colnames(self$data)[4]),
                     paste0(colnames(self$data)[1], " X ", colnames(self$data)[2], " X ", colnames(self$data)[3]),
                     paste0(colnames(self$data)[1], " X ", colnames(self$data)[2], " X ", colnames(self$data)[4]),
                     paste0(colnames(self$data)[1], " X ", colnames(self$data)[3], " X ", colnames(self$data)[4]),
                     paste0(colnames(self$data)[2], " X ", colnames(self$data)[3], " X ", colnames(self$data)[4]),                     
                     paste0(colnames(self$data)[1], " X ", colnames(self$data)[2], " X ", colnames(self$data)[3], " X ", colnames(self$data)[4])) 
                 interactionDF <- c(FactorDF[1] * FactorDF[2],
                     FactorDF[1] * FactorDF[3],
                     FactorDF[1] * FactorDF[4],
                     FactorDF[2] * FactorDF[3],
                     FactorDF[2] * FactorDF[4],
                     FactorDF[3] * FactorDF[4],
                     FactorDF[1] * FactorDF[2] * FactorDF[3],
                     FactorDF[1] * FactorDF[2] * FactorDF[4],
                     FactorDF[1] * FactorDF[3] * FactorDF[4],
                     FactorDF[2] * FactorDF[3] * FactorDF[4],
                     FactorDF[1] * FactorDF[2] * FactorDF[3] * FactorDF[4]) 
                   }
                      
               
            FactorNames <- c(FactorNames, interactions)
            FactorDF <- c(reps-1, FactorDF, interactionDF)
            interactions <- c(interactions, paste(apply(as.matrix(FactorNames, byrow = TRUE), 1, paste), collapse = " x "))


            ResidualDF <- TotalDF - do.call(sum, as.list(FactorDF))
            df <- data.frame("Source" = c("Blocks", unlist(FactorNames), "Residual", "Total"), "DF" = c(unlist(FactorDF), ResidualDF, TotalDF))

            if(self$options$degfree==TRUE)
               {
               for(row in 1:nrow(df))
                  {
                  content<-c("Source"=toString(df[row,1]), "DF"=df[row,2])
                  self$results$degfree$addRow(rowKey=row, content)
                  self$results$degfree$setVisible(visible=TRUE)
                  }  
                } else
                {self$results$degfree$setVisible(visible=FALSE)} 
            
                                          
            set.seed(NULL)

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
        }
        },
        .plot=function(image, ...) {  # <-- the plot function
            plotData <- image$state
            if(self$options$blockBy == 'separate')
            {
            plot <- ggplot(plotData, aes(x=x, y=y)) +
                geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                scale_y_reverse() +
                ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                theme(legend.text=element_text(size=11), axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                facet_wrap(block~., scales="free") +
                labs(fill = "Treatments") 
            } else {
            plot <- ggplot(plotData, aes(x=x, y=y)) +
                geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                scale_y_reverse() +
                theme(legend.text=element_text(size=11), axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                labs(fill = "Treatments") 
            } 
            print(plot)
            TRUE
        
        })
)
