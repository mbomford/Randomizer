
# This file is a generated template, your changes will not be overwritten

CRDClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "CRDClass",
    inherit = CRDBase,
    private = list(
        .run = function() {
       
       ready <- TRUE
       if (is.null(self$options$trt) || length(self$options$trt) <1)
	 ready <- FALSE

       if (ready) 
       {

            a <- expand.grid(self$data)
            b <- c()
            for (i in colnames(a)) {b <- c(b, paste0('na.omit(a$',i,')'))}
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
            randomization <- sample(rep(treatments, eval(parse(text=reps))))
            randomization <- as.character(unlist(randomization))
            plots <- length(randomization)
            columns <- self$options$col
            difference <- (columns*(ceiling(plots/columns)))-plots
            extras <- c(rep(NA, (difference)))
            randomization <- c(as.character(unlist(randomization)),unlist(extras))
            grid <- paste(as.character(columns), 'X', as.character(ceiling(plots/columns)))
                        
            TotalDF <- plots-1
            FactorNames <- c(colnames(self$data))
            FactorDF <- c()
            levels <- c()
            for(n in colnames(self$data)) 
               {
               FactorDF <- c(FactorDF, length(unique(eval(parse(text=paste0('na.omit(self$data$',n,')')))))-1)
               levels <- c(levels, length(unique(eval(parse(text=paste0('na.omit(self$data$',n,')')))))) 
               }
            levels <- paste(levels, collapse="x")
            
            interactions <- c()
            if(ncol(self$data) > 1)
               { 
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
               FactorDF <- c(FactorDF, interactionDF)
               interactions <- c(interactions, paste(apply(as.matrix(FactorNames, byrow = TRUE), 1, paste), collapse = " x "))
               }

            ResidualDF <- TotalDF - do.call(sum, as.list(FactorDF))

            df <- data.frame("Source" = c(unlist(FactorNames), "Residual", "Total"), "DF" = c(unlist(FactorDF), ResidualDF, TotalDF))
          
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

 #           if(self$options$mapText==TRUE)
 #              {
 #              results <- data.frame(matrix(randomization, ncol=columns, byrow=T))
 #              self$results$text$setContent(results)
 #              self$results$text$setVisible(visible=TRUE)

 #             for(column in 1:columns)
 #                {
 #                self$results$textmap$addColumn(name=as.text(column))
 #                }
 #             for(row in 1:nrow(results))
 #                {
 #                self$results$textmap$addRow(row, slice(results, row))
 #                }
 #             self$results$textmap$setVisible(visible=TRUE)

 #              } else
 #              {self$results$text$setVisible(visible=FALSE)}

            x <- rep(1:columns, ceiling(plots/columns))            
            y <- rep(1:ceiling(plots/columns), each = columns)

            plotData <- data.frame(x=x, y=y, level=randomization) 


              

            if(self$options$plotList==TRUE)
               {
               for(row in 1:nrow(plotData))
                  {
                  content<-c("Plot"=row, "Column"=plotData[row,1], "Row"=plotData[row,2], "Treatment"=toString(plotData[row,3]))
                  self$results$plots$addRow(rowKey=row, content)
                  self$results$plots$setVisible(visible=TRUE)
                  }
               self$results$plots$setVisible(visible=TRUE)  
               } else
               {
               self$results$plots$setVisible(visible=FALSE)
               }
          
            if(self$options$mapGraph==TRUE)
               {
               image <- self$results$plot
               image$setState(plotData)
               self$results$plot$setVisible(visible=TRUE)
               } else
               {self$results$plot$setVisible(visible=FALSE)}
           
            if(self$options$properties==TRUE)
               {
               properties <- data.frame("Factors" = factors, "Levels" = levels, "Treatments" = treatNo, "Replicates" = reps, "Plots" = plots, "ID"=seed)
               self$results$properties$setRow(rowNo=1, properties[1,])
               self$results$properties$setVisible(visible=TRUE)
               } else
               { self$results$properties$setVisible(visible=FALSE)}
               
                                
            set.seed(NULL)

 #           if(self$options$power==TRUE) 
 #              {
 #              power <- power.anova.test(groups = treatNo, n = reps, within.var=1, sig.level = 0.05, power = 0.8)
 #              self$results$power$setContent(power)
 #              self$results$power$setVisible(visible=TRUE)
 #              } else
 #              { self$results$power$setVisible(visible=FALSE)}

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)

       
        }
        },
        .plot=function(image, ...) {  # <-- the plot function
            plotData <- image$state
            plot <- ggplot(plotData, aes(x=x, y=y)) +
                geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                scale_y_reverse() +
                theme(legend.text=element_text(size=11), axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                labs(fill = "Treatments") 

theme()  
            print(plot)
            TRUE
        
        })
)