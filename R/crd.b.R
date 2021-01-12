
# This file is a generated template, your changes will not be overwritten

CRDClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "CRDClass",
    inherit = CRDBase,
    private = list(
        .init = function() 
        {
            factors <- ncol(self$data)
            FactorNames <- c(colnames(self$data))
            interactions <- c()
            if(ncol(self$data) > 1)
            { 
                if(ncol(self$data) == 2)
                 { 
                 interactions <- jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[2]))
                 } 
                if(ncol(self$data) == 3)
                 {              
                 interactions <- c(jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[2])), 
                     jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[3])), 
                     jmvcore::stringifyTerm(c(colnames(self$data)[2], colnames(self$data)[3])),
                     jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[2], colnames(self$data)[3])))
                 }
                if(ncol(self$data) == 4)
                 {
                 interactions <- c(jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[2])), 
                     jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[3])), 
                     jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[4])), 
                     jmvcore::stringifyTerm(c(colnames(self$data)[2], colnames(self$data)[3])),
                     jmvcore::stringifyTerm(c(colnames(self$data)[2], colnames(self$data)[4])),
                     jmvcore::stringifyTerm(c(colnames(self$data)[3], colnames(self$data)[4])),
                     jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[2], colnames(self$data)[3])),
                     jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[2], colnames(self$data)[4])),
                     jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[3], colnames(self$data)[4])),
                     jmvcore::stringifyTerm(c(colnames(self$data)[2], colnames(self$data)[3], colnames(self$data)[4])),                     
                     jmvcore::stringifyTerm(c(colnames(self$data)[1], colnames(self$data)[2], colnames(self$data)[3], colnames(self$data)[4]))) 
                 }
            }
            FactorNames <- c(FactorNames, interactions, "Residual", "Total")
            if(self$options$degfree==TRUE)
                {
                for(row in 1:length(FactorNames))
                  {
                  content<-c("Source"=toString(FactorNames[row]))
                  self$results$degfree$addRow(rowKey=row, content)
                  self$results$degfree$setVisible(visible=TRUE)
                  }  
                } else
                {self$results$degfree$setVisible(visible=FALSE)}
        },
        
        .run = function() {
       
       ready <- TRUE
       if (is.null(self$options$trt) || length(self$options$trt) <1 || length(self$options$trt) >4)
     ready <- FALSE

       if (ready) 
       {

            a <- na.omit(expand.grid(self$data))
            treatments <- vector("list", nrow(a))
            
            for (i in 1:nrow(a)) {treatments[i] <- jmvcore::stringifyTerm(a[i,], sep=", ")}
            treatments <- unique(treatments)
            
            reps <- self$options$reps
                                 
            if(self$options$seed == 0) 
               {seed <- sample(1:9999,reps)} else
               {seed <- self$options$seed}
            set.seed(seed)
            
            factors <- ncol(self$data)
            treatNo <- length(treatments)
            randomization <- sample(rep(treatments, reps))
            plots <- length(randomization)
            columns <- self$options$col
            difference <- (columns*(ceiling(plots/columns)))-plots
            extras <- c(rep(NA, (difference)))
            randomization <- c(as.character(unlist(randomization)),unlist(extras))
            TotalDF <- plots-1
            
            FactorDF <- c()
            levels <- c()
            for(n in colnames(self$data)) 
               {
               nlev <- nlevels(as.factor(self$data[,n])) 
               levels <- c(levels, nlev )
               FactorDF <- c(FactorDF, nlev-1) 
               }
            levels <- jmvcore::stringifyTerm(levels)
            
            if(ncol(self$data) > 1)
               { 
               if(ncol(self$data) == 2)
                 { 
                 interactionDF <- FactorDF[1] * FactorDF[2]
                 } 
               if(ncol(self$data) == 3)
                 { 
                 interactionDF <- c(FactorDF[1] * FactorDF[2],
                     FactorDF[1] * FactorDF[3],
                     FactorDF[2] * FactorDF[3],
                     FactorDF[1] * FactorDF[2] * FactorDF[3]) 
                 }
               if(ncol(self$data) == 4)
                 { 
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
                      
               FactorDF <- c(FactorDF, interactionDF)
               }

            ResidualDF <- TotalDF - do.call(sum, as.list(FactorDF))

            df <- c(FactorDF, ResidualDF, TotalDF)
          
            if(self$options$degfree==TRUE)
               {
               for(row in 1:length(df))
                  {
                  self$results$degfree$setCell(rowKey=row, col=2, value=df[row])
                  self$results$degfree$setVisible(visible=TRUE)
                  }  
                } else
                {self$results$degfree$setVisible(visible=FALSE)}   
          
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
               properties <- data.frame("Factors" = factors, "Levels" = levels, "Treatments" = treatNo, "Replicates" = reps, "Plots" = plots, "Seed"=seed)
               self$results$properties$setRow(rowNo=1, properties[1,])
               self$results$properties$setVisible(visible=TRUE)
               } else
               { self$results$properties$setVisible(visible=FALSE)}
               
                                
            set.seed(NULL)


       
        }
        },
        .plot=function(image, ggtheme, theme, ...) {  # <-- the plot function
            plotData <- image$state
            if(self$options$legend == TRUE)
            {
            plot <- ggplot(plotData, aes(x=x, y=y)) +
                geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                scale_y_reverse() +
                ggtheme +
                theme(legend.text=element_text(size=11), axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), 
                      axis.ticks.y=element_blank(), axis.line=element_blank()) +
                labs(fill = "Treatments") 
            } else
            {
            plot <- ggplot(plotData, aes(x=x, y=y)) +
                geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                scale_y_reverse() +
                ggtheme +
                theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), 
                      axis.ticks.y=element_blank(), axis.line=element_blank()) +
                labs(fill = "Treatments") 
            }   

 
            print(plot)
            TRUE
        
        })
)