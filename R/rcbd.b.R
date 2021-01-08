
# This file is a generated template, your changes will not be overwritten

RCBDClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "RCBDClass",
    inherit = RCBDBase,
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
            FactorNames <- c("Block", FactorNames, interactions, "Residual", "Total")
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
            
            FactorDF <- c()
            levels <- c()
            for(n in colnames(self$data)) 
               {
               nlev <- nlevels(as.factor(self$data[,n])) 
               levels <- c(levels, nlev )
               FactorDF <- c(FactorDF, nlev-1) 
               }
            levels <- jmvcore::stringifyTerm(levels)
            
            reps <- self$options$reps
                                 
            if(self$options$seed == 0) 
               {seed <- sample(1:9999,reps)} else
               {seed <- self$options$seed}
            set.seed(seed)
            
            factors <- ncol(self$data)
            treatNo <- length(treatments)
            randomization <- replicate(n=reps, sample(treatments), simplify=FALSE)
            randomization <- as.character(unlist(randomization))

            plots <- length(randomization)

            if(self$options$blockBy == 'byRow')
               {results <- data.frame(matrix(randomization, ncol=treatNo, nrow=reps, byrow=TRUE))
               x <- rep(1:treatNo, reps)
               y <- rep(1:reps, each = treatNo)
               Block <- y}
            if(self$options$blockBy == 'byColumn')
               {results <- data.frame(matrix(randomization, ncol=reps, nrow=treatNo, byrow=FALSE))
               y <- rep(1:treatNo, reps) 
               x <- rep(1:reps, each = treatNo)
               Block <- x}
            if(self$options$blockBy == 'separate')
               {results <- data.frame(matrix(randomization, ncol=reps, nrow=treatNo, byrow=FALSE))
               Block <- rep(1:reps, each = treatNo)
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
               properties <- data.frame("Factors" = factors, "Levels" = levels, "Treatments" = treatNo, "Blocks" = reps, "Plots" = plots, "Seed"=seed)
               self$results$properties$setRow(rowNo=1, properties[1,])
               self$results$properties$setVisible(visible=TRUE)
               } else
               { self$results$properties$setVisible(visible=FALSE)}
           

            plotData <- data.frame(Block=Block, x=x, y=y, level=randomization)  

            if(self$options$mapGraph==TRUE)
               {
               image <- self$results$plot
               image$setState(plotData)
               self$results$plot$setVisible(visible=TRUE)
               } else
               {self$results$plot$setVisible(visible=FALSE)}
              

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
            levels <- c()
            for(n in colnames(self$data)) 
               {
               nlev <- nlevels(as.factor(self$data[,n])) 
               levels <- c(levels, nlev )
               FactorDF <- c(FactorDF, nlev-1) 
               }
            levels <- jmvcore::stringifyTerm(levels)
            
            interactionDF <- c()
             
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
                      
               
            FactorDF <- c(reps-1, FactorDF, interactionDF)
 
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
            
                                          
            set.seed(NULL)

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
        }
        },
        .plot=function(image, ggtheme, theme, ...) {  # <-- the plot function
            plotData <- image$state
            if(self$options$legend == TRUE)
                {
                if(self$options$blockBy == 'separate')
                    {
                    plot <- ggplot(plotData, aes(x=x, y=y)) +
                        geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                        scale_y_reverse() +
                        ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                        ggtheme +
                        theme(legend.text=element_text(size=11), axis.title.x=element_blank(), axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), 
                            axis.ticks.y=element_blank(), axis.line=element_blank(), strip.background = element_rect(fill="grey")) + 
                        facet_wrap(Block~., scales="free", labeller = label_both) +
                        labs(fill = "Treatments") 
                    } else
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
                                 
                    }
                } else {
                if(self$options$blockBy == 'separate')
                    {
                    plot <- ggplot(plotData, aes(x=x, y=y)) +
                        geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                        scale_y_reverse() +
                        ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                        ggtheme +
                        theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), 
                            axis.ticks.y=element_blank(), axis.line=element_blank(), strip.background = element_rect(fill="grey")) + 
                        facet_wrap(Block~., scales="free", labeller = label_both)  
                    } else
                    {
                    plot <- ggplot(plotData, aes(x=x, y=y)) +
                        geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                        ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                        scale_y_reverse() +
                        ggtheme +
                        theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(),
                            axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), 
                            axis.ticks.y=element_blank(), axis.line=element_blank()) 
                        
                    }
            }
            
            print(plot)
            TRUE
        
        })
)
