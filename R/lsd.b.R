
# This file is a generated template, your changes will not be overwritten

LSDClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "LSDClass",
    inherit = LSDBase,
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
                
            }
            FactorNames <- c("Rows", "Columns", FactorNames, interactions, "Residual", "Total")
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
       if (is.null(self$options$trt) || length(self$options$trt) <1 || length(self$options$trt) >3)
     ready <- FALSE

       if (ready) 
       {

            a <- na.omit(expand.grid(self$data))
            levels <- c()
            for(n in colnames(self$data)) 
               {
               nlev <- nlevels(as.factor(self$data[,n])) 
               levels <- c(levels, nlev )
               }
            
            treatments <- vector("list", nrow(a))
            for (i in 1:nrow(a)) {treatments[i] <- jmvcore::stringifyTerm(a[i,], sep=", ")}
            treatments <- unique(treatments)

            if(self$options$seed == 0) 
               {seed <- sample(1:9999,1)} else
               {seed <- self$options$seed}
            set.seed(seed)

            treatNo <- length(treatments)   
            factors <- ncol(self$data)
            allsq <- matrix(nrow=treatNo, ncol=treatNo)
            sample1 <- function(x)
               {
               if(length(x)==1) 
                  {return(x)} else 
                  {return(sample(x,1))}
               }

            sq <- matrix(nrow=treatNo, ncol=treatNo)
            while (any(is.na(sq))) 
               {
               k <- sample1(which(is.na(sq)))
               i <- (k-1) %% treatNo +1
               j <- floor((k-1) / treatNo) + 1
               sqrow <- sq[i,]
               sqcol <- sq[,j]
               openCell <- rbind(cbind(which(is.na(sqcol)),j), cbind(i, which(is.na(sqrow))))
               openCell <- openCell[sample(nrow(openCell)),]
               openCell <- matrix(openCell[!duplicated(openCell),], ncol=2)
               for (c in 1:nrow(openCell))
                  {
                  ci <- openCell[c,1]
                  cj <- openCell[c,2]
                  freeNum <- which(!(1:treatNo %in% c(sq[ci,], sq[,cj])))
                  if (length(freeNum)>0) {sq[ci,cj] <- sample1(freeNum)}
                  else {sq <- matrix(nrow=treatNo, ncol=treatNo)
                     break; }
                  }
               }
               
            level <- c()
            x <- c(rep(1:treatNo, treatNo))
            y <- c(rep(1:treatNo, each=treatNo))
            Block <- x
            for (row in 1:treatNo)
                {for (column in 1:treatNo)
                   {level <- c(level, treatments[sq[row,column]])}}
            plotData <- data.frame(Block=x, y=y, level=unlist(level))

            TotalDF <- treatNo^2-1
            FactorDF <- levels-1

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
               
                      
            FactorDF <- c(treatNo-1, treatNo-1, FactorDF, interactionDF)

            ResidualDF <- TotalDF - do.call(sum, as.list(FactorDF))
            df <- c(FactorDF, ResidualDF, TotalDF)
            
            levels <- jmvcore::stringifyTerm(levels)
          
            if(self$options$properties==TRUE)
               {
               properties <- data.frame("Factors" = factors, "Levels" = levels, "Treatments" = treatNo, "Replicates" = treatNo, "Plots" = treatNo^2, "Seed"=seed)
               self$results$properties$setRow(rowNo=1, properties[1,])
               self$results$properties$setVisible(visible=TRUE)
               } else
               { self$results$properties$setVisible(visible=FALSE)}

            if(self$options$degfree==TRUE)
               {           
               for(row in 1:length(df))
                  {
                  self$results$degfree$setCell(rowKey=row, col=2, value=df[row])
                  self$results$degfree$setVisible(visible=TRUE)
                  }  
                } else
                {self$results$degfree$setVisible(visible=FALSE)} 

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
                  content<-c("Plot"=row, "Column"=plotData[row,1], "Row"=plotData[row,2], "Treatment"=plotData[row,3])
                  self$results$plots$addRow(rowKey=row, content)
                  self$results$plots$setVisible(visible=TRUE)
                  }
               self$results$plots$setVisible(visible=TRUE)  
               } else
               {
               self$results$plots$setVisible(visible=FALSE)
               }
                                
            set.seed(NULL)

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
        }
        },
        .plot=function(image, ggtheme, theme, ...) {  # <-- the plot function
            plotData <- image$state
            if(self$options$arrange == 'separate')
              {
              if(self$options$legend == TRUE)
                {
                plot <- ggplot(plotData, aes(x=Block, y=y)) +
                    geom_tile(aes(x=Block, y=y, height=0.8, width=0.8, fill=level)) +
                    ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                    scale_y_reverse() +
                    ggtheme +
                    theme(legend.text=element_text(size=11), axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), 
                      axis.ticks.y=element_blank(), axis.line=element_blank(), strip.background = element_rect(fill="grey")) + 
                    facet_wrap(Block~., scales="free", labeller = label_both) + 
                    labs(fill = "Treatments") 
                } else
                {
                plot <- ggplot(plotData, aes(x=Block, y=y)) +
                    geom_tile(aes(x=Block, y=y, height=0.8, width=0.8, fill=level)) +
                    ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                    scale_y_reverse() +
                    ggtheme +
                    theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), 
                      axis.ticks.y=element_blank(), axis.line=element_blank(), strip.background = element_rect(fill="grey")) + 
                    facet_wrap(Block~., scales="free", labeller = label_both) 
                }
                
              } else
              {
              if(self$options$legend == TRUE)
                {
                plot <- ggplot(plotData, aes(x=Block, y=y)) +
                    geom_tile(aes(x=Block, y=y, height=0.8, width=0.8, fill=level)) +
                    ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                    scale_y_reverse() +
                    ggtheme +
                    theme(legend.text=element_text(size=11), axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), 
                        axis.ticks.y=element_blank(), axis.line=element_blank()) +
                        labs(fill = "Treatments") 
                } else
                {
                plot <- ggplot(plotData, aes(x=Block, y=y)) +
                    geom_tile(aes(x=Block, y=y, height=0.8, width=0.8, fill=level)) +
                    ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                    scale_y_reverse() +
                    ggtheme +
                    theme(legend.position="none", axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), 
                        axis.ticks.y=element_blank(), axis.line=element_blank())
                }
              } 
            theme()  
            print(plot)
            TRUE
        
        })
)