
# This file is a generated template, your changes will not be overwritten

LSDClass <- if (requireNamespace('jmvcore')) R6::R6Class(
    "LSDClass",
    inherit = LSDBase,
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
               map <- data.frame()
               x <- c(rep(1:treatNo, treatNo))
               y <- c(rep(1:treatNo, each=treatNo))
               for (row in 1:treatNo)
                  {for (column in 1:treatNo)
                     {level <- c(level, treatments[sq[row,column]])
                      map[row,column]<-treatments[sq[row,column]]}}
               plotData <- data.frame(x, y, level)

            TotalDF <- treatNo^2-1
            FactorNames <- c(colnames(self$data))
            FactorDF <- c()
            for(n in colnames(self$data)) 
               {
               FactorDF <- c(FactorDF, length(unique(eval(parse(text=paste0('na.omit(self$data$',n,')')))))-1) 
               }
            interactions <- c()
            interactionDF <- c()
 #           if(ncol(self$data) > 1)
 #              { 
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
               FactorDF <- c(treatNo-1, treatNo-1, FactorDF, interactionDF)
               interactions <- c(interactions, paste(apply(as.matrix(FactorNames, byrow = TRUE), 1, paste), collapse = " x "))
#               }

            ResidualDF <- TotalDF - do.call(sum, as.list(FactorDF))

          
            if(self$options$properties==TRUE)
               {
               properties <- data.frame("Factors" = factors, "Levels" = levels, "Treatments" = treatNo, "Replicates" = treatNo, "Plots" = treatNo^2, "ID"=seed)
               self$results$properties$setRow(rowNo=1, properties[1,])
               self$results$properties$setVisible(visible=TRUE)
               } else
               { self$results$properties$setVisible(visible=FALSE)}

            if(self$options$degfree==TRUE)
               {           
               df <- data.frame("Source" = c("Rows", "Columns", unlist(FactorNames), "Residual", "Total"), "DF" = c(unlist(FactorDF), ResidualDF, TotalDF))
               for(row in 1:nrow(df))
                  {
                  content<-c("Source"=toString(df[row,1]), "DF"=df[row,2])
                  self$results$degfree$addRow(rowKey=row, content)
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
               
#             if(self$options$mapText==TRUE)
#               {  
#               self$results$text$setContent(map)
#               self$results$text$setVisible(visible=TRUE)
#               } else
#               {self$results$text$setVisible(visible=FALSE)}

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
                                
            set.seed(NULL)

            # `self$data` contains the data
            # `self$options` contains the options
            # `self$results` contains the results object (to populate)
        }
        },
        .plot=function(image, ...) {  # <-- the plot function
            plotData <- image$state
            if(self$options$arrange == 'square')
              {
              plot <- ggplot(plotData, aes(x=x, y=y)) +
                geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                scale_y_reverse() +
                theme(legend.text=element_text(size=11), axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                labs(fill = "Treatments") 
              } else
              {
              plot <- ggplot(plotData, aes(x=x, y=y)) +
                geom_tile(aes(x=x, y=y, height=0.8, width=0.8, fill=level)) +
                ggfittext::geom_fit_text(aes(label=level), reflow=TRUE) +
                scale_y_reverse() +
                theme(legend.text=element_text(size=11), axis.title.x=element_blank(), axis.text.x=element_blank(),
                      axis.ticks.x=element_blank(), axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank()) +
                facet_wrap(x~., scales="free") +
                labs(fill = "Treatments") 
               }
theme()  
            print(plot)
            TRUE
        
        })
)