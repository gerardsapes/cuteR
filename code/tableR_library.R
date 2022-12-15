###Code for publication ready tables of model outcomes######
library(plyr)

#LM Table compiling model stats for publication
stats_table<-function(model){
  #Extracting model summary
  m <- summary(model)
  #Generating table
  table <- as.data.frame(matrix(nrow = length(m$coefficients[,1])+1,ncol = 8))
  colnames(table) <- c("Model and Factors", "Model Type", "Estimate","2.5%","97.5%","p-value","d.f. (res.)","Adjusted R square")
  #Writing model formula
  table$`Model and Factors`[1] <- as.character(m$call)[2]
  #Writing model tipe
  table$`Model Type`[1] <- toupper(as.character(m$call)[1])
  #Writing factors and formatting them
  table$`Model and Factors`[2:length(table$Estimate)] <- rownames(m$coefficients)
  table$`Model and Factors` <- gsub("[()]","",table$`Model and Factors`)
  table$`Model and Factors` <- sub(":"," x ",table$`Model and Factors`)
  #Adding Estimates
  table$Estimate[2:length(table$Estimate)] <- round(m$coefficients[,1],digits=5)
  #Adding CI of estimates
  table$`2.5%`[2:length(table$Estimate)] <- round(confint(model)[,1],digits=5)
  table$`97.5%`[2:length(table$Estimate)] <- round(confint(model)[,2],digits=5)
  #Adding pvalues for overall model and factors
  p <- (stats::pf(m$fstatistic[1],m$fstatistic[2],m$fstatistic[3],lower.tail=F))
  attributes(p)<-NULL
  table$`p-value`[1] <- format(p,scientific=F)
  table$`p-value`[2:length(table$Estimate)] <- (format(m$coefficients[,4], scientific=F))
  #Formating p-values
  table$`p-value` <- ifelse(as.numeric(table$`p-value`<0.001),"<0.001",round(as.numeric(table$`p-value`),digits =3))
  #Adding residual degrees of freedom
  table$`d.f. (res.)`[1] <- m$df[2]
  #Adding Adjusted R2
  table$`Adjusted R square`[1] <- round(m$adj.r.squared,digits = 2)
  
  table[is.na(table)] <- " "
  return(table)
}

#Execution example of stats_table
#model <- lm(yvar~xvar,data)
#stats_table(model)

#GLM Table compiling model stats for publication
stats_table_glm<-function(model){
  #Extracting model summary
  m <- summary(model)
  #Generating table
  table <- as.data.frame(matrix(nrow = length(m$coefficients[,1])+1,ncol = 8))
  colnames(table) <- c("Model and Factors", "Model Type", "Estimate","2.5%","97.5%","p-value","d.f. (res.)","AIC")
  #Writing model formula
  table$`Model and Factors`[1] <- as.character(m$call)[2]
  #Writing model tipe
  table$`Model Type`[1] <- toupper(as.character(m$call)[1])
  #Writing factors and formatting them
  table$`Model and Factors`[2:length(table$Estimate)] <- rownames(m$coefficients)
  table$`Model and Factors` <- gsub("[()]","",table$`Model and Factors`)
  table$`Model and Factors` <- sub(":"," x ",table$`Model and Factors`)
  #Adding Estimates
  table$Estimate[2:length(table$Estimate)] <- round(m$coefficients[,1],digits=5)
  #Adding CI of estimates
  table$`2.5%`[2:length(table$Estimate)] <- round(confint(model)[,1],digits=5)
  table$`97.5%`[2:length(table$Estimate)] <- round(confint(model)[,2],digits=5)
  #Adding pvalues for overall model and factors
  #p <- (stats::pf(m$fstatistic[1],m$fstatistic[2],m$fstatistic[3],lower.tail=F))
  #attributes(p)<-NULL
  #table$`p-value`[1] <- format(p,scientific=F)
  table$`p-value`[2:length(table$Estimate)] <- (format(m$coefficients[,4], scientific=F))
  #Formating p-values
  table$`p-value` <- ifelse(as.numeric(table$`p-value`<0.001),"<0.001",round(as.numeric(table$`p-value`),digits =3))
  #Adding residual degrees of freedom
  table$`d.f. (res.)`[1] <- m$df[2]
  #Adding AIC
  table$`AIC`[1] <- round(m$aic,digits = 2)
  
  table[is.na(table)] <- " "
  return(table)
}

#Execution example of stats_table_glm
#model <- glm(yvar~xvar,family = binomial,data)
#stats_table_glm(model)


#Calculates means and SD of any given variable by group
data_summary <- function(data, var, group){
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-plyr::ddply(data, group, .fun=summary_func,
                        var)
  data_sum <- plyr::rename(data_sum, c("mean" = var))
  return(data_sum)
}

#Execution example of data_summary

#data <- stem_condy
#var <- stem_condy$Days_Drying 
#group <- stem_condy$Race #Grouping used for coloring (this one was a bit racist...)
#data_summary(data, var, group)

