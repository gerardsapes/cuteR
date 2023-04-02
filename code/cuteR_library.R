######Libraries used######
library(plyr)
library(ggplot2)
library(reshape2)
library(scales)
library(RColorBrewer)
library(ggsignif)
# library(devtools)
library(rlang)
library(factoextra)
library(ggpubr)
library(tidyverse)
library(broom)
library(multcomp)
library(Rmisc)

######General clean plot theme######
z_theme <- function() {
  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- RColorBrewer::brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[5]
  color.axis.text = palette[7]
  color.axis.title = palette[7]
  color.title = palette[8]
  # Begin construction of chart
  ggplot2::theme_bw(base_size=1) +
    # Set the entire chart region to a white color
    ggplot2::theme(panel.background=ggplot2::element_rect(fill=color.background, color=color.background)) +
    ggplot2::theme(plot.background=ggplot2::element_rect(fill=color.background, color=color.background),plot.margin = ggplot2::margin(0,10,0,0)) +
    ggplot2::theme(panel.border=ggplot2::element_rect(color=color.background)) +
    # Format the grid
    ggplot2::theme(panel.grid.major.x=ggplot2::element_blank()) +
    ggplot2::theme(panel.grid.major.y=ggplot2::element_blank()) +
    ggplot2::theme(panel.grid.minor=ggplot2::element_blank()) +
    ggplot2::theme(axis.ticks=ggplot2::element_blank()) +
    # Format the legend, but hide by default
    ggplot2::theme(legend.position="none") +
    ggplot2::theme(legend.background = ggplot2::element_rect(fill=color.background)) +
    ggplot2::theme(legend.text = ggplot2::element_text(size=7,color=color.axis.title)) +
    # Set title and axis labels, and format these and tick marks
    ggplot2::theme(plot.title=ggplot2::element_text(color=color.title, size=20, vjust=1.25)) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(size=9,color=color.axis.text,margin=ggplot2::margin(1,0,0,0))) +
    ggplot2::theme(axis.text.y=ggplot2::element_text(size=9,color=color.axis.text,margin=ggplot2::margin(0,1,0,0))) +
    ggplot2::theme(axis.title.x=ggplot2::element_text(size=12,color=color.axis.title, margin=ggplot2::margin(7,0,0,0))) +
    ggplot2::theme(axis.title.y=ggplot2::element_text(size=12,color=color.axis.title, margin=ggplot2::margin(0,7,0,0)))+
    ggplot2::theme(axis.ticks = ggplot2::element_line(size = .2, colour = "grey80"), axis.ticks.length = grid::unit(.1,'cm'))+
    ggplot2::theme(axis.line.x= ggplot2::element_line(colour = 'grey80', size = 1))+
    ggplot2::theme(axis.line.y= ggplot2::element_line(colour = 'grey80', size = 1))
}

######Color libraries######
colors <- c('#CC9900','#336666','#333333', NA, '#CC0000', NA, '#FFFF66', '#FFFF00','#33CC99', '#009933', '#9933FF','#CC33FF','#CC9933','#996600','#FF3399', '#CC0066', '#FF00CC', '#990066', '#996699','#3333FF', '#FF00FF', '#66CC66')

colors_line <- c('#CC9900','#336666','#333333', '#333333', '#CC0000', '#CC0000', '#FFFF66', '#FFFF66', '#FFFF00','#33CC99', '#009933', '#9933FF','#CC33FF','#CC9933','#996600','#FF3399', '#CC0066', '#FF00CC', '#990066', '#996699','#3333FF', '#FF00FF', '#66CC66')

colors_median <- c('#CC9900', '#CC9900','#333333','#333333','#333333', '#660000','#333333','#660000')


#Comparisons needed for Stats for boxplots
# mycomparisons <- list(c("NS.Light","S.Shade"),
#                       c("S.Light","S.Shade"),
#                       c("NS.Light","S.Light"))
# 
# mycomparisons1 <- c("NS.Light","S.Light","NS.Light")
# mycomparisons2 <- c("S.Shade","S.Shade","S.Light")
# 
# CN <- mycomparisons
# CN2 <- data.frame(matrix(ncol=2, nrow=length(mycomparisons1)))
# colnames(CN2) <- c('group1','group2')
# CN2$group1 <- mycomparisons1
# CN2$group2 <- mycomparisons2



######Stats for boxplots######
stats<-function(data,yvar,group,stats_file_path,comparisons){
  
  pv<-function(data,yvar,group,comparisons){
    
    #pv<-tidy(with(data, pairwise.wilcox.test(yvar, group, p.adjust.method = "none")))
    #Creating anova
    Test3 <- stats::aov(yvar~group, data=data)
    base::summary(Test3)
    
    #creating hypotheses based on all potential combinations
    #Delete groups without data
    checkNA <- base::data.frame(group,yvar)
    checkNA <- stats::aggregate(checkNA,by = base::list(group),FUN = 'mean', na.rm = T)
    checkNA <- checkNA[-2]
    checkNA <- checkNA[stats::complete.cases(checkNA),]
    #dropping unused levels before creating hypotheses for tukey
    checkNA$Group.1 <- base::droplevels(checkNA$Group.1)
    
    testings <- utils::combn(base::levels(checkNA$Group.1),m = 2)
    
    hypothesis<-base::vector()
    for (i in 1:base::length(base::as.data.frame(testings))) {
      
      hypothesis[i] <- base::paste(testings[1,i],'-',testings[2,i],' = 0', sep = '')
      
    }
    
    
    
    #checking if there are specific hypotheses to be made. If not, do all the comparisons.
    if (base::is.null(comparisons)==T) {
      hypothesis <- hypothesis
    } else {
      hypothesis <- hypothesis[hypothesis %in% comparisons]
    }
    
    #Run Tukey with specified hypotheses
    Tukey1<-multcomp::glht(Test3, linfct = multcomp::mcp(group = hypothesis))
    
    table_glht <- function(Tukey1) {
      pq <- base::summary(Tukey1)$test
      mtests <- base::cbind(pq$coefficients, pq$sigma, pq$tstat, pq$pvalues)
      error <- base::attr(pq$pvalues, "error")
      pname <- switch(Tukey1$alternativ, less = base::paste("Pr(<", base::ifelse(Tukey1$df ==0, "z", "t"), ")", sep = ""), 
                      greater = base::paste("Pr(>", base::ifelse(Tukey1$df == 0, "z", "t"), ")", sep = ""), two.sided = base::paste("Pr(>|",base::ifelse(Tukey1$df == 0, "z", "t"), "|)", sep = ""))
      base::colnames(mtests) <- c("Estimate", "Std. Error", base::ifelse(Tukey1$df ==0, "z value", "t value"), pname)
      return(mtests)
      
    }
    
    Tukey <- table_glht(Tukey1)
    
    
    #Tukey<-TukeyHSD(Test3, 'TxLT', conf.level=0.95)
    
    utils::write.csv(base::data.frame(Tukey),file = base::paste0(stats_file_path,"Tukeyresults.csv"),sep = ",", col.names = NA,
                     qmethod = "double") 
    dfTukey<-utils::read.csv(base::paste0(stats_file_path,"Tukeyresults.csv"))
    split<-stringr::str_split_fixed(dfTukey$X, " - ", 2)
    dfTukey$group1 <- split[,1]
    dfTukey$group2 <- split[,2]
    dfTukey <- dfTukey[,5:7]
    
    return(dfTukey)
    
  }
  
  pvalues<-pv(data,yvar,group,comparisons)
  
  #pvalues <- pvalues[order(pvalues$group1), ] 
  pvalues$map_signif <- base::ifelse(pvalues$Pr...t.. > 0.1, "", base::ifelse(pvalues$Pr...t.. > 0.05, ".", base::ifelse(pvalues$Pr...t.. > 0.01,"*",base::ifelse(pvalues$Pr...t.. > 0.001,"**", "***"))))  
  
  return(pvalues)
  
}


#Comparisons to be done by Tukey test
Tukeygroups <- function(group, mycomparisons) {
  
  comparisons <- base::list()
  
  if (base::is.null(mycomparisons)==T) {
    
    #Delete groups without data
    checkNA <- base::data.frame(group,yvar)
    checkNA <- stats::aggregate(checkNA,by = base::list(group),FUN = 'mean', na.rm = T)
    checkNA <- checkNA[-2]
    checkNA <- checkNA[stats::complete.cases(checkNA),]
    #dropping unused levels before creating hypotheses for tukey
    checkNA$Group.1 <- base::droplevels(checkNA$Group.1)
    
    combos <- utils::combn(base::levels(checkNA$Group.1),2)
    
    for (i in 1:base::length(combos[1,])) {
      comparisons[[i]] <- c(combos[1,i],combos[2,i])
    }
    
    return(comparisons)
    
  } else {
    
    comparisons <- mycomparisons  
    return(comparisons)
  }
}


######Simpler BoxPlot without stats######
cuteboxes <- function(data,group,variable,xlab,title) {
  
  ggplot2::ggplot(data,aes(group,variable))+
    ggplot2::geom_boxplot(aes(fill=group),size=0.2,alpha=.5, notch = F)+ ###SIGNIFICANCE SWITCH
    ggplot2::geom_jitter(aes(color=group,shape=group),size=2,alpha=1)+
    #ggplot2::coord_flip()+
    ggplot2::guides(fill=FALSE,color=FALSE)+
    ggplot2::xlab('Treatment')+
    ggplot2::ylab(xlab)+
    z_theme()+
    ggplot2::theme(axis.line = ggplot2::element_line(size = 0.5, colour = "grey80")) +
    #scale_y_continuous(breaks=seq(0,100,10))+
    ggplot2::ggtitle(title)+
    ggplot2::scale_fill_manual(values=colors)+
    ggplot2::scale_color_manual(values=colors)
}

##Execution example of cuteboxes without stats
#data <- data
#group <- data$Race #Grouping used for coloring 
#variable <- data$Plant_length_cm
#title <- '' #Title of graph. Leave as is if you don't want one
#xlab <- 'Plant Length (Cm)'
#
#cuteboxes(data,group,variable,xlab,title)


######BoxPlot with Stats######
#######BoxPlot#####
cuteboxes <- function(data,group,yvar,xlab,title,colors,colors_line) {
  
  
  
  comb <- (as.data.frame(t(utils::combn(levels(group),2))))
  
  CN <- list()
  
  for (i in 1:length(comb[,1])) {
    
    CN[[i]] <- c(comb[i,1],comb[i,2])
    
  }
  
  
  #Delete groups without data
  checkNA <- data.frame(group,yvar)
  checkNA <- stats::aggregate(checkNA,by = list(group),FUN = 'mean', na.rm = T)
  checkNA <- checkNA[-2]
  dropped <-as.character(checkNA[which(complete.cases(checkNA)==F),1])
  checkNA <- checkNA[complete.cases(checkNA),]
  #dropping unused levels before creating hypotheses for tukey
  checkNA$Group.1 <- droplevels(checkNA$Group.1)
  
  dropped_pos <- list()
  
  if (length(dropped) > 0) {
    
    
    for (i in 1:length(dropped)) {
      
      dropped_pos[[i]] <- grep(dropped[i], CN)
      
    }
    
    drops <- vector()
    for (i in 1:length(dropped_pos)) {
      
      drops <-  c(drops,  dropped_pos[[i]]) 
      
    }
    CN[drops] <- NULL
    
  }
  
  data <- data[group %in% checkNA$Group.1,]
  yvar <- yvar[group %in% checkNA$Group.1]
  group <- group[group %in% checkNA$Group.1] 
  group <- droplevels(group)
  
  if(length(statistics$map_signif[gr])>=1){
    
    ggplot2::ggplot(data,aes(group,yvar))+
      ggplot2::geom_boxplot(aes(fill=group),lwd= 0.71, size=0.2,alpha=.5, notch = F,color= colors_line[1:length(levels(group))])+ ###SIGNIFICANCE SWITCH
      ggplot2::geom_signif(comparisons=CN[gr], textsize = 5, vjust = 0.7, 
                  step_increase=0.12, annotation= statistics$map_signif[gr])+
      ggplot2::geom_jitter(aes(color=group),size=2,alpha=.3)+
      #ggplot2::coord_flip()+
      ggplot2::guides(fill=FALSE,color=FALSE)+
      ggplot2::xlab('treatment')+
      ggplot2::ylab(xlab)+
      z_theme()+
      ggplot2::theme(axis.line = ggplot2::element_line(size = 0.5, colour = NA)) +
      #ggplot2::scale_y_continuous(breaks=seq(0,100,10))+
      ggplot2::ggtitle(title)+
      ggplot2::scale_fill_manual(values= colors[1:length(levels(group))]) +
      ggplot2::scale_color_manual(values=colors_line[1:length(levels(group))])+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,vjust=0.5, hjust = 1,colour = '#666666'),axis.text.y = ggplot2::element_text(hjust = 1,colour = '#666666'),axis.title.x = ggplot2::element_text(colour = "#666666"),axis.title.y = ggplot2::element_text(colour = "#666666"))
    
  } else {
    
    ggplot2::ggplot(data,aes(group,yvar))+
      ggplot2::geom_boxplot(aes(fill=group),lwd= 0.71, size=0.2,alpha=.5, notch = F,color= colors_line[1:length(levels(group))])+ ###SIGNIFICANCE SWITCH
      ggplot2::geom_jitter(aes(color=group),size=2,alpha=.3)+
      #ggplot2::coord_flip()+
      ggplot2::guides(fill=FALSE,color=FALSE)+
      ggplot2::xlab('treatment')+
      ggplot2::ylab(xlab)+
      z_theme()+
      ggplot2::theme(axis.line = ggplot2::element_line(size = 0.5, colour = NA)) +
      #scale_y_continuous(breaks=seq(0,100,10))+
      ggplot2::ggtitle(title)+
      ggplot2::scale_fill_manual(values=colors[1:length(levels(group))])+
      ggplot2::scale_color_manual(values=colors_line[1:length(levels(group))])+
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust=0.5, hjust = 1,colour = '#666666'),axis.text.y = ggplot2::element_text(hjust = 1,colour = '#666666'),axis.title.x = ggplot2::element_text(colour = "#666666"),axis.title.y = ggplot2::element_text(colour = "#666666"))    
    
  }
}


# #Execution example of cuteboxes with stats
# data <- data13
# group <- data13$IntFac2 #Grouping used for coloring and statistics. WILL FAIL IF a treatment name starts with a number. Use "_" instead of spaces.
# variable <- data13$D13_Stem
# title <- '' #Title of graph. Leave as is if you don't want one
# xlab <- 'Delta 13C Stems'
# 
# statistics<-stats(data,yvar,group,stats_file_path,comparisons = NULL)
# gr <-statistics$Pr...t.. <= 0.05
# cuteboxes(data,group,variable,xlab,title,colors,colors_line)



######Scatterplot######
cuteregs <- function(data,xvar,yvar,group,method,formula,xlab,ylab,title){
  if (is.factor(group) ==T) {
    
    
    ggplot2::ggplot(data, aes(x=xvar, y=yvar, color=group,fill=group)) +
      ggplot2::geom_point(size=2.5) + 
      ggplot2::geom_smooth(method=method, formula = formula, se=T, size = 1,alpha=0.1)+
      ggplot2::labs(y=ylab,x=xlab,title=title) +
      z_theme() +
      ggplot2::theme(axis.line = ggplot2::element_line(size = 0.5, colour = "grey80")) +
      ggplot2::scale_color_manual(values=colors)+ggplot2::scale_fill_manual(values=colors)
    
  } else if (is.numeric(group) ==T) {
    
    ggplot2::ggplot(data, aes(x=xvar, y=yvar, color=group,fill=group)) +
      ggplot2::geom_point(size=2.5) + 
      ggplot2::geom_smooth(method=method, formula = formula, se=T, size = 1,alpha=0.1,color = "black")+
      ggplot2::labs(y=ylab,x=xlab,title=title) +
      z_theme() +
      ggplot2::theme(axis.line = ggplot2::element_line(size = 0.5, colour = "grey80")) +
      ggplot2::scale_color_gradient(low = colors[1],high = colors[2],guide = "colourbar")+
      ggplot2::scale_fill_gradient(low = colors[1],high = colors[2],guide = "colourbar")
    
  }
  
}

# #Execution example of cuteregs
# data <- dataset
# xvar <- dataset$Days_Drying
# yvar <- dataset$kl_stem
# group <- dataset$Race #Grouping used for coloring and number of regressions
# xlab <- 'Days since start of Drought'
# ylab <- 'Stem Conductance (Units)'
# title <- '' #Title of graph. Leave as is if you don't want one
# method <- 'loess'  #Type of regression model (eg. loess, lm, glm). Loess useful to observe natural behavior of the data.
# formula <- y~x #Type of relationship between variables (eg. y~x, y~poly(x,2) for quadratic, poly(x,3) for cubic, etc...)
# cuteregs(data,xvar,yvar,group,method,formula,xlab,ylab,title) #No need to touch anything if everything on top is specified correctly.
# 



######Scatterplot with error bars on points######
meanregs <- function(data,xvar,sd_xvar,yvar,sd_yvar,group,method,formula,family, xlab,ylab,title){
  
  ggplot2::ggplot(data, aes(x=xvar, y=yvar, color=group)) +
    ggplot2::geom_point() + 
    ggplot2::geom_errorbar(aes(ymin=yvar-sd_yvar, ymax=yvar+sd_yvar), width=0) +
    ggplot2::geom_errorbarh(aes(xmin=xvar-sd_xvar, xmax=xvar+sd_xvar), width=0) +
    ggplot2::geom_smooth(method = method, formula = formula, family = family, se=F, size = 0.5)+
    ggplot2::labs(y=ylab,x=xlab,title=title) +
    z_theme() 
}

##Execution example of meanregs
#data <- stem_condy
#xvar <- stem_condy$Days_Drying 
#sd_xvar <- 0 #standard deviation or standard error values for xvariable (previously calculated using summarySE function)
#yvar <- stem_condy$Stem_Conductivity_mmol.s.1.MPA.1.m.1
#sd_yvar <- stem_condy$sd #standard deviation or standard error values for xvariable (previously calculated using summarySE function)
#group <- stem_condy$Race #Grouping used for coloring 
#xlab <- 'Days since onset of drought'
#ylab <- 'Stem K (mmol.s.1.MPA.1.m.1)'
#title <- '' #Title of graph. Leave as is if you don't want one
#method <- 'loess' #Type of regression model (eg. loess, lm, glm). Loess useful to observe natural behavior of the data.
#formula <- y~x  #Type of relationship between variables (eg. y~x, y~poly(x,2) for quadratic, poly(x,3) for cubic, etc...)
#family <- NA #Only necessary if using glm as method (eg. gaussian, binomial, etc.)
#meanregs(data,xvar,sd_xvar,yvar,sd_yvar,group,method,formula,family,xlab,ylab,title)




######Extra######
#All the functions above are optimized for easy editting. You can assign the functions to an object to reduce the number
#of lines and add extra features or overide current ones. 

#Example:

#plot <- meanregs(data,xvar,sd_xvar,yvar,sd_yvar,group,method,formula,family,xlab,ylab,title)
#plot + geom_line() #will add an extra line based on the paramiters defined within meanregs
#plot + scale_y_continuous(limits = c(-30,10)) #Will set new range limits for the y axis
#
#plot + geom_line()+ scale_y_continuous(limits = c(-30,10)) #You can add several together!

#Plots a regression bu the points are average values with SD based on groups


######Scatterplot with error bars on points and bigger points, also simpler######
SDregs <- function(data,xvarname,yvarname,groups_vector,group,method,formula,xlab,ylab,title){
  
  dfx <- data_summary(data, var=xvarname,group=groups_vector)
  colnames(dfx)[4] <- "sd.x"
  
  dfy <- data_summary(data, var=yvarname,group=groups_vector)
  colnames(dfy)[4] <- "sd.y"
  
  data_means <- cbind(dfx,dfy[3:4])
  
  ggplot2::ggplot(data_means, aes(x=eval(parse(text = xvarname)), y=eval(parse(text = yvarname)), color=eval(parse(text = group)))) +
    ggplot2::geom_point() +
    ggplot2::geom_pointrange(aes(xmin=eval(parse(text = xvarname))-sd.x, xmax=eval(parse(text = xvarname))+sd.x)) +
    ggplot2::geom_pointrange(aes(ymin=eval(parse(text = yvarname))-sd.y, ymax=eval(parse(text = yvarname))+sd.y)) +
    ggplot2::geom_smooth(method=method, formula = formula, se=F, size = 0.5) +
    ggplot2::labs(y=ylab,x=xlab,title=title) +
    z_theme() +
    ggplot2::theme(axis.line = ggplot2::element_line(size = 0.5, colour = "grey80")) +
    ggplot2::scale_color_manual(values=colors) + ggplot2::theme(legend.position="right")
}


