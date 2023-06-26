# Code for mosaic analysis of Continuous Variables

# Read me ----
# This code can be used to analyze data of humans and animals, and therefore uses the category names ‘female’ and ‘male’. You can use sex or gender categories. 
# This code excepts data files of the following structure: 
    #The first column should be named "Group" and specify "F" for female, and "M" for male participants. 
    #Besides the first column, all other columns should be of continuous numerical variables. 
# For questionnaire data, please use the ‘Code for mosaic analysis of questionnaire data’.

#The following three functions are needed to conduct a mosaic analysis: 

#1.Cohens_D_dist(Data):
  #Input variable: "Data" - your data (should follow the instructions above).
  #Result: this function returns a histogram of the distribution of Cohen's D values and a datafrarme with the Cohen's D values.
  # This will help you choose the variables you wish to include in the mosaic analysis. In principle, you should include variables with large sex/gender differences (|Cohen’s d| > 0.5).


#2.General_Mosaic(Data,CohensD_Min,Percentage=0.33): generates the gender mosaic figure from a given data set.
  #Input variables:
  #"Data": your data (should follow the instructions above).
  #"CohensD_Min": the minimum cohen’s D (absolute value) that you want to include in the mosaic; has to be at least 0.5
  #"Percentage" (Default = 0.33): The percentage that would be used as a cutoff to determine the "Female-typical" and "Male-typical" scores for each variable.

#Results:
  # According to the minimum cohen’s D value and percentage given, this function will return a figure presenting for each male and each female whether they are "Female-typical", "Male-typical", or neutral, on each of the variables included in the mosaic analysis.
  # Notice: there are four colors in each mosaic : Blue for Male-typical, Pink for Female-typical, Grey for neutral and White for missing values.

#3.Internally_consistent_function(Mosaic_M,Mosaic_F,allow_nas=0):  
  #Input variables: a."Mosaic_M" and "Mosaic_F" that was returned from the General_Mosaic function (the mosaic tables that were used to create the figure).
    #b."allow_nas"(Default = 0): number of missing values allowed for a row (observation) to still be considered internally consistent.
  # Result: a.number of internally consistent males and females, meaning individuals that were either Male-typical on all variables or Female-typical on all variables.
    #b.number of mosaic males and females, meaning individuals that have at least one Male-typical variable and one Female-typical variable. 


#libraries----
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(dplyr)
library(lattice)
library(lsr)

#Functions----
Cohens_D_dist = function(Data){
  # Setup
  Number_of_Variables = ncol(Data) - 1
  Variables_Names = colnames(Data[,-1]) 
  
  # Creation of subgroups 
  Data_total = Data[,-1] #Data without Group and 
  Data_M = Data_total[Data[,'Group']=="M",] #Males data
  Data_F = Data_total[Data[,'Group']=="F",] #Females data
  
  #Convert all data to numeric values
  Data_total <- apply(Data_total, 2, as.numeric)
  Data_M <- apply(Data_M, 2, as.numeric)
  Data_F <- apply(Data_F, 2, as.numeric)
  
  #calculating Cohen's d value for each variable
  Cohens_D=rep(NA,length(Number_of_Variables))
  for (i in 1:Number_of_Variables){
    Cohens_D[i]=cohensD(Data_M[,i],Data_F[,i])
  }
  #plotting the Cohen's d distribution
  Cohen_D_df = data.frame(Variables_Name=Variables_Names ,  Cohens_D_value=Cohens_D)
  plot = ggplot(Cohen_D_df,aes(x=Cohens_D_value)) + geom_histogram(binwidth=.2,color="black", fill="white")+
    ggtitle("Cohen's d Distribution") + xlab("Cohen's d") + ylab("Count")


  return(list(plot,Cohen_D_df))
}

#----

General_Mosaic = function(Data,CohensD_Min,Percentage=0.33){
  
  # Setup
  Number_of_Variables = ncol(Data) - 1
  Variables_Names = colnames(Data[,-1]) 
  
  # Creation of subgroups 
  Data_total = Data[,-1] #Data without Group  
  Data_M = Data_total[Data[,'Group']=="M",] #Males data
  Data_F = Data_total[Data[,'Group']=="F",] #Females data
  
  #Convert all data to numeric values
  Data_total <- apply(Data_total, 2, as.numeric)
  Data_M <- apply(Data_M, 2, as.numeric)
  Data_F <- apply(Data_F, 2, as.numeric)
  
  # Calculate absolute value of the Cohen's D for the differences between Males and Females 
  Cohens_D=rep(NA,length(Number_of_Variables))
  for (i in 1:Number_of_Variables){
    Cohens_D[i]=abs(cohensD(Data_M[,i],Data_F[,i]))}
  
  #check the Cohen's D input value
  if (CohensD_Min<0.5){
    return("Selected Cohen's D is smaller than 0.5, please select a bigger one")}
  
  if (max(Cohens_D)<CohensD_Min){
    return("Selected Cohen's D is too big, please select a smaller one")}
  

  #Calculate the values for the males' and females' region limits
  males_mean = vector(mode = 'numeric', length = Number_of_Variables)
  females_mean =vector(mode = 'numeric', length = Number_of_Variables)
    
  # Calculation of average values of females and Males for each variable
  for (i in c(1:Number_of_Variables)) {
    males_mean[i] = mean(Data_M[,i],na.rm = TRUE)
    females_mean[i] = mean(Data_F[,i],na.rm = TRUE)
    }
    
    # We have 2 possibility for each variable :
      # (1) : Males' average is greater than Females' average 
            # --> In this case, we calculate the males' limit using the value corresponds to the  (1-percentage)*100th males' percentile
            #and the females' limit using the value corresponds to the (percentage)*100th females' percentile
     
     # (2) :  Females' average is greater than Males' average
            # --> In this case, we calculate the males' limit using the value corresponds to the (percentage)*100th males' percentile 
            #and the females' limit using the value corresponds to the (1-percentage)*100th females' percentile
    

    
  males_region_limits = vector(mode = 'numeric', length = Number_of_Variables)
  females_region_limits = vector(mode = 'numeric', length = Number_of_Variables)
    
  for (i in c(1:Number_of_Variables)) {
    if(males_mean[i]>females_mean[i]){
      males_region_limits[i]= sort(na.omit(Data_M[,i]),decreasing=TRUE)[round(length(na.omit(Data_M[,i]))*Percentage)]
      females_region_limits[i] = sort(na.omit(Data_F[,i]))[round(length(na.omit(Data_F[,i]))*Percentage)]
      if (males_region_limits[i]<=females_region_limits[i]){
        return("Warning! Regions limits intersect, try a smaller percentage")}
    }
    
    if(females_mean[i]>males_mean[i]){
      males_region_limits[i]= sort(na.omit(Data_M[,i]))[round(length(na.omit(Data_M[,i]))*Percentage)]
      females_region_limits[i] = sort(na.omit(Data_F[,i]),decreasing=TRUE)[round(length(na.omit(Data_F[,i]))*Percentage)]
      if (males_region_limits[i]>=females_region_limits[i]){
        return("Warning! Regions limits intersect, try a smaller percentage")}
    }
  }
    

  # Creating the Mosaic 
  Mosaic= as.data.frame(matrix(NA,nrow = nrow(Data_total),
                                 ncol = (Number_of_Variables+1)))
  colnames(Mosaic)=c('Group',Variables_Names)
  Mosaic[,1]= Data$Group
    
  #Classifying the observations as -1=Female,1=Male,0=Neutral or missing data, according to the Male and Female limits
  for (i in c(1:Number_of_Variables)) {
    if(males_mean[i]>females_mean[i]){
      Mosaic[,i+1] = ifelse(Data_total[,i]>=males_region_limits[i],1,
                              ifelse(Data_total[,i]<=females_region_limits[i],-1,0))}
    if(females_mean[i]>males_mean[i]){
      Mosaic[,i+1] = ifelse(Data_total[,i]<=males_region_limits[i],1,
                              ifelse(Data_total[,i]>=females_region_limits[i],-1,0))}
    }
    
  #Preparing the data for the gender mosaic figure
  Mosaic_M = Mosaic[Mosaic$Group=="M",-1]
  Mosaic_F = Mosaic[Mosaic$Group=="F",-1]
  rownames(Mosaic_M) <- 1:nrow(Mosaic_M)
  rownames(Mosaic_F) <- 1:nrow(Mosaic_F)
    
  colors <- colorRampPalette(c("#FF99FF", "#CCCCCC", "white", "#0099FF"))
    
  #males mosaic
  M_M = levelplot(t(Mosaic_M[nrow(Mosaic_M):1,c(which(Cohens_D>CohensD_Min))]),col.regions = colors,asp=2,at=seq(-1, 1, by = 0.2),cut=3,xlab="Variables",
                      ylab=list(label = "Participants", vjust = 1.5), main=list(label = "Male Mosaic", hjust = 0.8),
                      colorkey=list(height = 0.8, width=0.6, space="right",at=seq(-1, 1, by = 0.5),
                      labels=list(at=c(-0.75, -0.25, 0.25, 0.75),labels=c("Female", "Intermediate", "NA", "Male"))),
                      scales=list(x=list(rot=60,cex=.5,alternating=2),y=list(alternating=2),tck = c(0,1)))
  #females mosaic
  M_F = levelplot(t(Mosaic_F[nrow(Mosaic_F):1,c(which(Cohens_D>CohensD_Min))]),col.regions = colors,asp=2,at=seq(-1, 1, by = 0.2),
                      cut=3,xlab="Variables",ylab=list(label = "Participants", vjust = 1.5), main=list(label = "Female Mosaic", hjust = 0.8),
                      colorkey=list(height = 0.8, width=0.6, space="right",at=seq(-1, 1, by = 0.5),
                      labels=list(at=c(-0.75, -0.25, 0.25, 0.75),labels=c("Female", "Intermediate", "NA", "Male"))),
                      scales=list(x=list(rot=60,cex=.5,alternating=2),y=list(alternating=2),tck = c(0,1)))
  #combined plot  
  plot=grid.arrange(M_M, M_F, ncol=2, nrow = 1,top=textGrob(paste("Mosaic Using",  Percentage*100, "% as Cutoff", sep=" ")))
  return(list(plot,Mosaic_M,Mosaic_F))} 
    
#---- 
  
Internally_consistent_function = function(Mosaic_M,Mosaic_F,allow_nas = 0){
  
  check_row_internally_consistent= function(row,allow_nas){ 
    #checks if a row is internally consistent, allowing "allow_nas" number of missing values  
    if (sum(is.na(row))<=allow_nas){
      all(row[!is.na(row)] == 1) || all(row[!is.na(row)] == -1)}
    else {FALSE}
  }
  
  consistent_Female = sum(apply(Mosaic_F, 1, FUN=function(row) check_row_internally_consistent(row, allow_nas=allow_nas))) #all 'male' or all 'female'
  consistent_Male <- sum(apply(Mosaic_M, 1, FUN=function(row) check_row_internally_consistent(row, allow_nas=allow_nas))) #all 'male' or all 'female'
  Mosaic_check_Male <- sum(apply(Mosaic_M, 1, function(row) any(row == 1) && any(row == -1)),na.rm = TRUE) #some 'male' and some 'female'
  Mosaic_check_Female <- sum(apply(Mosaic_F, 1, function(row) any(row == 1) && any(row == -1)),na.rm = TRUE) #some 'male' and some 'female'
  
  return(c("Internally consistent Females:"=consistent_Female,
          " Internally consistent Males:"=consistent_Male,
          " Mosaic Female participants:"=Mosaic_check_Female,
          " Mosaic Male participants:"=Mosaic_check_Male))
}

#----

#Using the functions above the create your gender mosaic:

df <- #your data
Cohens_D_dist(df) #Looking at the Cohen's D values

results <- General_Mosaic(Data=df,CohensD_Min=0.5,Percentage=0.33) #creating the mosaic
results[[1]] #if no figure was created,check if the function returned a warning
Mosaic_M = results[[2]]
Mosaic_F = results[[3]]

#mosaic summary: number of internally consistent and mosaic participants
Internally_consistent_function(Mosaic_M,Mosaic_F,allow_nas = 0)  




