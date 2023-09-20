library(gbm)
library(dismo)
library(viridis)
library(caret)
library(mlbench)
require(matrixStats)
require(fields)
require(reshape2)
library(gridExtra)

save_dir<-"~/Desktop/NOAA_ERD/Projects/OPC_WhaleSafe/BRT_results/EPAC"

*
* in function below eval_7525_Gaussian Change "d" to response variable Epac,Tspin, TotalKrill 
* as well as else to find and replace 
*



#########Get predicted values based on 75% 25% split
eval_7525_Gaussian <- function(dataInput, gbm.x, gbm.y, lr=lr, tc=tc){
DataInput <- dataInput
DataInput_bound <- floor((nrow(DataInput)/4)*3)         #define % of training and test set
DataInput_train<- DataInput[sample(nrow(DataInput),DataInput_bound),]
DataInput_test<- DataInput[sample(nrow(DataInput),nrow(DataInput)-DataInput_bound),]
DataInput.kfolds <- gbm.fixed(data=DataInput_train, gbm.x= gbm.x, gbm.y = gbm.y, 
                             family="gaussian", tree.complexity=tc,
                             learning.rate = lr, bag.fraction = 0.6,n.trees = 2000 ) #, n.folds=4) this was gbm.step
preds <- predict.gbm(DataInput.kfolds, DataInput_test,
                     n.trees=DataInput.kfolds$gbm.call$best.trees, type="response")
d <- cbind(preds,DataInput_test$E..pacifica) ############## CHANGE EPAC HERE 
colnames(d) <- c("predicted", "observed")
d <- as.data.frame(d)

  pear=cor(d$predicted, d$observed, use="na.or.complete",method = "pearson")
  spear=cor(d$predicted, d$observed, use="na.or.complete",method = "spearman")
 # rmse=sqrt(mean((d$observed - d$predicted)^2)) #RMSE
  ave_diff_obs_pred=mean(abs(d$observed - d$predicted)) #AVE

#dev <- calc.deviance(DataInput_test$KRILL.TOTAL, preds, calc.mean=TRUE, family="gaussian")
######## INTERNAL 
#see https://stats.stackexchange.com/questions/408119/which-method-is-correct-for-calculating-total-deviance-explained-for-boosted-reg and Leathwick et al why you want to use CV deviance. 
#the below method is used if you are cv with GBM STEP - here I am not 
#int.null.deviance = DataInput.kfolds$self.statistics$mean.null
#int.residual.deviance = DataInput.kfolds$cv.statistics$deviance.mean
#int.dev = (int.null.deviance- int.residual.deviance)/int.null.deviance
  null <- DataInput.kfolds$self.statistics$null.deviance
  res <- DataInput.kfolds$self.statistics$resid.deviance
  int.dev=((null - res)/null)*100 

preds_Internal <- predict.gbm(DataInput.kfolds, DataInput_train, n.trees=DataInput.kfolds$gbm.call$best.trees, type="response", family="gaussian")
rmse1<-sqrt(mean((DataInput_train$E..pacifica-preds_Internal)^2)); rmse.int<-rmse1
RMSE_percent_of_max_Internal <-(rmse1/(max(DataInput_train$E..pacifica) - min(DataInput_train$E..pacifica)))*100

ext.residual.deviance = calc.deviance(DataInput_test$E..pacifica, preds, calc.mean=T, family="gaussian")
ext.null.deviance = calc.deviance(DataInput_test$E..pacifica,rep(mean(DataInput_test$E..pacifica),nrow(DataInput_test)), calc.mean=T, family="gaussian") 
ext.dev=(ext.null.deviance - ext.residual.deviance)/ext.null.deviance

rmse<-sqrt(mean((DataInput_test$E..pacifica-preds)^2))
RMSE_percent_of_max <-(rmse/(max(DataInput_test$E..pacifica) - min(DataInput_test$E..pacifica)))*100

#return(c(dev,rmse,RMSE_percent_of_max,ext.dev*100,int.dev*100)) #return(DataInput.kfolds)
return(list(c(rmse,RMSE_percent_of_max,ext.dev*100,rmse.int,RMSE_percent_of_max_Internal, int.dev,pear, spear, ave_diff_obs_pred), DataInput.kfolds)) #dev,
}

########Make function to fit 10 models, and output as a list in rds format
fit.brt.n10_eval <- function(data, gbm.x, gbm.y, lr,tc, iterations){
  Species_Models <- vector("list",10)
  for (i in 1:iterations){
    model <- eval_7525_Gaussian(data=data, gbm.x= gbm.x, gbm.y=gbm.y,lr=lr, tc=tc)
    Species_Models[[i]] <- model    
  }
  return(Species_Models)
}



######################################## LOAD DATA 
setwd("~/Desktop/NOAA_ERD/Projects/OPC_WhaleSafe/datafiles")
load(file="all_krill_vars_matched.Rdata")

setwd('~/Desktop/NOAA_ERD/Projects/ROMs_Data/Elith_Supplement/')
source("brt.functions.R")

model.data2<-all_krill_vars_matched
model.data2$z<-log10(abs(model.data2$z))

#subset to 2002 onwards 
model.data2<-model.data2[which(model.data2$Year>2001),]



#plot distribution of variables
#for(k in 1:ncol(model.data2)){
#dev.new()
#hist(model.data2[,k], main=colnames(model.data2)[k])
#}

#################### LIMIT TO NORTH OF PT CONCEPTION 
#plot(model.data$Longitude, model.data$Latitude)
#map(add=T)
#abline(h =  34.448113)
model.data2<-model.data2[which(model.data2$Latitude > 34.448113),]
model.data<-model.data2


##################################################################################
############################# ALL COMBINED MODEL #####################################################

#PROJECTIONS MODEL 
##### REMOVE DIST 2 CANYON from all - doesnt contribute a lot to these models or published model
#published_no_winter_but_addSpringWinterVars_noCHL<-c(  "z_sd", "Distance_from_shore", #"distance_to_canyon",
# "su_05" ,"sv_05","svstr_05" ,"bf_05" , "ssh_05", "ild_05", "curl_05", "sst_05") 
#projections model does not have sst_sd, ssh_sd, EKE , sustr, curl z 


may_vars_all <-c(   "ssh",  "ssh_sd_0.3", "sst_sd_0.3" , "sst" , "su" ,"sustr","sv" ,"svstr" ,"ild"              ,"bv" , "curl" , "z_sd", "z" ,  "Distance_from_shore" )
#remove EKE - not sure if calculate is correct or accurate across the ROMS data that I have 

may_vars_trimmed <-c( "z" ,  "z_sd",  "Distance_from_shore" ,"ild", "curl","su","svstr","bv")

published<- c("z_sd" , "Distance_from_shore" , "distance_to_canyon" , "ild" , "curl"  , "su" , "svstr", "bv") #didnt use chl 

############################ set dataset and model name  
for (cc in 1:3){
print(cc)
model_to_run<- cc  

  # variable names above that corrsepond to # above this
var_tested<-list(may_vars_all,may_vars_trimmed, published )[[model_to_run]]


model_name<- c( "may_vars_all","may_vars_trimmed","published")[model_to_run]
folder_name <- c( "may_vars_all","may_vars_trimmed","published")[model_to_run]
var_ind<-which(colnames(model.data2) %in% var_tested ==T)


####### i am running 50 iterations of  75/25 cv

########################################## run models 
krill_col_numb<-3
iters<-50 # DO 50 
model_eval<- fit.brt.n10_eval(model.data2, gbm.x=var_ind, gbm.y =krill_col_numb, lr= .003, tc=3, iterations= iters)
model_iterations<-model_eval; combined_models<-model_iterations
evalvars<-NULL
for(k in 1:iters){ 							#this was 50 
evalvars<-rbind(evalvars, rbind(model_iterations[[k]][1][[1]]))
}; eval_df= evalvars
eval1<- (paste(round(colMeans(eval_df),2), round(colSds(as.matrix(eval_df)),2), sep=" ± "))#[-1]
eval1<-cbind(eval1)
rownames(eval1)<-c( "ext_rmse","ext_rmse_percent_of_max","ext_dev","int_rmse","int_rmse_percent_of_max","int_dev","PearsonCorr","SpearmanCorr","Diff_btwn_Obs_and_Preds")
combined_eval<-eval1; print(eval1)

########## pull out data from partial plots
part_plot<-list()
percent_contrib<-NULL #list()
for(q in 1:iters){                                #this was 50 
mod<-model_iterations[[q]][2][[1]]  ###
part_plot1<-data.frame(row.names=1:100)
for(x in 1:length(var_tested)){ ###
pp<-plot(mod ,var_tested[x],return.grid=T) ###
part_plot1<-cbind(part_plot1, pp) ###
} ###
part_plot[[q]]<-part_plot1 ###

sum1<-summary(model_iterations[[q]][2][[1]] , plot=F )
sum2<-sum1[order(sum1[,1], levels = var_tested),]
percent_contrib<-cbind(percent_contrib, sum2[,2])
rownames(percent_contrib)<-sum1[order(sum1[,1], levels = var_tested),1]
}
All_percent_contribution<-cbind(rownames(percent_contrib), paste(round(rowMeans(percent_contrib),2), round(rowSds(percent_contrib),2), sep=" ± "))
Combined_All_percent_contribution<-All_percent_contribution



setwd(paste(save_dir, folder_name,sep="/"))

########### PLOT partial plots 
#setwd(results_wd)
pdf(paste(model_name, "_partial_plots.pdf", sep=""), 9,6)
par(mfrow=c(3,2))
mn_part_plot<-list()  
for(y in 1:length(var_tested)){
id<-which(colnames(part_plot[[1]])==var_tested[y])
all1<-NULL
all2<-NULL
for(z in 1:iters){											 #this was 50 
all1<-rbind(all1, cbind(c(part_plot[[z]][,id])))
all2<-rbind(all2, cbind(c(part_plot[[z]][,id+1])))
}
all3<-cbind(all1, all2)
all1<-all3[order(all3[,1]),]

plot(all1, xlab=var_tested[y], col="white", ylab=paste("f(",var_tested[y], ")", sep=""),cex.axis=1.2, cex.lab=1.2) #, ylim=c(-8,2))
plx<-predict(loess(all1[,2] ~ all1[,1], span = 0.3), se=T)
mn_part_plot[[y]]<- cbind(all1[,1], plx$fit)      
lines(all1[,1],plx$fit)
lines(all1[,1],plx$fit - qt(0.975,plx$df)*plx$se, lty=2)#0.975
lines(all1[,1],plx$fit + qt(0.975,plx$df)*plx$se, lty=2)
legend("bottomright", paste(All_percent_contribution[which(All_percent_contribution[,1]==var_tested[y]),2],"%", sep=" "), bty="n", cex=1.4)
}
dev.off()

#### save tables as image for quick review 
p<-tableGrob(combined_eval)
png("Eval_Stats.png", height=50*nrow(p), width = 200*ncol(p)) # height=500, width=500)
grid.arrange(p , top = model_name)
dev.off()


colnames(Combined_All_percent_contribution)<-c("Variable", "Mean+-SD Contribution")
p<-tableGrob(Combined_All_percent_contribution)
png("VariablePercentContrib.png", width=50*nrow(p), height = 200*ncol(p))
grid.arrange(p , top = model_name)
dev.off()
####################


#setwd(save_wd)
save(combined_models, file="combined_models.Rdata")

save(combined_eval, file="combined_eval.Rdata")
save(Combined_All_percent_contribution, file="Combined_All_percent_contribution.Rdata")
Combined_mn_part_plot<-mn_part_plot ; save(Combined_mn_part_plot, file="Combined_mn_part_plot.Rdata")
}
#################################
















