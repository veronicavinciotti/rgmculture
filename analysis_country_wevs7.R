#WVS7+EVS7 - 90 COUNTRIES
setwd("G:/My Drive/Collaborations/ErnstWit/CultureRgm/country")
library(tidyverse)
#For color palettes: https://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3

dat1 <- readr::read_rds("EVS_WVS_Joint_Rds_v4_0.rds")
length(table(dat1$cntry))

load("EVS_WVS_Joint_rData_v4_0.RData")
WVS7<-EVS_WVS_Joint_v4_0_Rworkspace
dim(WVS7)

#missing codes for questions (does not apply to indices)
#-1 Don't know 
#-2 No answer/refused 
#-3 Not applicable (filter)
#-4 Not asked in that Wave
#-5 Missing; Not applicable for other reasons


#number of countries: 90
length(table(WVS7$cntry))
unique(WVS7$cntry)
unique(dat1$cntry)
unique(WVS7$cntry_AN)
names(attributes(dat1$cntry)$labels)

##SELECT CULTURAL TRAITS
# ------------ Compute "autonomy index" (Y003)
table(WVS7$Y003) #not available for European study
#rebuilding it from scratch
a<-rep(-5,length=nrow(WVS7))
b<- -(WVS7$A040 + WVS7$A042)+(WVS7$A029 +WVS7$A039)
a[WVS7$A040 >=0 & WVS7$A042 >=0 & WVS7$A029 >=0 & WVS7$A039 >=0]<-b[WVS7$A040 >=0 & WVS7$A042 >=0 & WVS7$A029 >=0 & WVS7$A039 >=0]
WVS7$Y003<-a
table(WVS7$Y003)

#an index whose lowest value is -2, so values below this are missing
WVS7$Y003R <- WVS7$Y003 + 3


# ------------ # Recoding "Proud of Nationality" (G006)
table(WVS7$G006)
#5: I'm not of that nationality (it does not make the variable ordinal, but it does not feature in the data)

##Possible variables for marginal model
#Sex
table(WVS7$X001) #1. Male 2. Female

#Age
table(WVS7$X003)
WVS7$X003[WVS7$X003 < 18] <- -6 #will be recoded as NA later on

# ------------ Select Cultural Map variables (and rename missing values to NA)

WVS7 <- WVS7 %>%
  dplyr::select(cntry,cntry_AN,A008,A165,E018,E025,F063,F118,F120,G006,Y002, Y003R,X001, X003) %>% 
  mutate(across(everything(), function(x){replace(x, which(x<0), NA)})) %>% 
  rename("level of happiness" = A008,
         "trust in people" = A165,
         "respect for authority" = E018,
         "voice through petitions" = E025,
         "importance of God" = F063,
         "justification of homosexuality" = F118,
         "justification of abortion" = F120,
         "national pride" = G006,
         "post-materialism" = Y002,
         "obedience vs independence" = Y003R,
          "gender" =  X001,
          "age" = X003)


# A list with one data matrix for each country
country_matrix <- list()
country_names<-NULL
country_iso2<-NULL
for (i in 1:length(table(WVS7$cntry))){
  country_names[i]<-names(attributes(dat1$cntry)$labels)[which(attributes(dat1$cntry)$labels==unique(WVS7$cntry)[i])]
  country_iso2[i]<-WVS7$cntry_AN[WVS7$cntry == as.numeric(unique(WVS7$cntry))[i]][1]
  country_matrix[[i]] <- WVS7 %>% filter(cntry == as.numeric(unique(WVS7$cntry))[i]) %>% dplyr::select(-c(cntry,cntry_AN))
}


names(country_matrix)<-country_names
nr<-lapply(country_matrix,nrow)
#sample size per country
data.frame(country_names,as.numeric(nr))
#missing values per questions per country
pm<-matrix(0,nrow=length(country_matrix),ncol=ncol(country_matrix[[1]]))
for(i in 1:length(country_matrix))
{
  pm[i,]<-apply(country_matrix[[i]],2,function(x) sum(is.na(x))/length(x)*100)
}
rownames(pm) <-country_names
colnames(pm)<-colnames(country_matrix[[1]])
round(pm,3)
apply(pm,2,max)
which(pm[,5]==1)
which(pm[,6]==1)
#Iraq: no answers on God; Egypt and Takjikistan: no answer on homosexuality
#Serbia, Montenegro and Northern Ireland also excluded, as they have no distance data
country_matrix<-country_matrix[!(country_names %in% c("iraq","egypt","tajikistan", "serbia", "northern ireland","montenegro"))]
country_iso2<- country_iso2[!(country_names %in% c("iraq","egypt","tajikistan", "serbia", "northern ireland","montenegro"))]
country_names<- country_names[!(country_names %in% c("iraq","egypt","tajikistan", "serbia", "northern ireland","montenegro"))]
length(country_matrix)

country_matrix<-country_matrix[order(country_names)]
country_iso2<-country_iso2[order(country_names)]
country_names<-country_names[order(country_names)]
country_names[country_names=="hong kong sar"]<-"hong kong"
country_names[country_names=="macau sar"]<-"macao"
country_names[country_names=="taiwan roc"]<-"taiwan"


####OTHER DATA (DISTANCES)
library(readxl)
library(dplyr)
iso2 <- read_excel("iso_alpha_2.xls")
head(iso2)
iso2$country_name[iso2$country_name=="Great Britain"]<-"United Kingdom"
iso2.sel<-data.frame(countryname=iso2$country_name[match(country_iso2,iso2$iso2)],iso2=country_iso2)
iso2.sel
iso2.sel[country_names=="Libya",2] <- "LY"
iso2.sel[country_names=="Palestine",2] <- "PS"
iso2.sel[country_names=="Trinidad",2] <- "TT"
iso2.sel[country_names=="Czechia",2] <- "CZ"
dim(iso2.sel)
head(iso2.sel)

geo_cepii <- read_excel("geo_cepii.xls")

geo_cepii_nodup <- geo_cepii[!duplicated(geo_cepii[,c('iso2')]),]
iso3 <- geo_cepii_nodup %>% dplyr::select(iso2, iso3)
iso23.sel <- left_join(iso2.sel, iso3, by = c("iso2" = "iso2"))
iso23.sel
dim(iso23.sel)
head(iso23.sel)
countryname<-iso23.sel$countryname
countryname[11]<-"Bosnia-Herzegovina"
countryname[45]<-"Libya"

#######################################################
###MARGINAL MODELS: ORDINAL REGRESSION ################
library(MASS)
library(AER)
par.mar<-list() #list of matrices of cdfs cutpoints per country (rows:observations, columns:cultural traits)
ct<-10 #number of cultural traits
coefmat<-matrix(0,nrow=length(country_matrix),ncol=2*ct)
for(i in 1:length(country_matrix))
{
  dat<-data.frame(country_matrix[[i]])
  tup<-matrix(NA,nrow=nrow(dat),ncol=ct)
  tdown<-matrix(NA,nrow=nrow(dat),ncol=ct)
  for(j in 1:ct)
  {
    y<-factor(dat[,j],ordered=TRUE)
    age<-dat$age
    gender<-dat$gender
    dat.yx<-data.frame(y=y,age=age,sex=gender)
    a<-apply(dat.yx,1,function(x) sum(is.na(x)))
    if(length(table(y))>2){
      or<-polr(y~age+sex,data=dat.yx)
      or.ft<-matrix(NA,nrow=length(y),ncol=length(table(y)))
      or.ft[a==0,]<-or$fitted.values #probabilities per category
      coefmat[i,c(j,j+ct)]<-c(coeftest(or)[1,1]/coeftest(or)[1,2],coeftest(or)[2,1]/coeftest(or)[2,2])
      for(k in 1:length(y))
      {
        if(a[k]==0)
          tup[k,j]<-stats::qnorm(sum(or.ft[k,1:as.numeric(y[k])])) #upper limits of interval
        if(a[k]==0 & as.numeric(y[k])!=1)
          tdown[k,j]<-stats::qnorm(sum(or.ft[k,1: (as.numeric(y[k])-1)])) #lower limits of interval
      }
    }
    else
    {
      or<-glm(y~age+sex,data=dat.yx, family=binomial)
      b<-stats::qnorm(1-fitted(or,"probs"))
      tup[a==0 & y == as.numeric(levels(y)[[1]]),j] <-b[y[a==0] == as.numeric(levels(y)[[1]])]
      tdown[a==0 & y == as.numeric(levels(y)[[2]]),j] <-b[y[a==0] == as.numeric(levels(y)[[2]])]
      coefmat[i,c(j,j+ct)]<-c(coeftest(or)[2,1]/coeftest(or)[2,2],coeftest(or)[3,1]/coeftest(or)[3,2])
    }
  }
  tup[is.na(tup) | is.infinite(tup)]<-stats::qnorm((nrow(tup)+.5)/(nrow(tup)+1))
  tdown[is.na(tdown)]<- stats::qnorm(.5/(nrow(tup)+1))
  par.mar[[i]]<-list(tup=tup,tdown=tdown)
}

#rownames(coefmat)<-country_names
rownames(coefmat)<-countryname
colnames(country_matrix[[1]])[1:ct]
ct.names<-c("H","T","R","V","G","O","A","P","M", "B")
colnames(coefmat)<-c(paste(ct.names,"-age",sep=""),paste(ct.names,"-gender", sep=""))

####EXAMPLE ON THE IMPORTANCE OF MARGINAL CORRECTION
library("gplots")
pdf("betacoef.pdf")
a<-heatmap.2(coefmat, scale = "none", col = colorpanel(100,"#0571b0", "#f7f7f7","#ca0020"), key=FALSE,
            Colv = FALSE, trace = "none", density.info = "none",margins=c(8.2,4.5),dendrogram = "none",cexRow =0.45, lwid=c(0.1,5), lhei=c(0.1,5))
dev.off()
a<-heatmap.2(coefmat, scale = "none", col = colorpanel(100,"#0571b0", "#f7f7f7","#ca0020"),
             Colv = FALSE, trace = "none", density.info = "none",margins=c(6.5,4),dendrogram = "none",cexRow =0.3,cexCol = 0.7)

i<-16 #China
i<-75 #Taiwan
i<-32 # Hong Kong
dat<-data.frame(country_matrix[[i]])
age.q<-quantile(dat$age,c(0.25,0.5,0.75),na.rm=TRUE)
age.ct<-NULL
age.ct[dat$age<age.q[1] | dat$age == age.q[1]]<-"young"
age.ct[dat$age<age.q[2] & dat$age>age.q[1]]<-"medium"
age.ct[dat$age> age.q[3] | dat$age == age.q[3]]<-"old"
age.ct<-factor(age.ct,levels=c("young","medium","old"))
dat$agecat<-age.ct
y<-dat$justification.of.homosexuality
x<-dat$justification.of.abortion
dat.p<-data.frame(y=y,x=factor(x))
library(ggplot2)
pdf("homoaboHK.pdf")
ggplot(data=subset(dat.p,!is.na(x)), aes(x=x, y=y)) +labs(x="justification of abortion",y="justification of homosexuality") +geom_boxplot(alpha=0.6,position=position_dodge(0.8),coef=0,outlier.shape=NA,outlier.size=0, notch=FALSE) +ggtitle("Hong Kong") + theme_classic()+theme(axis.title =element_text(size=20),plot.title = element_text(size = 20, face = "bold"))+ theme(legend.position='none')
dev.off()
y<-c(dat$justification.of.homosexuality,dat$justification.of.abortion)
x<-c(age.ct,age.ct)
type<-factor(c(rep("homosexuality",nrow(dat)),rep("abortion",nrow(dat))),levels=c("homosexuality","abortion"))
dat.p<-data.frame(y=y,x=x,type=type)
pdf("homoaboHK-age.pdf")
ggplot(data=subset(dat.p,!is.na(x)), aes(x=x, y=y, fill=type)) +labs(x="age",y="justification") +geom_boxplot(position=position_dodge(0.8),coef=0,outlier.shape=NA,outlier.size=0, notch=FALSE) +ggtitle("Hong Kong")+ scale_fill_manual(values=c("#bdd7e7","#08519c"
)) + theme_classic()+theme(plot.title = element_text(size = 20, face = "bold"))+ theme(axis.title =element_text(size=20),legend.position='top',legend.text=element_text(size=20),legend.title=element_blank())
dev.off()


idx<-which(age.ct=="young")
cor(dat$justification.of.homosexuality[idx],dat$justification.of.abortion[idx],use="complete.obs")
idx<-which(age.ct=="medium")
cor(dat$justification.of.homosexuality[idx],dat$justification.of.abortion[idx],use="complete.obs")
idx<-which(age.ct=="old")
cor(dat$justification.of.homosexuality[idx],dat$justification.of.abortion[idx],use="complete.obs")


##sample configuration
cm<-NULL
for(i in 1:length(country_matrix))
{
  cm[[i]]<-cbind(rep(countryname[i],nrow(country_matrix[[i]])),country_matrix[[i]])
}
a<- as.data.frame(do.call(rbind,cm))
colnames(a)[1]<-"countryname"
a<-as.data.frame(a)
#age
pdf("age.pdf",width=16,height=12)
ggplot(data=subset(a,!is.na(age)), aes(x=countryname, y=age)) +labs(x="",y="age of respondent")+geom_boxplot(alpha=0.6,position=position_dodge(0.8),coef=0,outlier.shape=NA,outlier.size=0, notch=FALSE) +ggtitle("Survey Sample Configuration: Age") + theme_classic()+theme(plot.title = element_text(size = 30, face = "bold"),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),axis.title.y =element_text(size=30),aspect.ratio=12/16)+ theme(legend.position='none')
dev.off()
ag.med<-NULL
for(i in 1:length(unique(a$countryname)))
  ag.med<-c(ag.med,median(a$age[a$countryname==unique(a$countryname)[i]],na.rm=TRUE))
mean(a$age[!is.na(a$age)])
sum(is.na(a$age))/length(a$age)*100
ag.mean<-NULL
for(i in 1:length(unique(a$countryname)))
  ag.mean<-c(ag.mean,mean(a$age[a$countryname==unique(a$countryname)[i]],na.rm=TRUE))
summary(ag.mean)
ag.miss<-NULL
for(i in 1:length(unique(a$countryname)))
{
  b<-a$age[a$countryname==unique(a$countryname)[i]]
  ag.miss<-c(ag.miss,sum(is.na(b))/length(b)*100)
}
min(ag.miss)
max(ag.miss)

dat<-data.frame(countryname,ag.med)
dat[which.min(dat[,2]),1]
dat[which.max(dat[,2]),1]
min(ag.med)
max(ag.med)

#gender
mean(a$gender[!is.na(a$gender)])
sum(is.na(a$gender))/length(a$gender)*100
g.mean<-NULL
for(i in 1:length(unique(a$countryname)))
  g.mean<-c(g.mean,mean(a$gender[a$countryname==unique(a$countryname)[i]],na.rm=TRUE))
summary(g.mean)
g.miss<-NULL
for(i in 1:length(unique(a$countryname)))
{
  b<-a$gender[a$countryname==unique(a$countryname)[i]]
  g.miss<-c(g.miss,sum(is.na(b))/length(b)*100)
}
min(g.miss)
max(g.miss)


a<-a[!is.na(a$gender),]
Gender<-ifelse(a$gender=="1", "male","female")
a<-cbind(a,Gender)
colnames(a)[ncol(a)]<-"Gender"
dim(a)
a.gender<-NULL
for(i in 1:length(unique(a$countryname)))
  a.gender<-c(a.gender,rep(table(a$Gender,a$countryname)[1,i]/(table(a$Gender,a$countryname)[1,i]+table(a$Gender,a$countryname)[2,i]),each=table(a$countryname)[i]))
a$gender<-a.gender
pdf("gender.pdf",width=16,height=12)
ggplot(data=a, aes(fill=Gender,x=countryname, y=gender)) +labs(x="",y="gender of respondent")+scale_fill_manual(values=c("#bdd7e7","#08519c"))+geom_bar(position="fill", stat="identity") +ggtitle("Survey Sample Configuration: Gender") + theme_classic()+theme(plot.title = element_text(size = 30, face = "bold"),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),axis.title.y =element_text(size=30), legend.text=element_text(size=30),aspect.ratio=12/16,legend.title= element_blank())
dev.off()



##############################################
#####RGM approach
#############################################
#devtools::install_github("franciscorichter/rgm")
data<-country_matrix
for(i in 1:length(data))
  data[[i]]<-data[[i]][,1:ct] #only cultural traits

##histograms of cultural traits
a<- as.data.frame(do.call(rbind,data))
i<-10
colnames(data[[1]])[i]
tb<-table(a[,i])
dat<-data.frame(x=as.numeric(rownames(tb)),y=as.numeric(tb))
pdf("Obedience.pdf")
ggplot(dat, aes(x=x, y=y)) + geom_bar(stat="identity", width=0.95,fill="blue")+theme_classic()+theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank(),axis.line=element_blank())+xlab("")+ylab("")
dev.off()

##summary statistics of each cultural trait
a<- as.data.frame(do.call(rbind,data))
apply(a,2,mean,na.rm=TRUE)

ct.minmax<-matrix(0,nrow=length(data),ncol=ncol(data[[1]]))
for(i in 1:length(data))
{
  ct.minmax[i,]<-apply(data[[i]],2,mean,na.rm=TRUE)}
colnames(ct.minmax)<-colnames(data[[1]])
apply(ct.minmax,2,min)
apply(ct.minmax,2,max)

##missing data for each cultural trait
pm<-matrix(0,nrow=length(data),ncol=ncol(data[[1]]))
for(i in 1:length(data))
{
  pm[i,]<-apply(data[[i]],2,function(x) sum(is.na(x))/length(x))
}
colnames(pm)<-colnames(data[[1]])
apply(pm,2,min)
apply(pm,2,max)
country_names[apply(pm,2,which.max)]
a<- as.data.frame(do.call(rbind,data))
apply(a,2,function(x) sum(is.na(x))/length(x))


#missing values per questions per country
pm<-matrix(0,nrow=length(data),ncol=ncol(data[[1]]))
for(i in 1:length(data))
{
  pm[i,]<-apply(data[[i]],2,function(x) sum(is.na(x))/length(x))
}
rownames(pm) <-countryname
colnames(pm)<-colnames(data[[1]])

pm.av<-apply(pm,1,mean)
summary(pm.av*100)
sort(pm.av*100)

#####GRAPHICAL MODELLING ######

library(rgm)

##initialize graphs outside of rgm
p<-ncol(data[[1]])
m<-matrix(1:p,ncol=p,nrow = p)
lt<-lower.tri(m)
initial.graphs<-matrix(0,nrow=p*(p-1)/2,ncol=length(data))
for(i in 1:length(data))
{
  row.c<-apply(data[[i]],1,sum)
  g<-huge.select(huge(as.matrix(data[[i]][!is.na(row.c),]),method="glasso"),criterion="stars")$refit
  initial.graphs[,i]<-g[lt]
}
#save.image("data_marg_inig_wevs7.RData")

####################################################
#####MODEL 1: intercept only #######################
####################################################

setwd("G:/My Drive/Collaborations/ErnstWit/CultureRgm/country")
load("data_marg_inig_wevs7.RData")

library(rgm)
source("rgm.culture.R")
source("Gmcmc.culture.R")

iter<-100000 #outer iterations for LSM
bd.iter<-20 #inner iterations for bdgraph
res<-rgm.culture(data, X=NULL, L=FALSE, bd.iter=bd.iter, iter=iter,initial.graphs=initial.graphs,method="gcgm",gcgm.marg="ordinal",gcgm.par =par.mar)
#save.image("country-rgm-wevs7-intercept.RData") #only intercept

setwd("G:/My Drive/Collaborations/ErnstWit/CultureRgm/country")
library(rgm)
library(ggplot2)
load("country-rgm-wevs7-intercept.RData") 
####plot
iter<-ncol(res$sample.alpha)
iter
burn<-floor(0.25*iter)
burn
sample.graphs<-res$sample.graphs[,,-(1:burn)]
sample.alpha<-res$sample.alpha[,-(1:burn)]
post.pi<-res$sample.pi[,,-(1:burn)]
probit.pi<-res$pi.probit[,,-(1:burn)]
sample.K<-res$sample.K[,,-(1:burn)] 

# some plots
alpha.est<-apply(sample.alpha,1,mean)
alpha.est

plot(sample.alpha[1,],type="l")
plot(sample.alpha[5,],type="l")

postpi.mean<-apply(post.pi,c(1,2),mean)


####################################################
#####MODEL 2: intercept+latent space ###############
####################################################
setwd("G:/My Drive/Collaborations/ErnstWit/CultureRgm/country")
load("data_marg_inig_wevs7.RData")
library(rgm)
source("rgm.culture.R")
source("Gmcmc.culture.R")

iter<-100000 #outer iterations for LSM
bd.iter<-20 #inner iterations for bdgraph
res<-rgm.culture(data, X=NULL,L=TRUE, D=2, bd.iter=bd.iter, iter=iter,initial.graphs=initial.graphs,method="gcgm",gcgm.marg="ordinal",gcgm.par =par.mar)
#save.image("country-rgm-wevs7-latent.RData") #intercept+latent space

setwd("G:/My Drive/Collaborations/ErnstWit/CultureRgm/country")
library(rgm)
load("country-rgm-wevs7-latent.RData") #100000x20 iterations
####plot
iter<-ncol(res$sample.alpha)
iter
burn<-floor(0.25*iter)
burn
sample.graphs<-res$sample.graphs[,,-(1:burn)]
sample.cloc<-res$sample.loc[,,-(1:burn)]
sample.alpha<-res$sample.alpha[,-(1:burn)]
post.pi<-res$sample.pi[,,-(1:burn)]
probit.pi<-res$pi.probit[,,-(1:burn)]
sample.K<-res$sample.K[,,-(1:burn)] 

#applying rotation to latent coordinates
hlp<-array(apply(res$sample.loc,3,rot),dim=dim(res$sample.loc))

sample.cloc<-hlp[,,-(1:burn)]

# some plots
cloc.est<-apply(sample.cloc,c(1,2),mean)
alpha.est<-apply(sample.alpha,1,mean)
plot(sample.alpha[1,],type="l")
plot(sample.alpha[5,],type="l")
plot(sample.cloc[1,2,],type="l")

#sparsity
apply(sample.alpha,1,mean)


#latent space
library(ggplot2)
names(data)
cx.est<-cloc.est[,1]
cy.est<-cloc.est[,2]
dat.p<-data.frame(cx.est,cy.est)
pdf("country-latentspace.pdf")
ggplot(dat.p,aes(y=cy.est, x=cx.est,label=countryname))+geom_label(fontface = "bold", size=2)+geom_hline(yintercept=0)+geom_vline(xintercept=0)+theme_classic()+xlim(-0.05,0.3)+xlab("latent coordinate (dim 1)")+ ylab("latent coordinate (dim 2)") +theme(axis.title = element_text(size = 20),plot.title = element_text(size = 20))+ggtitle("Latent Space of Countries")
dev.off()

postpi.mean<-apply(post.pi,c(1,2),mean)
colnames(postpi.mean)<-countryname
dat.sm<-as.matrix(postpi.mean)

p<-ncol(data[[1]])
ct<-colnames(data[[1]])
ct<-c("H","T","R","V","G","O","A","P","M","B")
edge.label<-NULL
for(i in 1:(length(ct)-1))
  for(j in (i+1):length(ct))
    edge.label<-c(edge.label,paste(ct[i],ct[j],sep="-"))

rownames(dat.sm)<-edge.label
dat.sm<-t(dat.sm)
library("gplots")
a<-heatmap.2(dat.sm, scale = "none", col = colorpanel(100,"#0571b0", "#f7f7f7","#ca0020"),
             trace = "none", density.info = "none",dendrogram = "none",cexRow = 0.5,cexCol = 0.3)
pdf("country-probs.pdf")
a<-heatmap.2(dat.sm, scale = "none", col = colorpanel(100,"#0571b0", "#f7f7f7","#ca0020"), key=F,
             trace = "none", density.info = "none",dendrogram = "none",cexRow = 0.45,cexCol = 1, lwid=c(0.1,10), lhei=c(0.1,10))
dev.off()

#compare with Jeffrey distance (taking partial correlations into account)
post.k<-apply(sample.K,c(1,2),mean)
p<-ncol(data[[1]])
khat_corr <- list() #standardized precision matrices
for (k in 1:length(data)){
  kh<-matrix(0,p,p)
  kh[lower.tri(kh,diag=TRUE)]<-post.k[,k]
  kh<-(t(kh)+kh)-diag(diag(kh))
  a <- solve(kh)
  b <- diag(1/sqrt(diag(a))) %*% a %*% diag(1/sqrt(diag(a)))
  khat_corr[[k]] <- solve(b)
}
dist.net<-matrix(0,length(data),length(data))
for(i in 1: (length(data) -1))
  for(j in (i+1):length(data))
  {
    dist.net[i,j]<-dist.net[j,i]<-0.5*(sum(khat_corr[[j]]*solve(khat_corr[[i]])) + sum(khat_corr[[i]]*solve(khat_corr[[j]])))-p
  }


dist<-crossprod(t(cloc.est),t(cloc.est))
lt.dist<-dist[lower.tri(dist)]
jf.dist<-dist.net[lower.tri(dist.net)]
plot(lt.dist,jf.dist,xlab="Latent Space Vicinity",ylab="Jeffrey (Network) Distance")
abline(lm(jf.dist~lt.dist))


##World map with the 84 countries
library(sf)
library(rnaturalearth)
world <- ne_countries(scale = 50,returnclass = "sf")%>%filter(admin!="Antarctica")

iso23.map<-iso23.sel
iso23.map$iso3[iso23.map$iso3=="ROM"]<-"ROU"
sum(world$adm0_a3 %in% iso23.map$iso3)

world$wvs7<-ifelse(world$adm0_a3 %in% iso23.map$iso3,"Included","Excluded")
world$wvs7[world$wvs7=="Excluded"]<-"NA"
a<-world$wvs7[world$wvs7=="Included"]
pm.av1<-pm.av[match(world$adm0_a3[world$wvs7=="Included"],iso23.map$iso3)]
a[pm.av1*100==0]<-"0%"
a[pm.av1*100 > 0 & pm.av1*100 <2]<-"0%-2%"
a[pm.av1*100>2 & pm.av1*100 <4]<-"2%-4%"
a[pm.av1*100 >4]<-"4%-10%"
world$wvs7[world$wvs7=="Included"]<-a
world$wvs7<-factor(world$wvs7)

world_label <- world  %>% filter(adm0_a3 %in% iso23.map$iso3)
sf_use_s2(FALSE)
a<-cbind(st_coordinates(st_centroid(world_label$geometry)))
world_label$longitude<-a[,1]
world_label$latitude<-a[,2]


pdf("ewvs7map.pdf",width=16,height=9)
world %>% 
  ggplot() + 
  geom_sf(aes(fill = wvs7)) +
  scale_fill_manual(values=c("#08519c", "#3182bd", "#6baed6", "#bdd7e7","white"))+
  theme_void() +
  #theme(legend.position = "top") +
  theme(legend.position = "top",aspect.ratio=9/16) +
  labs(fill = "% Missing") 
 # + ggrepel::geom_label_repel(data = world_label,
 #                            aes(x=longitude,y = latitude, label = name),
 #                           size = 3, force = 3 , force_pull = 5,max.overlaps=40)
#  + guides(fill=guide_legend(nrow=2, byrow=TRUE))
dev.off()



##create distance matrices
dist_cepii <- read_excel("dist_cepii.xls")
dist_cepii2 <- dist_cepii[dist_cepii$iso_o %in% iso23.sel$iso3,]
dist_cepii3 <- dist_cepii2[dist_cepii2$iso_d %in% iso23.sel$iso3,]

iso3.sel <- iso23.sel %>% dplyr::select(countryname, iso3)
dim(iso3.sel)

dist_cepii4 <- inner_join(iso3.sel, dist_cepii3,by = c("iso3" = "iso_o"),multiple="all")
dim(dist_cepii4)
head(dist_cepii4[,1:7])
dist_cepii5 <- inner_join(iso3.sel, dist_cepii4, by = c("iso3" = "iso_d"),multiple="all")
head(dist_cepii5[,1:7])


##Superimposing World Bank Groups on latent space
library(countrycode)
iso23.map<-iso23.sel
iso23.map$iso3[iso23.map$iso3=="ROM"]<-"ROU"
worldbank<-countrycode(iso23.map$iso3, origin = "iso3c", destination="region")
worldbank<-countrycode(iso23.map$iso3, origin = "iso3c", destination="region23")
table(worldbank)
worldbank
cultural_background <- factor(worldbank, levels=c("Northern America","Caribbean","Central America","South America"  ,"Australia and New Zealand","Northern Africa","Eastern Africa","Western Africa","Northern Europe","Eastern Europe","Southern Europe","Western Europe","Western Asia","Central Asia" , "South-Eastern Asia", "Southern Asia","Eastern Asia")
)
library(ggplot2)
cx.est<-cloc.est[,1]
cy.est<-cloc.est[,2]
dat.p<-data.frame(cx.est,cy.est,cultural_background)
library(RColorBrewer)
getPalette<-colorRampPalette(brewer.pal(6,"Set1"))
pdf("country-latentspace-WBgroups.pdf")
ggplot(dat.p,aes(y=cy.est, x=cx.est,label=countryname))+ geom_label(aes(fill = factor(cultural_background)), colour = "white", fontface = "bold", size=2) + 
   guides(fill = guide_legend(title = "World Bank\n Regions", override.aes = aes(label = "")))+theme_classic()+xlim(-0.05,0.3)+xlab("latent coordinate (dim 1)")+ ylab("latent coordinate (dim 2)") +theme(axis.title = element_text(size = 20),plot.title = element_text(size = 20),legend.text=element_text(size=7),legend.title =element_text(size=9))+ggtitle("Latent Space of Countries")+
  theme(legend.position="top",legend.justification="right")+geom_hline(yintercept=0)+geom_vline(xintercept=0)
dev.off()

##################################
#Proximity distance ##############
#actual distance
geodist <- matrix(dist_cepii5$dist, length(unique(dist_cepii5$iso3)), length(unique(dist_cepii5$iso3)), byrow = TRUE)
diag(geodist) <- NA
rownames(geodist) <- colnames(geodist) <- iso3.sel$countryname
dim(geodist)
geodist<-1/log(geodist) #similarity
diag(geodist)<-0
head(geodist)
geodist<-geodist[order(colnames(geodist)),order(colnames(geodist))]
#border sharing
geocontig <- matrix(dist_cepii5$contig, length(unique(dist_cepii5$iso3)), length(unique(dist_cepii5$iso3)), byrow = TRUE)
diag(geocontig) <- 0
rownames(geocontig) <- colnames(geocontig) <- iso3.sel$countryname
geocontig<-geocontig[order(colnames(geocontig)),order(colnames(geocontig))]

# Common Official Language
lang <- matrix(dist_cepii5$comlang_off, length(unique(dist_cepii5$iso3)), length(unique(dist_cepii5$iso3)), byrow = TRUE)
diag(lang) <- 0
rownames(lang) <- colnames(lang) <- iso3.sel$countryname
lang<-lang[order(colnames(lang)),order(colnames(lang))]

# Common Major Language (more than 9% of population)
langethno <- matrix(dist_cepii5$comlang_ethno, length(unique(dist_cepii5$iso3)), length(unique(dist_cepii5$iso3)), byrow = TRUE)
diag(langethno) <- 0
rownames(langethno) <- colnames(langethno) <- iso3.sel$countryname
langethno<-langethno[order(colnames(langethno)),order(colnames(langethno))]

# Common Colonizer
colony <- matrix(dist_cepii5$colony, length(unique(dist_cepii5$iso3)), length(unique(dist_cepii5$iso3)), byrow = TRUE)
diag(colony) <- 0
rownames(colony) <- colnames(colony) <- iso3.sel$countryname
colony<-colony[order(colnames(colony)),order(colnames(colony))]

# Same country
smctry <- matrix(dist_cepii5$smctry, length(unique(dist_cepii5$iso3)), length(unique(dist_cepii5$iso3)), byrow = TRUE)
diag(smctry) <- 0
rownames(smctry) <- colnames(smctry) <- iso3.sel$countryname
smctry<-smctry[order(colnames(smctry)),order(colnames(smctry))]

# Same continent
a<-read.delim("countryInfo_wevs.txt",header=TRUE)
a[66,1]<-"ROM"
a<-a[match(iso23.sel$iso3,a[,1]),]
table(a[,4])
contdist<-matrix(0,nrow(a),nrow(a))
for(i in 1: (ncol(contdist)-1))
  for(j in (i+1): ncol(contdist))
    contdist[i,j]<-contdist[j,i]<-ifelse(a[i,4]==a[j,4],1,0)
rownames(contdist) <- colnames(contdist) <- iso3.sel$countryname

##Superimposing Language Groups on latent space
a<-read.delim("countryInfo_wevs.txt",header=TRUE)
apply(lang,2,sum)
table(a[,3])
a[66,1]<-"ROM"
a<-a[match(iso23.sel$iso3,a[,1]),]
table(a[,3])
b<-a[a[,3]!="Others",]
cultural_background <- factor(b[,3],levels=c("Arabic","Chinese","English","Spanish", "Portuguese"))
table(cultural_background)

library(ggplot2)
cx.est<-cloc.est[a[,3]!="Others",1]
cy.est<-cloc.est[a[,3]!="Others",2]
dat.p<-data.frame(cx.est,cy.est,cultural_background)
pdf("country-latentspace-Lgroups.pdf")
ggplot(dat.p,aes(y=cy.est, x=cx.est,label=countryname[a[,3]!="Others"]))+ geom_label(aes(fill = factor(cultural_background)), colour = "white", fontface = "bold", size=2) + 
  scale_fill_manual(values=c("#2ca25f","#c994c7","#ca0020","#fec44f","#3182bd"))+
  guides(fill = guide_legend(title = "Spoken\n Language", override.aes = aes(label = "")))+theme_classic()+xlim(-0.05,0.3)+xlab("latent coordinate (dim 1)")+ ylab("latent coordinate (dim 2)") +theme(axis.title = element_text(size = 20),plot.title = element_text(size = 20),legend.text=element_text(size=9),legend.title =element_text(size=9))+ggtitle("Latent Space of Countries")+
  theme(legend.position="top",legend.justification="right")+geom_hline(yintercept=0)+geom_vline(xintercept=0)
dev.off()


######################################################
####MODEL 3: intercept + distance measures ###########
######################################################
##some checks on which distances to use
###
dist<-crossprod(t(cloc.est),t(cloc.est)) # from just the latent space model
dist.latent<-dist[lower.tri(dist)]
dat<-data.frame(geo=geodist[upper.tri(geodist)],langoff=lang[upper.tri(lang)],cont=contdist[upper.tri(contdist)],col=colony[upper.tri(colony)],smc=smctry[upper.tri(smctry)],langpri=langethno[upper.tri(langethno)],y=dist.latent[upper.tri(dist.latent)])
summary(lm(y~geo+langoff+cont+col+smc+langpri+I(geo^2),data=dat))
summary(lm(y~geo+langoff+cont+col+smc+I(geo^2),data=dat))

W<-array(dim = c(length(data),length(data),4))
W[,,1]<-geodist
W[,,2]<-contdist
W[,,3]<- lang
W[,,4]<- langethno

W[,,5]<-geodist^2

library(rgm)
source("rgm.culture.R")
source("Gmcmc.culture.R")
iter<-100000 #outer iterations for LSM
bd.iter<-20 #inner iterations for bdgraph
res<-rgm.culture(data, W=W, L=FALSE, D=2, bd.iter=bd.iter, iter=iter,initial.graphs=initial.graphs,method="gcgm",gcgm.marg="ordinal",gcgm.par =par.mar)
#save.image("country-rgm-wevs7-distance.RData") #intercept + distance matrices


setwd("G:/My Drive/Collaborations/ErnstWit/CultureRgm/country")
library(rgm)
load("country-rgm-wevs7-distance.RData")
iter<-ncol(res$sample.alpha)
iter
burn<-floor(0.25*iter)
burn
sample.graphs<-res$sample.graphs[,,-(1:burn)]
sample.alpha<-res$sample.alpha[,-(1:burn)]
sample.beta<-res$sample.theta[,-(1:burn)]
post.pi<-res$sample.pi[,,-(1:burn)]
probit.pi<-res$pi.probit[,,-(1:burn)]
sample.K<-res$sample.K[,,-(1:burn)] 

# some plots
alpha.est<-apply(sample.alpha,1,mean)
plot(sample.alpha[1,],type="l")
plot(sample.alpha[5,],type="l")
plot(sample.beta[1,],type="l")
plot(sample.beta[2,],type="l")
plot(sample.beta[3,],type="l")
plot(sample.beta[4,],type="l")
plot(sample.beta[5,],type="l")

require(tidyverse)
require(reshape2)
require(ggridges)


full.coeffs <- t(sample.beta)
temp.LQ <- dim(full.coeffs)[1]

cof <- apply(sample.beta,1,mean)
cof<-matrix(cof,nrow=1)
colnames(cof) <- c("Spatial Proximity","Common Continent", "Common Language (Off)", "Common Language (Prim)")
cof


buff <- melt(full.coeffs)
buff$cov <- buff$Var2
buff$cov <- factor(buff$cov, labels=colnames(cof))
buff$it <- buff$Var1
buff$Var1 <- buff$Var2 <- NULL

pdf("distances.pdf")
buff %>% 
  ggplot(aes(y=cov, x=value, height = after_stat(density)))+
  geom_density_ridges(scale=0.99,alpha=0.8,stat = "density", trim = TRUE)+
  geom_vline(xintercept = 0, col="black", lwd=0.8, lty=2)+
  scale_fill_grey()+xlab("")+ylab("") + xlim(-0.3,0.3)+theme_classic()+theme(axis.text.y = element_text(size = 20,vjust=0.01),plot.title = element_text(size = 20))+ggtitle(expression(paste("Posterior Distributions of ",beta)))
dev.off()



###################################################################
#MODEL 4: intercept + distance measures + latent space ############
###################################################################
W<-array(dim = c(length(data),length(data),4)) #choose same from model 3
W[,,1]<-geodist
W[,,2]<-contdist
W[,,3]<- lang
W[,,4]<- langethno

library(rgm)
source("rgm.culture.R")
source("Gmcmc.culture.R")

iter<-100000 #outer iterations for LSM
bd.iter<-20 #inner iterations for bdgraph
res<-rgm.culture(data, W=W, L=TRUE, D=2, bd.iter=bd.iter, iter=iter,initial.graphs=initial.graphs,method="gcgm",gcgm.marg="ordinal",gcgm.par=par.mar)
#save.image("country-rgm-wevs7-distance+latent.RData") #intercept+ distance matrices+latent space

setwd("G:/My Drive/Collaborations/ErnstWit/CultureRgm/country")
library(rgm)
load("country-rgm-wevs7-distance+latent.RData")
iter<-ncol(res$sample.alpha)
iter
burn<-floor(0.25*iter)
burn


sample.graphs<-res$sample.graphs[,,-(1:burn)]
sample.cloc<-res$sample.loc[,,-(1:burn)]
sample.alpha<-res$sample.alpha[,-(1:burn)]
sample.beta<-res$sample.theta[,-(1:burn)]
post.pi<-res$sample.pi[,,-(1:burn)]
probit.pi<-res$pi.probit[,,-(1:burn)]
sample.K<-res$sample.K[,,-(1:burn)] 


#applying rotation to latent coordinates
hlp<-array(apply(res$sample.loc,3,rot),dim=dim(res$sample.loc))

sample.cloc<-hlp[,,-(1:burn)]


# some plots
cloc.est<-apply(sample.cloc,c(1,2),mean)
alpha.est<-apply(sample.alpha,1,mean)
plot(sample.cloc[1,1,],type="l")
plot(sample.alpha[1,],type="l")
plot(sample.alpha[5,],type="l")
plot(sample.beta[1,],type="l")
plot(sample.beta[2,],type="l")
plot(sample.beta[3,],type="l")
plot(sample.beta[4,],type="l")

require(tidyverse)
require(reshape2)
require(ggridges)


full.coeffs <- t(sample.beta)
temp.LQ <- dim(full.coeffs)[1]

cof <- apply(sample.beta,1,mean)
cof<-matrix(cof,nrow=1)
colnames(cof) <- c("Spatial Proximity","Common Continent", "Common Language (Off)", "Common Language (Prim)")
cof


buff <- melt(full.coeffs)
buff$cov <- buff$Var2
buff$cov <- factor(buff$cov, labels=colnames(cof))
buff$it <- buff$Var1
buff$Var1 <- buff$Var2 <- NULL

pdf("distances.pdf",width=16,height=9)
buff %>% 
  ggplot(aes(y=cov, x=value, height = after_stat(density)))+
  geom_density_ridges(scale=0.99,alpha=0.8,stat = "density", trim = TRUE)+
  geom_vline(xintercept = 0, col="black", lwd=0.8, lty=2)+
  scale_fill_grey()+xlab("")+ylab("") + xlim(-0.15,0.25)+theme_classic()+theme(aspect.ratio=9/16,axis.text.y = element_text(size = 20,vjust=0.01),plot.title = element_text(size = 20))+ggtitle(expression(paste("Posterior Distributions of ",beta)))
dev.off()


#anything left on the latent space?

##Superimposing World Bank Groups on latent space
library(countrycode)
iso23.map<-iso23.sel
iso23.map$iso3[iso23.map$iso3=="ROM"]<-"ROU"
worldbank<-countrycode(iso23.map$iso3, origin = "iso3c", destination="region")
worldbank<-countrycode(iso23.map$iso3, origin = "iso3c", destination="region23")
table(worldbank)
worldbank
cultural_background <- factor(worldbank, levels=c("Northern America","Caribbean","Central America","South America"  ,"Australia and New Zealand","Northern Africa","Eastern Africa","Western Africa","Northern Europe","Eastern Europe","Southern Europe","Western Europe","Western Asia","Central Asia" , "South-Eastern Asia", "Southern Asia","Eastern Asia")
)
library(ggplot2)
cx.est<-cloc.est[,1]
cy.est<-cloc.est[,2]
dat.p<-data.frame(cx.est,cy.est,cultural_background)
library(RColorBrewer)
getPalette<-colorRampPalette(brewer.pal(6,"Set1"))
ggplot(dat.p,aes(y=cy.est, x=cx.est,label=countryname))+ geom_label(aes(fill = factor(cultural_background)), colour = "white", fontface = "bold", size=2) + 
  guides(fill = guide_legend(title = "World Bank\n Regions", override.aes = aes(label = "")))+theme_classic()+xlim(-0.2,0.2)+xlab("latent coordinate (dim 1)")+ ylab("latent coordinate (dim 2)") +theme(axis.title = element_text(size = 20),plot.title = element_text(size = 20),legend.text=element_text(size=7),legend.title =element_text(size=9))+ggtitle("Latent Space of Countries")+
  theme(legend.position="top",legend.justification="right")+geom_hline(yintercept=0)+geom_vline(xintercept=0)

a<-read.delim("countryInfo_wevs.txt",header=TRUE)
apply(lang,2,sum)
table(a[,3])
a[66,1]<-"ROM"
a<-a[match(iso23.sel$iso3,a[,1]),]
table(a[,3])
b<-a[a[,3]!="Others",]
cultural_background <- factor(b[,3],levels=c("Arabic","Chinese","English","Spanish", "Portuguese"))
table(cultural_background)

library(ggplot2)
cx.est<-cloc.est[a[,3]!="Others",1]
cy.est<-cloc.est[a[,3]!="Others",2]
dat.p<-data.frame(cx.est,cy.est,cultural_background)
ggplot(dat.p,aes(y=cy.est, x=cx.est,label=countryname[a[,3]!="Others"]))+ geom_label(aes(fill = factor(cultural_background)), colour = "white", fontface = "bold", size=2) + 
  scale_fill_manual(values=c("#2ca25f","#c994c7","#ca0020","#fec44f","#3182bd"))+
  guides(fill = guide_legend(title = "Spoken\n Language", override.aes = aes(label = "")))+theme_classic()+xlim(-0.2,0.2)+xlab("latent coordinate (dim 1)")+ ylab("latent coordinate (dim 2)") +theme(axis.title = element_text(size = 20),plot.title = element_text(size = 20),legend.text=element_text(size=9),legend.title =element_text(size=9))+ggtitle("Latent Space of Countries")+
  theme(legend.position="top",legend.justification="right")+geom_hline(yintercept=0)+geom_vline(xintercept=0)

#########################################
###DIC CALCULATION ############
########################################

library(mvtnorm)
library(tlrmvnmvt)
sample.K<-res$sample.K[,,-(1:burn)]
post.k<-apply(sample.K,c(1,2),mean)
niter<-dim(res$sample.K)[3]
sample.K<-res$sample.K[,,sample(floor(0.75*niter):niter,50,replace=F)] 

funapply<-function(i, lw, up,sig)
{
  as.numeric(log(pmvn(lower=lw[i,], upper=up[i,], sigma=sig)))
}
B<-length(data)

#average deviance
dev<-NULL
for(j in 1: dim(sample.K)[3])
  {
  print(j)
  devj<-NULL
  for(B in 1:length(data))
  {
    print(B)
    K<-matrix(0,ncol(data[[B]]),ncol(data[[B]]))
    K[lower.tri(K,diag=TRUE)]<-sample.K[,B,j] 
    K<-K+t(K)-diag(diag(K))
    sig<-solve(K)
    devc<-sapply(1:nrow(data[[B]]),funapply, lw=par.mar[[B]]$tdown,up=par.mar[[B]]$tup,sig=sig)
    devj<-c(devj,devc)
  }
  dev[j]<--2*sum(devj)
}
mean.deviance<- sum(dev)/dim(sample.K)[3]
var.deviance<-var(dev)


#deviance of posterior mean
dev.mean<-NULL
for(B in 1:length(data))
{
  print(B)
  K<-matrix(0,ncol(data[[B]]),ncol(data[[B]]))
  K[lower.tri(K,diag=TRUE)]<-post.k[,B] 
  K<-K+t(K)-diag(diag(K))
  sig<-solve(K)
  devc<-sapply(1:nrow(data[[B]]),funapply, lw=par.mar[[B]]$tdown,up=par.mar[[B]]$tup,sig=sig)
  dev.mean<-c(dev.mean,devc)
}

deviance.mean<- -2*sum(dev.mean)


#DIC
#version 1
deviance.mean+2*(mean.deviance-deviance.mean)
#version 2
deviance.mean + 2*var.deviance

#MODEL COMPARISON (DIC values)
#after 10 iterations (version 1)
#model 1 (only intercept)
#3072308
#model 2 (only latent space)
#3072462
#model 3 (distance matrices)
#3072596
#model 4 (distance matrices+latent space)
#3072562
#after 30 iterations (version 1)
#model1
#3072259
#model 2
#3072435
#model 3
#3072372
#model 4
#3072507

#after 50 iterations (version 1&2)
#model1 (intercept only)
# V1: 3072273 V2: 3116506
#model 2 (latent space)
# V1: 3072387 V2: 3115201
#model 3 (language prim, language off, common continent, spatial)
# V1: 3072430 V2: 3095245
#model 4 (language prim, language off, common continent, spatial, latent space)
# V1: 3072537  V2: 3100461

###other models
#model 3 (language prim, sharing border, spatial)
# V1: 3072385 V2: 3116454
#model 4 (language prim, sharing border, spatial)
# V1: 3072536 V2: 3104938
#model 3 (language prim, language off, common continent, spatial, spatial^2)
# V1: 3072418 V2: 3100265

#########################################################
## model 2 vs model 4 ###################################
#########################################################

setwd("G:/My Drive/Collaborations/ErnstWit/CultureRgm/country")

load("country-rgm-wevs7-distance+latent.RData")
iter<-ncol(res$sample.alpha)
burn<-floor(0.25*iter)
sample.cloc<-res$sample.loc[,,-(1:burn)]
hlp<-array(apply(res$sample.loc,3,rot),dim=dim(res$sample.loc))
sample.cloc<-hlp[,,-(1:burn)]
cloc.est<-apply(sample.cloc,c(1,2),mean)
cx.est.mod4<-cloc.est[,1]
cy.est.mod4<-cloc.est[,2]

#latent space
pdf("latentspace-mod4.pdf")
library(ggplot2)
dat.p<-data.frame(cx.est.mod4,cy.est.mod4)
ggplot(dat.p,aes(y=cy.est.mod4, x=cx.est.mod4,label=countryname))+geom_label(fontface = "bold", size=2)+geom_hline(yintercept=0)+geom_vline(xintercept=0)+theme_classic()+xlim(-0.3,0.3)+ylim(-0.065,0.06)+xlab("latent coordinate (dim 1)")+ ylab("latent coordinate (dim 2)") +theme(axis.title = element_text(size = 20),plot.title = element_text(size = 20))+ggtitle("Latent Space of Countries (Model (2))")
dev.off()

load("country-rgm-wevs7-latent.RData")
iter<-ncol(res$sample.alpha)
burn<-floor(0.25*iter)
sample.cloc<-res$sample.loc[,,-(1:burn)]
hlp<-array(apply(res$sample.loc,3,rot),dim=dim(res$sample.loc))
sample.cloc<-hlp[,,-(1:burn)]
cloc.est<-apply(sample.cloc,c(1,2),mean)
cx.est.mod2<-cloc.est[,1]
cy.est.mod2<-cloc.est[,2]
#latent space
pdf("latentspace-mod2.pdf")
library(ggplot2)
dat.p<-data.frame(cx.est.mod2,cy.est.mod2)
ggplot(dat.p,aes(y=cy.est.mod2, x=cx.est.mod2,label=countryname))+geom_label(fontface = "bold", size=2)+geom_hline(yintercept=0)+geom_vline(xintercept=0)+theme_classic()+xlim(-0.3,0.3)+ylim(-0.065,0.06)+xlab("latent coordinate (dim 1)")+ ylab("latent coordinate (dim 2)") +theme(axis.title = element_text(size = 20),plot.title = element_text(size = 20))+ggtitle("Latent Space of Countries (Model (3))")
dev.off()
