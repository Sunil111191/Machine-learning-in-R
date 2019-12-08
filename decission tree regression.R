ld_train=read.csv("E:/mydata/loan_data_train.csv",stringsAsFactors = F)
ld_test= read.csv("E:/mydata/loan_data_test.csv",stringsAsFactors = F)
dim(ld_train)
View(ld_train)
dim(ld_test)
#create the interest rate variable in the test dataset and assign NA valuues to that.
ld_test$Interest.Rate = NA
View(ld_test)
#creating a new feauture which guides us from which dataset the row been choosen.
ld_train$data = "train"
ld_test$data = "test"
#combining the two datasets to create ld_all which comprises the rows
ld_all=rbind(ld_train,ld_test)
#dropping the variable 1st method
ld_test$Interest.Rate = NULL 
View(ld_train)
dim(ld_train)
dim(ld_test)
View(ld_test)
dim(ld_all)
View(ld_all)
#dropping the variable 2nd method
ld_all$Amount.Funded.By.Investors = NULL
View(ld_all)
dim(ld_all)
library(dplyr)
#check the attributes of the columns wheather they r useful in model building
glimpse(ld_all)
#here we found interest rate,debt to income ratio,open credit lines,amount requested
#revolving credit balance are in character data type.
ld_all=ld_all %>% mutate(Interest.Rate=as.numeric(gsub("%","",Interest.Rate)) , 
                         Debt.To.Income.Ratio=as.numeric(gsub("%","",Debt.To.Income.Ratio)) , 
                         Open.CREDIT.Lines=as.numeric(Open.CREDIT.Lines) , 
                         Amount.Requested=as.numeric(Amount.Requested) , 
                         Revolving.CREDIT.Balance=as.numeric(Revolving.CREDIT.Balance) )
#converting loan length as dummy variable and dropping the loan length variable
ld_all=ld_all %>%
  mutate(ll_36=as.numeric(Loan.Length=="36 months")) %>% 
  select(-Loan.Length)
table(ld_all$Loan.Purpose)
round(tapply(ld_all$Interest.Rate,ld_all$Loan.Purpose,mean,na.rm = T))
ld_all=ld_all %>%mutate(lp_10=as.numeric(Loan.Purpose=='educational'),
       lp_11=as.numeric(Loan.Purpose %in% c("major_purchase","medical","car")), 
       lp_12=as.numeric(Loan.Purpose %in% c("vacation","wedding","home_improvement")),        lp_13=as.numeric(Loan.Purpose %in% c("other","small_business","credit_card")), 
       lp_14=as.numeric(Loan.Purpose %in% c("debt_consolidation","house","moving"))) %>% 
  select(-Loan.Purpose)
dim(ld_all)
table(ld_all$State)
CreateDummies=function(data,var,freq_cutoff=0){ 
  t=table(data[,var])
  t=t[t>freq_cutoff] 
  t=sort(t) 
  categories=names(t)[-1]
for( cat in categories) {
  name=paste(var,cat,sep="_") 
  name=gsub(" ","",name)
  name=gsub("-","_",name) 
  name=gsub("\\?","Q",name) 
  name=gsub("<","LT_",name) 
  name=gsub("\\+","",name) 
  name=gsub("\\/","_",name) 
  name=gsub(">","GT_",name) 
  name=gsub("=","EQ_",name) 
  name=gsub(",","",name)
data[,name]=as.numeric(data[,var]==cat)
}
data[,var]=NULL 
return(data)
}
ld_all=CreateDummies(ld_all ,"State",100) 
ld_all=CreateDummies(ld_all,"Home.Ownership",100)
library(tidyr)
ld_all=ld_all %>%
  separate(FICO.Range,into=c("f1","f2"),sep="-") %>% 
  mutate(f1=as.numeric(f1), 
         f2=as.numeric(f2), 
         fico=0.5*(f1+f2)) %>% 
  select(-f1,-f2)
ld_all=CreateDummies(ld_all,"Employment.Length",100)

ld_all=ld_all[!(is.na(ld_all$ID)),]

for(col in names(ld_all)){
  if(sum(is.na(ld_all[,col]))>0 & !(col %in% c("data","Interest.Rate")))
    {
    ld_all[is.na(ld_all[,col]),col]=mean(ld_all[ld_all$data=='train',col],na.rm=T)
  } }

## separate train and test
ld_train=ld_all %>% filter(data=='train') %>% select(-data)
ld_test=ld_all %>% filter(data=='test') %>% select(-data,-Interest.Rate)

set.seed(2) 
s=sample(1:nrow(ld_train),0.7*nrow(ld_train)) 
ld_train1=ld_train[s,] 
ld_train2=ld_train[-s,]

library(tree)

ld.tree=tree(Interest.Rate~.-ID,data=ld_train1)  
plot(ld.tree)
text(ld.tree)

val.IR=predict(ld.tree,newdata = ld_train2)
rmse_val=((val.IR)-(ld_train2$Interest.Rate))^2 %>% mean() %>% sqrt()
rmse_val 
ld.tree.final=tree(Interest.Rate~.-ID,data=ld_train) 
test.pred=predict(ld.tree.final,newdata=ld_test) 
View(test.pred)
write.csv(test.pred,"E:/mysubmission.csv",row.names = F)
