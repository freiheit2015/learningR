if(!file.exists("data")){
  dir.create("data")
}

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.csv")
list.files("./data")

dateDownloaded <- date()
dateDownloaded

cameraData <- read.table("./data/cameras.csv")
head(cameraData)

cameraData<- read.table("./data/cameras.csv", sep=",", header =TRUE)
head(cameraData)

cameraData<- read.csv("./data/cameras.csv")
head(cameraData)

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.xlsx")
list.files("./data")

library(xlsx)
cameraData <- read.xlsx("./data/cameras.xlsx",sheetIndex=1,header=TRUE)
head(cameraData)

fileUrl <- "https://data.baltimorecity.gov/api/views/dz54-2aru/rows.xlsx?accessType=DOWNLOAD"
download.file(fileUrl, destfile = "./data/cameras.xlsx")
dateDownloaded<- date()


library(XML)
fileUrl<- "http://www.w3schools.com/xml/simple.xml"
doc <-xmlTreeParse(fileUrl,useInternal= TRUE)
rootNode<- xmlRoot(doc)
xmlName(rootNode)

names(rootNode)

rootNode[[1]]



fileUrl<-"http://espn.go.com/nfl/team/_/name/bal/baltimore-ravens"
doc<-htmlTreeParse(fileUrl,useInternal=TRUE)
scores<- xpathSApply(doc,"//li[@class='score']",xmlValue)
teams<-xpathSApply(doc,"//li[@class='team-name']",xmlValue)
scores


library(jsonlite)
jsonData<- fromJSON("https://api.github.com/users/jtleek/repos")
names(jsonData)

names(jsonData$owner)

jsonData$owner$login

myjson<- toJSON(iris, pretty=TRUE)
cat(myjson)

iris2<- fromJSON(myjson)
head(iris2)

library(data.table)
DF= data.frame(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DF,3)
DT= data.table(x=rnorm(9),y=rep(c("a","b","c"),each=3),z=rnorm(9))
head(DT,3)
tables()
DT[2,]
DT[DT$y=="a",]
DT[c(2,3)]

DT[,list(mean(x),sum(z))]
DT[,table(y)]

DT[,w:=z^2]#add a new column


DT2<-DT#copy a datatable

DT[,m:={tmp<-(x+z);log2(tmp+5)}]

DT[,a:=x>0]

DT[,b:=mean(x+w),by=a]#grouped by a

set.seed(123)
DT<- data.table(x=sample(letters[1:3],1E5,TRUE))
DT[,.N,by=x]

DT<-data.table(x=rep(c("a","b","c"),each=100),y=rnorm(300))
setkey(DT,x)
DT['a']

DT1<-data.table(x=c('a','a','b','dt1'),y=1:4)
DT2<-data.table(x=c('a','b','dt2'),z=5:7)
setkey(DT1,x);setkey(DT2,x)
merge(DT1,DT2)

big_df<- data.frame(x=rnorm(1E6),y=rnorm(1E6))
file<- tempfile()
write.table(big_df,file=file, row.names=FALSE,col.names=TRUE,sep="\t",quote=FALSE)
system.time(fread(file))

ucscDb<-dbConnect(MySQL(),user="genome",host="genome-mysql.cse.ucsc.edu")
result<-dbGetQuery(ucscDb,"show databases;");dbDisconnect(ucscDb);

source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library(rhdf5)
created = h5createFile("example.h5")
created

created=h5createGroup("example.h5","foo")
created=h5createGroup("example.h5","baa")
created=h5createGroup("example.h5","foo/foobaa")
h5ls("example.h5")
#Write to groups
A= matrix(1:10,nr=5,nc=2)
h5write(A,"example.h5","foo/A")
B= array(seq(0.1,2.0,by=0.1),dim=c(5,2,2))
attr(B,"scale")<-"liter"
h5write(B, "example.h5","foo/foobaa/B")
h5ls("example.h5")

#write a data set
df=data.frame(1L:5L, seq(0,1,length.out=5),c("ab","cde","fghi","a","s"),stringsAsFactors=FALSE)
h5write(df,"example.h5","df")
h5ls("example.h5")

#Reading data
readA= h5read("example.h5","foo/A")
readB= h5read("example.h5","foo/foobaa/B")
readdf= h5read("example.h5","df")
readA

#writing and reading chunks
h5write(c(12,13,14),"example.h5","foo/A",index=list(1:3,1))
h5read("example.h5","foo/A")

#Reading from the Web
con= url("http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en")
htmlCode=readLines(con)
close(con)
htmlCode

library(XML)
url<- "http://scholar.google.com/citations?user=HI-I6C0AAAAJ&hl=en"
html<-htmlTreeParse(url,useInternalNodes=T)
xpathSApply(html,"//title",xmlValue)

#get from the httr package
library(httr);html2= GET(url)
content2=content(html2,as="text")
parsedHtml= htmlParse(content2,asText=TRUE)
xpathSApply(parsedHtml,"//title",xmlValue)

#accessing websites with passwords
pg1=GET("http://httpbin.org/basic-auth/user/passwd")
pg1

pg2=GET("http://httpbin.org/basic-auth/user/passwd",authenticate("user","passwd"))
pg2
names(pg2)


#subsetting quick review
set.seed(13435)
X<-data.frame("var1"=sample(1:5),"var2"=sample(6:10),"var3"=sample(11:15))
X<-X[sample(1:5),];X$var2[c(1,3)]=NA
X

X[(X$var1<=3& X$var3>11),]#and
X[(X$var1<=3|X$var3>15),] #or

X[which(X$var2>8),]

sort(X$var1)
sort(X$var1,decreasing=TRUE)
sort(X$var2,na.last=TRUE)

X[order(X$var1),]

library(plyr)
arrange(X,var1)
arrange(X,desc(var1))
#adding rows and columns
X$var4<-rnorm(5)
X

Y<-cbind(X,rnorm(5))
Y

fileUrl<-"https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
download.file(fileUrl,destfile="./data/restaurant.csv")
restData<-read.csv("./data/restaurant.csv")

head(restData,n=3)
tail(restData,n=3)
summary(restData)

quantile(restData$councilDistrict,na.rm=TRUE)

table(restData$zipCode,useNA="ifany")
table(restData$councilDistrict,restData$zipCode)

sum(is.na(restData$councilDistrict))
any(is.na(restData$councilDistrict))
all(restData$zipCode>0)

colSums(is.na(restData))

all(colSums(is.na(restData))==0)

table(restData$zipCode %in% c("21212"))
table(restData$zipCode %in% c("21212","21213"))

restData[restData$zipCode %in% c("21212","21213"),]

data(UCBAdmissions)
DF=as.data.frame(UCBAdmissions)
summary(DF)

xt<- xtabs(Freq ~ Gender+Admit,data=DF)
xt

warpbreaks$replicate <- rep(1:9, len=54)
xt= xtabs(breaks~.,data= warpbreaks)
xt

ftable(xt)

object.size()

#create new variables
restData<-read.csv("./data/restaurant.csv")
##creating sequences
s1<- seq(1,10,by=2)
s2<-seq(1,10,length=3)
x<-c(1,3,5,6,8);seq(along=x)

restData$nearMe=restData$neighborhood %in% c("Roland Park","Homeland")
table(restData$nearMe)

restData$zipWrong=ifelse(restData$zipCode<0, TRUE, FALSE)
table(restData$zipWrong,restData$zipCode<0)

restData$zipGroups=cut(restData$zipCode,breaks=quantile(restData$zipCode))
table(restData$zipGroups)
table(restData$zipGroups,restData$zipCode)

##easier cutting
library(Hmisc)
restData$zipGroups=cut2(restData$zipCode,g=4)
table(restData$zipGroups)

restData$zcf<-factor(restData$zipCode)
restData$zcf[1:10]
class(restData$zcf)
##levels of factor variables
yesno<- sample(c("yes","no"),size=10,replace=TRUE)
yesnofac= factor(yesno,levels=c("yes","no"))
relevel(yesnofac,ref="yes")
as.numeric(yesnofac)

##using the mutate function
library(Hmisc);library(plyr)
restData2=mutate(restData,zipGroups=cut2(zipCode,g=4))
table(restData2$zipGroups)

#reshaping data
library(reshape2)
head(mtcars)
##melting data frames
mtcars$carname<-rownames(mtcars)
carMelt<- melt(mtcars,id=c("carname","gear","cyl"),measure.vars=c("mpg","hp"))
head(carMelt,n=3)
tail(carMelt,n=3)
##casting data frames
cylData<-dcast(carMelt,cyl~variable)
cylData

cylData<-dcast(carMelt,cyl~variable,mean)
cylData
##averaging values
head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray,sum)
spIns=split(InsectSprays$count,InsectSprays$spray)
spIns
sprCount=lapply(spIns,sum)
sprCount

unlist(sprCount)
sapply(spIns,sum)

library(plyr)
ddply(InsectSprays,.(spray),summarize,sum=sum(count))

spraySums<-ddply(InsectSprays,.(spray),summarize,sum=ave(count,FUN=sum))
dim(spraySums)
head(spraySums)

#merging data
fileUrl1="http://dl.dropboxusercontent.com/u/7710864/data/reviews-apr29.csv"
fileUrl2="http://dl.dropboxusercontent.com/u/7710864/data/solutions-apr29.csv"
download.file(fileUrl1,destfile="./data/reviews.csv")
download.file(fileUrl2,destfile="./data/solutions.csv")
reviews=read.csv("./data/reviews.csv");solutions<-read.csv("./data/solutions.csv")
head(reviews,2)

names(reviews)
names(solutions)

mergedData=merge(reviews,solutions,by.x="solution_id",by.y="id",all=TRUE)
head(mergedData)

intersect(names(reviews),names(solutions))

mergedData2=merge(reviews,solutions,all=TRUE)

#using join in the plyr package
df1=data.frame(id=sample(1:10),x=rnorm(10))
df2=data.frame(id=sample(1:10),y=rnorm(10))
arrange(join(df1,df2),id)

df3=data.frame(id=sample(1:10),z=rnorm(10))
dfList=list(df1,df2,df3)
join_all(dfList)

#editing text variables
cameraData<-read.csv("./data/cameras.csv")
names(cameraData)

tolower(names(cameraData))#no capitla letter in the names

splitNames=strsplit(names(cameraData),"\\.")
splitNames

firstElement<-function(x){x[1]}
sapply(splitNames,firstElement)

sub("_","",names(reviews))

testName<-"this_is_a_test"
sub("_","",testName)
gsub("_","",testName)

#finding values
grep("Alameda",cameraData$intersection)
table(grepl("Alameda",cameraData$intersection))
cameraData2<- cameraData[!grepl("Alameda",cameraData$intersection),]

grep("Alameda",cameraData$intersection,value=TRUE)
grep("JeffStreet",cameraData$intersection)
length(grep("JeffStreet",cameraData$intersection))

#data
d1=date()
class(d1)

d2=Sys.Date()
d2
class(d2)
#Formatting dates
##%d=day as number(0-31);%a=abbreviated weekday;%A=unabbreviated weekday;%M=month(00-12);%b=abbreviated month,%B=unabbreviated month,%y= 2digit year,%Y=four digit year
format(d2,"%a %b %d")

x=c("1jan1960","2jan1960","31mar1960");z=as.Date(x,"%d%b%Y")
z

x=c("1一月1960","2一月1960","31三月1960");z=as.Date(x,"%d%b%Y")
z

weekdays(d2)
months(d2)
julian(d2)

library(lubridate);ymd("20140108")
mdy(08/04/2013)
dmy(03-04-2013)

ymd_hms("2011-08-03 10:15:03")
ymd_hms("2011-08-03 10:15:03",tz="Pacific/Auckland")

x=dmy(c("","",""))
wday(x[1])
wday(x[1],label=TRUE)






