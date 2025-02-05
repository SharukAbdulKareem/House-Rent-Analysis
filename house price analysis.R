#SHARUK ABDUL KAREEM
#TP061621


HouseData = read.csv("C:\\Users\\SHARUK\\Downloads\\House_Rent_Dataset.csv",header =TRUE)
HouseData


head(HouseData)
length (HouseData) #no:of column

ncol (HouseData) #no: of column
nrow (HouseData) #no: of rows

head(HouseData,10)
tail(HouseData,10)

#Data preprocessing

names(HouseData)=c("DATE_POSTED","BHK","RENTAL","SIZE","FLOOR","AREA_TYPE",
"LOCALITY_OF_AREA","CITY","FURNISHING_STATUS","TENANT_PREFERRED","REST_ROOM","CONTACT_PERSON")
HouseData


#view heading name
names(HouseData)

#view all data in table form
View(HouseData)


# view class
class(HouseData)


str(HouseData)
#access column data and data type

#view summary of all columns
summary (HouseData)

#find the unique values in each column
factor (HouseData$BHK)
factor (HouseData$RENTAL)
factor (HouseData$SIZE)
factor (HouseData$FLOOR)
factor (HouseData$AREA_TYPE)
factor (HouseData$LOCALITY_OF_AREA)
factor (HouseData$CITY)
factor (HouseData$FURNISHING_STATUS)
factor (HouseData$TENANT_PREFERRED)
factor (HouseData$REST_ROOM)
factor (HouseData$CONTACT_PERSON)

install.packages("tidyverse")
install.packages("magrittr") # package installations are only needed the first time you use it
install.packages("dplyr")


library(ggplot2)
library(tidyverse)
library(plotly)
library(dplyr)
library(corrplot)
library(magrittr)
library(plotrix)
library(base)
library("RColorBrewer")

# to see available color palettes
display.brewer.all()



#Data cleaning
boxplot(HouseData$RENTAL,main= "RENTAL Outlier")
boxplot(HouseData$SIZE,main= "SIZE Outlier")



#Rental Remove outlier
MinRental <- quantile(HouseData$RENTAL, 0.010)
MxRental<- quantile(HouseData$RENTAL, 0.990)
HouseData <- subset(HouseData, HouseData$RENTAL >
                      MinRental & HouseData$RENTAL < MxRental)

#Size Remove outlier
MinSize <- quantile(HouseData$SIZE, 0.010)
MxSize<- quantile(HouseData$SIZE, 0.990)
HouseData <- subset(HouseData, HouseData$SIZE >
                      MinSize & HouseData$SIZE < MxSize)



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


# QUESTION 1

#Question 1.which city is the best city for rental based on the  number of rental, bhk,bathroom, size, area type and locality?


#1)Average rental in each city

       #Sort
Avg_rental_city<-HouseData%>%group_by(CITY)%>%summarise(Avg_rent=mean(RENTAL))%>%arrange(desc(Avg_rent))
Avg_rental_city

       #Plot graph
Avg_rental_city_gg<-ggplot(Avg_rental_city, aes(CITY, Avg_rent, fill = Avg_rent)) +
  geom_bar(stat='Identity',width=0.5,fill=brewer.pal(n=6, name="Dark2")) +
  labs(title='Average Rent by City', x='City',y='Average Rent')+
  geom_text(aes(label= Avg_rent))+theme_bw()
Avg_rental_city_gg+coord_flip()

#2)Total number of available Houses in each city

       #Sort
Total_Rooms_City<-HouseData%>%group_by(CITY)%>%summarise(Count=length(BHK))%>%arrange(desc(Count))
Total_Rooms_City

       #Plot graph
Total_Rooms_gg<-ggplot(Total_Rooms_City, aes(CITY, Count, fill = Count)) +
geom_bar(stat='Identity',width=0.5, fill=brewer.pal(n=6, name="Accent")) +
  labs(title='Number of available houses in each city', x='City', y='Number of house')+ theme_bw()+
  geom_text(aes(label=Count))
Total_Rooms_gg

#3)what is the min, max and Average size of room for rent in each city?

      #Sort for average size
Avg_size_city<-HouseData%>%group_by(CITY)%>%summarise(Avg_size=mean(SIZE))%>%arrange(desc(Avg_size))
Avg_size_city

      #Plot graph
ggplot(HouseData,aes(x=CITY,y=SIZE))+
  geom_boxplot(fill=brewer.pal(n=6, name="Accent"))+
  labs(title='Min, Max and Average size of room for rent in each city', x='CITY', y='SIZE')+ theme_bw()
  theme_classic()

#4)what is the highest rent in each city?

       #Sort
max_RENT_City<-HouseData%>%group_by(CITY)%>%summarise(Count=max(RENTAL))
max_RENT_City


      #Plot graph
ggplot(max_RENT_City,aes(x=CITY,y=Count))+
  geom_point(aes(color=CITY),size= 5)+geom_segment(aes(x=CITY,xend=CITY,y=0, yend=Count))+
  labs(title='Highest rent in each city', x='CITY', y='RENT')+geom_text(aes(label=Count),vjust=-1,size =3)+
  theme_bw()


#5)what is the Minimum rent in each city?

min_RENT_City<-HouseData%>%group_by(CITY)%>%summarise(Count=min(RENTAL))
min_RENT_City

ggplot(min_RENT_City,aes(x=CITY,y=Count))+
  geom_point(aes(color=CITY),size= 7)+geom_text(aes(label=Count),vjust=-1,size =5)+
  labs(title='Minimum rent in each city', x='City', y='Rental')+theme_bw()


#6) what is the Average number of bathrooms available in each house in each city

Avg_REST_ROOM<-HouseData%>%group_by(CITY)%>%summarise(Avg_rent=mean(REST_ROOM))%>%arrange(desc(Avg_rent))
Avg_REST_ROOM

ggplot(Avg_REST_ROOM,aes(x=CITY,y=Avg_rent))+
  geom_point(aes(color=CITY),size=5)+geom_segment(aes(x=CITY,xend=CITY,y=0, yend=Avg_rent ))+
  geom_text(aes(label=Avg_rent),vjust=-1,size =4)+
  labs(title='Average number of bathrooms available in each house in each city', x='City', y='Average bathrooms')+
  theme_bw()


#7) How many unique localities of area are there in each city


house_area_local = HouseData%>%group_by(CITY)%>%summarise(Count=length(unique(LOCALITY_OF_AREA)))
house_area_local

ggplot(house_area_local,aes(x=CITY,y=Count))+
  labs(title='Localities of area in each city', x='city', y='Number of localities')+
  geom_point(aes(color=CITY),size=9, shape= 8)+ geom_text(aes(label=Count),vjust=-1,size =3)+
  theme_bw()




#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#Question 2

#Question 2.What kind of house does a bachelor prefer?

Bach<-HouseData[HouseData$TENANT_PREFERRED=="Bachelors", ]

#1) What is the area type bachelors prefer?


C_Area = nrow(Bach[Bach$AREA_TYPE=="Carpet Area",])
S_Area = nrow(Bach[Bach$AREA_TYPE=="Super Area",])

VAL= c(C_Area,S_Area)
l = c("Carpet Area", "Super Area")

pctg = round(VAL/sum(VAL)*100)
l = paste(l, pctg)
l = paste(l,"%",sep="")

pie(VAL,l,radius=1,main="Area Types Preferred By Bachelors",col=c("orange","purple"),clockwise=TRUE)


#2) Which city do most bachelors prefer to stay?


CITY<-Bach%>%group_by(CITY)%>%summarise(Count=length(CITY))%>%arrange(desc(Count))
CITY

bar<-ggplot(CITY, aes(CITY, Count)) + geom_bar(stat='Identity',width=0.6,fill=brewer.pal(n=6, name="Paired")) +
  labs(title='Preffered cities by bachelors', x='City', y='Tenant count')+geom_text(aes(label=Count))+theme_bw()
bar


#3) Which type of furnishing do bachelors like?


furn1_bach = nrow(Bach[Bach$FURNISHING_STATUS=="Furnished",])


furn2_bach = nrow(Bach[Bach$FURNISHING_STATUS=="Unfurnished",])


furn3_bach = nrow(Bach[Bach$FURNISHING_STATUS=="Semi-Furnished",])


VALJ= c(furn1_bach,furn2_bach,furn3_bach)
lJ = c("Furnished", "Unfurnished","Semi-Furnished")

pctg = round(VALJ/sum(VALJ)*100)
lJ = paste(lJ, pctg)
lJ = paste(lJ,"%",sep="")

pie(VALJ,lJ,radius=1,main="Furnishing status Preferred By Bachelors",
    col=c("red","pink","yellow"),clockwise=TRUE)



#4) What type of contact do bachelors prefer

Bach_contact = Bach%>%group_by(CONTACT_PERSON)%>%summarise(count=length(CONTACT_PERSON ))
Bach_contact


bar<-ggplot(Bach_contact, aes(CONTACT_PERSON, count, fill = count)) + geom_bar(stat='Identity',width=0.6) +
labs(title='Preffered contact person by bachelors', x='point of contact', y='Count')+geom_text(aes(label=count))+
theme(plot.background = element_rect(fill="green"), legend.position="none")
bar


#5) What is the average number of bathrooms bachelors prefer compared to other tenants?


bathrooms<-HouseData%>%group_by(TENANT_PREFERRED )%>%summarise(Count=mean(REST_ROOM))%>%arrange(desc(Count))
bathrooms

bathrooms_gg<-ggplot(bathrooms, aes(TENANT_PREFERRED, Count, fill = Count)) +
  geom_bar(stat='Identity',width=0.5,fill=heat.colors(3)) +
  labs(title='Average number of bathrooms bachelors prefer?', y='Average', x='Tenants')+
  geom_text(aes(label=Count))+theme_bw()+coord_flip()
bathrooms_gg


#6) what are the Top 7 floor preference to bachelors

Bach_FLOOR=Bach%>% group_by(FLOOR) %>% summarize(Count = length(FLOOR)) %>%
  arrange(desc(Count)) %>% slice(1:7)
Bach_FLOOR

Bach_FLOOR_gg<-ggplot(Bach_FLOOR, aes(Count,FLOOR, fill = Count)) +
  geom_bar(stat='Identity',width=0.5,fill=brewer.pal(n=7, name="Set3")) +
  labs(title='Top 7 Floors bachelors prefer', x='number of bachelors', y='Floor')+
  geom_text(aes(label=Count))+ theme_bw()
Bach_FLOOR_gg


#7) what are the Top 10 Locality  bachelors prefer the most


Bach_locality=Bach%>% group_by(LOCALITY_OF_AREA) %>% summarize(Count = length(LOCALITY_OF_AREA)) %>%
  arrange(desc(Count)) %>% slice(1:10)
Bach_locality

Bach_locality_gg<-ggplot(Bach_locality, aes(Count, LOCALITY_OF_AREA)) +
  geom_point(aes(color=LOCALITY_OF_AREA),size= 5)+geom_segment(aes(x=0,xend=Count,y=LOCALITY_OF_AREA,
  yend=LOCALITY_OF_AREA ))+ geom_text(aes(label=Count),vjust=-1)+
  labs(title='Top 10 Localities bachelors prefer', x='Count', y='Locality')+ theme_bw()
Bach_locality_gg

#8) How many BHK do bachelors expect

Bach_bhk = Bach%>%group_by(BHK)%>%summarise(count=length(BHK))
Bach_bhk

Bach_bhk_gg= ggplot(Bach_bhk ,aes(x=BHK,y=count))+ geom_line(color="red",size= 5)+
  labs(title="BHK bachelors expect", x='BHK', y='number of bachelors')+ theme_bw()
Bach_locality_gg
Bach_bhk_gg

#9) What is the rental do bachelors prefer.

Bach_rent = Bach%>%group_by(RENTAL)%>%summarise(Count=length(RENTAL))
Bach_rent

Bach_rent_gg= ggplot(Bach_rent,aes(x=Count,y=RENTAL))+
  geom_histogram(stat= "identity",colour="orange",fill="red")+
  labs(title='What is the rental do bachelors prefer',x='Number of bachelors')+theme_bw()
Bach_rent_gg












#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX





# Question 3

#Question 3 what is the most preferred furnishing status by Bachelors/Family in each city?

#1) most preferred furnishing status by Bachelors/Family in CHENNAI

BSC=nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Semi-Furnished" & HouseData$CITY == "Chennai",])


BUC =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Unfurnished" & HouseData$CITY == "Chennai",])


BFC =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Furnished" & HouseData$CITY == "Chennai",])


VAL1= c(BSC,BUC,BFC)
lA = c("Semi-Furnished", "Unfurnished","Furnished")

pctg = round(VAL1/sum(VAL1)*100)
lA = paste(lA, pctg)
lA = paste(lA,"%",sep="")

pie(VAL1,lA,radius=1,main="Furnishing status preference by Bachelors/Family in Chennai",
    col=c("orange","purple","blue"),clockwise=TRUE)



#2) most preferred furnishing status by Bachelors/Family in BANGALORE

BSB =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Semi-Furnished" & HouseData$CITY == "Bangalore",])


BUB =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Unfurnished" & HouseData$CITY == "Bangalore",])


BFB =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Furnished" & HouseData$CITY == "Bangalore",])

VAL2= c(BSB,BUB,BFB)
lB= c("Semi-Furnished", "Unfurnished","Furnished")

pctg = round(VAL2/sum(VAL2)*100)
lB = paste(lB, pctg)
lB = paste(lB,"%",sep="")

pie3D(VAL2,labels=lB,explode=.4,main="Furnishing status preference by Bachelors/Family in BANGALORE")




#3) most preferred furnishing status by Bachelors/Family in Kolkata

BSK =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Semi-Furnished" & HouseData$CITY == "Kolkata",])

BUK =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Unfurnished" & HouseData$CITY == "Kolkata",])


BFK =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Furnished" & HouseData$CITY == "Kolkata",])

VAL3= c(BSK,BUK,BFK)
lC = c("Semi-Furnished", "Unfurnished","Furnished")

pctg = round(VAL3/sum(VAL3)*100)
lC = paste(lC, pctg)
lC = paste(lC,"%",sep="")


pie(VAL3,lC,radius=1,main="Furnishing status preference by Bachelors/Family in Kolkata",
    col=c("green","yellow","orange"),clockwise=TRUE)




#4) most preferred furnishing status by Bachelors/Family in Delhi

BSD =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Semi-Furnished" & HouseData$CITY == "Delhi",])


BUD =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Unfurnished" & HouseData$CITY == "Delhi",])


BFD =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Furnished" & HouseData$CITY == "Delhi",])

VAL4= c(BSD,BUD,BFD)
lD= c("Semi-Furnished", "Unfurnished","Furnished")

pctg = round(VAL4/sum(VAL4)*100)
lD = paste(lD, pctg)
lD = paste(lD,"%",sep="")
pie3D(VAL4,labels=lD,explode=.4,main="Furnishing status preference by Bachelors/Family in Delhi")


#5) most preferred furnishing status by Bachelors/Family in Mumbai

BSM =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Semi-Furnished" & HouseData$CITY == "Mumbai",])


BUM =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Unfurnished" & HouseData$CITY == "Mumbai",])


BFM =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Furnished" & HouseData$CITY == "Mumbai",])


VAL5= c(BSM,BUM,BFM)
lE = c("Semi-Furnished", "Unfurnished","Furnished")

pctg = round(VAL5/sum(VAL5)*100)
lE = paste(lE, pctg)
lE = paste(lE,"%",sep="")

pie(VAL5,lE,radius=1,main="Furnishing status preference by family in Mumbai",
    col=c("orange","purple","blue"),clockwise=TRUE)


#6) most preferred furnishing status by Bachelors/Family in Hyderabad
BSH =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Semi-Furnished" & HouseData$CITY == "Hyderabad",])


BUH =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Unfurnished" & HouseData$CITY == "Hyderabad",])


BFH =nrow(HouseData [HouseData$TENANT_PREFERRED == "Bachelors/Family" &
HouseData$FURNISHING_STATUS == "Furnished" & HouseData$CITY == "Hyderabad",])


VAL6= c(BSH,BUH,BFH)
lF= c("Semi-Furnished", "Unfurnished","Furnished")

pctg = round(VAL6/sum(VAL6)*100)
lF = paste(lF, pctg)
lF = paste(lF,"%",sep="")

pie3D(VAL6,labels=lF,explode=.4,main="Furnishing status preference by Bachelors/Family in Hyderabad")


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX


#Question 4

#4.What are the available options for a tenant who is looking for a house with a rental price Between Rs20000 and Rs30000.

#1) How many houses are available for each number of BHK in this budget for Tenants


budget = HouseData[(HouseData$RENTAL>=20000 & HouseData$RENTAL<=30000),]
budget

Bhk_budget =  budget%>%group_by(BHK)%>%summarise(count=length(BHK))
Bhk_budget

Bhk_budget_gg= ggplot(Bhk_budget,aes(x=BHK,y=count))+
  geom_histogram(stat= "identity",colour="orange",fill="violetred1")+
  labs(title='Number of houses available for this budget ', y='Number of house')+
  geom_text(aes(label=count))+ theme_bw()
Bhk_budget_gg

#2) How many houses are available with each area type in this budget for Tenants?

AreaType_budget = budget%>%group_by(AREA_TYPE)%>%summarise(count=length(AREA_TYPE))
AreaType_budget

AreaType_budget_gg<-ggplot(AreaType_budget, aes(AREA_TYPE, count)) +
  geom_bar(stat='Identity',width=0.6,fill="slateblue1") +
  labs(title='Number of area type available for this Size ',
  x='Area Type', y='Number of house ')+
  geom_text(aes(label=count)) +theme_bw()
AreaType_budget_gg

#3) How many houses does each city has with this budget for Tenants?


city_budget = budget%>%group_by(CITY)%>%summarise(count=length(CITY))
city_budget

city_budget_gg= ggplot(city_budget,aes(x=CITY,y=count))+
  geom_histogram(stat= "identity",colour="orange",fill=brewer.pal(n=6, name="Dark2"))+
  labs(title='Number of houses in each city with this budget ', y='Number of houses')+
  geom_text(aes(label=count))+theme_bw()
city_budget_gg

#4) What are the sizes available for this budget for tenants

Size_budget = budget%>%group_by(SIZE)%>%summarise(count=length(SIZE))
Size_budget

Size_budget_gg= ggplot(Size_budget,aes(x=SIZE,y=count,fill = SIZE))+ geom_point(stat= "identity",shape=23,size=5)+
labs(title='Variety of sizes available in this budget ',y='Number of houses')+theme_bw()
Size_budget_gg



#5)  What are the type of contact that are available for this budget for tenants

contact_budget = budget%>%group_by(CONTACT_PERSON)%>%summarise(count=length(CONTACT_PERSON))
contact_budget

contact_budget_gg<-ggplot(contact_budget, aes(CONTACT_PERSON, count, fill = count)) +
  geom_bar(stat='Identity',width=0.6,fill="green") +
  labs(title='Number of contact type available for this budget ', x='Contact person', y='Number of houses')+
  geom_text(aes(label=count))+theme_bw()
contact_budget_gg



#6) Who are the  Tenants preferred for the houses in this this budget.

AreaType_budget = budget%>%group_by(TENANT_PREFERRED)%>%summarise(count=length(TENANT_PREFERRED))
AreaType_budget

AreaType_budget_gg= ggplot(AreaType_budget,aes(x=TENANT_PREFERRED,y=count))+
  geom_point(aes(color=TENANT_PREFERRED),size=4, shape= 3)+ geom_text(aes(label=count),vjust=-1,size =3)+
  labs(title='Tenants preferred for houses in this budget ', x='Tenants', y='Number of tenants')+
  theme_bw()
AreaType_budget_gg



#7) how many houses are there with each number of bathrooms for this budget

bathroom_budget = budget%>%group_by(REST_ROOM)%>%summarise(count=length(REST_ROOM))
bathroom_budget

ggplot(bathroom_budget,aes(x=REST_ROOM,y=count))+
  geom_line(color="blue",size=1.2)+geom_point()+
  labs(title='houses with number of bathrooms for this budget ', x='bathroom', y='total of bathrooms')+
  geom_text(aes(label=count),vjust=-1,size =3)+theme_bw()
bathroom_budget_gg


#8) What are the rental options that are available in the budget range

Rental_budget = budget%>%group_by(RENTAL)%>%summarise(count=length(RENTAL))
Rental_budget
ggplot(Rental_budget,aes(x=RENTAL,y=count))+
labs(title='Rental options that are available in the budget ', y='Total of houses')+
  geom_point(aes(color=RENTAL),size=7)+theme_bw()

#9) What are the top 10 floors available for this budget range

Floor_budget =budget%>% group_by(FLOOR) %>% summarize(Count = length(FLOOR)) %>%
  arrange(desc(Count)) %>% slice(1:10)
Floor_budget

Floor_budget_gg<-ggplot(Floor_budget, aes(Count, FLOOR, fill = Count)) +
  geom_bar(stat='Identity',width=0.5,fill=brewer.pal(n=10, name="Set3")) +
  labs(title='Top 10 Floors available for this budget', x='Number of houses', y='Floor')+
  geom_text(aes(label=Count))+theme_bw()
Floor_budget_gg


#10) What are the top 7 localities available for this budget range

Locality_budget =budget%>% group_by(LOCALITY_OF_AREA) %>% summarize(Count = length(LOCALITY_OF_AREA)) %>%
  arrange(desc(Count)) %>% slice(1:7)
Locality_budget

Locality_budget_gg<-ggplot(Locality_budget, aes(Count, LOCALITY_OF_AREA, fill = Count)) +
  geom_bar(stat='Identity',width=0.5,fill=brewer.pal(n=7, name="Set2")) +
  labs(title='top 7 localities available for this budget', y='Locality', x='Total of houses')+
  geom_text(aes(label=Count))+theme_bw()
Locality_budget_gg


#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

# Question 5

#5.What are the available options for a tenant who is looking for a house with a size between 1200 to 2000

Size = HouseData[(HouseData$SIZE>=1200 & HouseData$SIZE<=2000),]
Size

#1) How many houses are available for each number of BHK in this Size  for Tenants


Size_BHK =  Size%>%group_by(BHK)%>%summarise(count=length(BHK))
Size_BHK

Size_BHK_gg= ggplot(Size_BHK,aes(x=BHK,y=count))+ geom_histogram(stat= "identity",colour="orange",fill="red")+
  labs(title='Houses for each number of BHK in this Size', y='Total of houses')+geom_text(aes(label=count))+
  theme_bw()
Size_BHK_gg


#2) How many houses are available with each area type in this Size  for Tenants?

Size_area =  Size%>%group_by(AREA_TYPE)%>%summarise(count=length(AREA_TYPE))
Size_area

Size_area_gg<-ggplot(Size_area, aes(AREA_TYPE, count)) + geom_bar(stat='Identity',width=0.6,fill="cyan") +
  labs(title='Number of area type available for this Size ', x='Area Type', y='Number of house')+
  geom_text(aes(label=count))+theme_bw()
Size_area_gg



#3) How many houses does each city has with this Size  for Tenants?



Size_city= Size%>%group_by(CITY)%>%summarise(count=length(CITY))
Size_city

Size_city_gg= ggplot(Size_city,aes(x=CITY,y=count))+
  geom_histogram(stat= "identity",colour="orange",fill="royalblue1")+
  labs(title='Number of houses in each city with this Size ', y='Number of houses')+
  geom_text(aes(label=count))+theme_bw()
Size_city_gg


#4) What are the specific sizes available for this Size range for tenants

Size_pref = Size%>%group_by(SIZE)%>%summarise(count=length(SIZE))
Size_pref

Size_budget_gg= ggplot(Size_pref,aes(x=SIZE,y=count,fill= count))+ geom_point(stat= "identity",shape=17,size =4) +
  labs(title='Specific sizes available for this Size range ', y='Number of sizes')+
  facet_wrap(~count)
Size_budget_gg




#5)  What are the type of contact that are available for this Size  for tenants

Size_contact = Size%>%group_by(CONTACT_PERSON)%>%summarise(count=length(CONTACT_PERSON))
Size_contact


Size_contact_gg<-ggplot(Size_contact, aes(CONTACT_PERSON, count, fill = count)) +
  geom_bar(stat='Identity',width=0.6,fill="green") +
  labs(title='Contacts available for this Size ', x='Contact person', y='Number of contacts')+
  geom_text(aes(label=count))+theme_bw()
Size_contact_gg


#6) Who are the  Tenants preferred for the houses in this this Size .

Size_tenants = Size%>%group_by(TENANT_PREFERRED)%>%summarise(count=length(TENANT_PREFERRED))
Size_tenants

Size_tenants_gg= ggplot(Size_tenants,aes(x=TENANT_PREFERRED,y=count))+
  geom_point(aes(color=TENANT_PREFERRED),size=9, shape= 1)+ geom_text(aes(label=count),vjust=-1,size =3)+
  labs(title='Tenants preferred for houses in this Size ', x='Tenants', y='Number of tenants')+
  theme_bw()
Size_tenants_gg

#7) how many houses are there with each number of bathrooms for this Size


Size_bathroom = Size%>%group_by(REST_ROOM)%>%summarise(count=length(REST_ROOM))
Size_bathroom

Size_budget_gg= ggplot(Size_bathroom,aes(x=REST_ROOM,y=count))+geom_line(color="red",size=1.3)+geom_point()+
  labs(title='Number of bathrooms in houses for this Size ', y='Number of houses')+
  geom_text(aes(label=count),vjust=-1,size =3)+ theme_bw()
Size_budget_gg

#8) What are the rental options that are available in the budget range

Size_Rental = Size%>%group_by(RENTAL)%>%summarise(count=length(RENTAL))
Size_Rental

ggplot(Size_Rental,aes(x=RENTAL,y=count))+
  geom_point(aes(color=RENTAL),size=5)+scale_x_continuous(limits=c(1,100000))+scale_y_continuous(limits=c(1,45))+
  labs(title='Rental options available for this Size ', y='Number of houses')+theme_bw()



#9) What are the top 7 floors available for this budget range

Size_Floor =Size%>% group_by(FLOOR) %>% summarize(Count = length(FLOOR)) %>%
  arrange(desc(Count)) %>% slice(1:10)
Size_Floor


Size_Floor_gg<-ggplot(Size_Floor, aes(Count, FLOOR, fill = Count)) +
  geom_bar(stat='Identity',width=0.5,fill=brewer.pal(n=10, name="Set3")) +
  labs(title='Top 10 Floors available for this size', x='Number of houses', y='Floor')+
  geom_text(aes(label=Count) )+theme_bw()
Size_Floor_gg

#10) What are the top 7 localities available for this budget range

Size_Locality =Size%>% group_by(LOCALITY_OF_AREA) %>% summarize(Count = length(LOCALITY_OF_AREA)) %>%
  arrange(desc(Count)) %>% slice(1:7)
Size_Locality

Size_Locality_gg<-ggplot(Size_Locality, aes(Count, LOCALITY_OF_AREA, fill = Count)) +
  geom_bar(stat='Identity',width=0.5,fill=brewer.pal(n=7, name="Set2")) +
  labs(title='Top 7 localities available for this size', x='Number of houses', y='Localities')+
  geom_text(aes(label=Count) )+ theme_bw()
Size_Locality_gg



#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

