##Notes for paper; all input files were added as csv files for simplicity; file names were modified for simplicity in coding; data from sierra was titled books.csv, and data from illiad was titled ill.csv

##Load required packages
require(plyr)
require(ggplot2)

##Designate the subject librarian
##Replace name with a unique identifier
lib<-c("name")

##Designate relevant sections
##Replace LC with the appropriate LC Subclass, and 1:999 with the range(s) that are relevant within that LC Subclass
##If multiple LC Subclasses are needed replace the n in sn and snrange with a number (1, 2...) and duplicate these two rows for each class
sn<-c("LC")
snrange<-c(1:999)

##Create a vector containing all desired call numbers
##Replace sn and snrange with the vector names used above (ex. s1 and s1range, s2 and s2range), and ensure that all vectors created above are included on this line
match<-c(paste(sn, snrange), paste(sn, snrange))

##Read in and merge descriptor files
##Replace descriptor.csv with the name of your csv file
##Duplicate the first row for each of the relevant LC Subclasses, changing the name to S1, S2, etc. as needed
##Modify the second row to include all iterations of the first row (ex. (S1, S2, S3))
Sn<-read.csv("descriptor.csv")
descriptors<-rbind(Sn, Sn)

##Read in circulation source data from larger collection analysis project
books<-read.csv("books.csv")

##Create a column in the source data with LC Number and a match column
books$split<-books$full_call_number
books$split2<-gsub("[A-Z]+", "", books$split)
books$LC_Number<-as.numeric(sapply(strsplit(as.character(books$split2), '\\.'), "[", 1))
books$match<-paste(books$LC_subcategory, books$LC_Number)

##Separate out only books that match each of the designated sections
bookssub<-books[books$match %in% match,]

##Create a new data frame with the LC subcategory and minimimun and maximum call number for each descriptor
min<-aggregate(LC_Number ~ Description, data=descriptors, FUN=min)
max<-aggregate(LC_Number ~ Description, data=descriptors, FUN=max)
category<-descriptors[match(unique(descriptors$Description), descriptors$Description),]
category<-merge(category, min, by="Description")
category<-merge(category, max, by="Description")
category<-category[ -c(5)]
descriptors<-descriptors[ -c(1:2)]

##Merge books subset for relevant sections with the descriptor data frame and write the books subset into a csv file
bookssub<-merge(bookssub, descriptors, by="match", all.x=TRUE)
bookssub2<-bookssub[ -c(15:16)]
write.csv(bookssub2, paste(lib, "BooksSub.csv", sep=""))

##Count the number of books in each section
count<-as.data.frame(table(bookssub$Description))

##Sum the total checkouts and the ytd checkouts for each section and calculate the total mean, median, and maximum checkouts for each section
ytdcheckouts<-aggregate(YTD_checkout_tot ~ Description, data=bookssub, FUN="sum")
checkouts<-aggregate(checkout_total ~ Description, data=bookssub, FUN="sum")
checkoutmean<-aggregate(checkout_total ~ Description, data=bookssub, FUN="mean")
checkoutmedian<-aggregate(checkout_total ~ Description, data=bookssub, FUN="median")
checkoutmax<-aggregate(checkout_total ~ Description, data=bookssub, FUN="max")

##Merge the counts, ytd checkouts, and total checkouts into one data frame
bookssum<-merge(count, ytdcheckouts, by.x="Var1", by.y="Description")
bookssum<-merge(bookssum, checkouts, by.x="Var1", by.y="Description")
bookssum<-merge(bookssum, checkoutmean, by.x="Var1", by.y="Description")
bookssum<-merge(bookssum, checkoutmedian, by.x="Var1", by.y="Description")
bookssum<-merge(bookssum, checkoutmax, by.x="Var1", by.y="Description")

##Merge the count and checkout information with the category information and clean up naming conventions
bookssum<-merge(category, bookssum, by.x="Description", by.y="Var1")
bookssum<-bookssum[ -c(4)]
names(bookssum)<-c("Description", "LC_Subcategory", "Start_LC_Number", "End_LC_Number", "Items", "YTD_Checkouts", "Checkouts", "Mean_Checkouts", "Median_Checkouts", "Max_Checkouts")

##Read in ILL source data from larger collection analysis project
ill<-read.csv("ill.csv")

##Create a column in the source data with LC Number and a match column
ill$split<-ill$Call.Number.Congress...Other
ill$split2<-gsub("[A-Z]+", "", ill$split)
ill$LC_Number<-as.numeric(sapply(strsplit(as.character(ill$split2), '\\.'), "[", 1))
ill$match<-paste(ill$LC_Subclasses, ill$LC_Number)

##Separate out only ILL requests that match each of the designated sections
illsub<-ill[ill$match %in% match,]

##Merge ILL subset for relevant sections with the descriptor data frame and write the ILL subset into a csv file
illsub<-merge(illsub, descriptors, by="match", all.x=TRUE)
illsub2<-illsub[ -c(18:19)]
write.csv(illsub2, paste(lib, "ILLSub.csv", sep=""))

##Count the number of books requested through ILL for each section and clean up naming conventions
illcount<-as.data.frame(table(illsub$Description))
names(illcount)<-c("Description", "Requests")

##Merge the booksum and illsum data sets to create an overall summary data set
sum<-merge(bookssum, illcount, by="Description", all=TRUE)

##Identify rows with missing data (typically categories in which we have no books)
sum[c("Items", "YTD_Checkouts", "Checkouts", "Requests")][is.na(sum[c("Items", "YTD_Checkouts", "Checkouts", "Requests")])]<-0
sum<-merge(sum, category, by="Description")
sum<-sum[ -c(12:15)]
names(sum)<-c("Description", "LC_Subcategory", "Start_LC_Number", "End_LC_Number", "Items", "YTD_Checkouts", "Checkouts", "Mean_Checkouts", "Median_Checkouts", "Max_Checkouts", "Requests")
sum$Requests<-as.numeric(sum$Requests)

##Create percentage columns for frequency, YTD checkouts, and total checkouts
sum$P_Items<-sum$Items/(sum(sum$Items))
sum$P_YTD_Checkouts<-sum$YTD_Checkouts/(sum(sum$YTD_Checkouts))
sum$YTD_Use<-sum$P_YTD_Checkouts/sum$P_Items
sum$YTD_Rating<-c("Underused")
sum$YTD_Rating[sum$YTD_Use>1]<-c("Overused")
sum$P_Checkouts<-sum$Checkouts/(sum(sum$Checkouts))
sum$Use<-sum$P_Checkouts/sum$P_Items
sum$Rating<-c("Underused")
sum$Rating[sum$Use>1]<-c("Overused")

##Create a new field to illustrate changes in usage over time
sum$Usage_Change<-sum$YTD_Use - sum$Use
sum$Trend<-c("Long Term Trend")
sum$Trend[sum$Usage_Change>.05]<-c("Increasing Use")
sum$Trend[sum$Usage_Change<(-.05)]<-c("Decreasing Use")

##Create percentage columns for the count of ill requests
sum$P_Requests<-sum$Requests/(sum(sum$Requests))
sum$Borrowing<-sum$P_Requests/(sum$P_Items)
is.na(sum)<-is.na(sum)
sum[c("YTD_Use", "Use", "Usage_Change", "Borrowing")][is.na(sum[c("YTD_Use", "Use", "Usage_Change", "Borrowing")])]<-0

##Calculate the average and standard deviation for the ill borrowing ratio
m<-mean(sum$Borrowing)
s<-sd(sum$Borrowing)

##Calculate an ill rating based on the ratio, the mean, and the standard deviation:  below mean = low demand, above mean = high demand, above mean+sd = very high demand, above mean+2sd = extremely high demand
sum$ILL_Rating<-c("Low Demand")
sum$ILL_Rating[sum$Borrowing>m]<-c("High Demand")
sum$ILL_Rating[sum$Borrowing>(m+s)]<-c("Very High Demand")
sum$ILL_Rating[sum$Borrowing>(m+2*s)]<-c("Extremely High Demand")

##Calculate recommendations for YTD and all time usage based on the YTD and all time ratings and the ILL ratings
sum$YTD_Recommendation<-c("Ease Off")
sum$YTD_Recommendation[sum$YTD_Rating=="Overused" & sum$ILL_Rating %in% c("High Demand", "Very High Demand", "Extremely High Demand")]<-c("Growth Opportunity")
sum$YTD_Recommendation[sum$YTD_Rating=="Underused" & sum$ILL_Rating %in% c("High Demand", "Very High Demand", "Extremely High Demand")]<-c("Change in Purchasing")
sum$YTD_Recommendation[sum$YTD_Rating=="Overused" & sum$ILL_Rating=="Low Demand"]<-c("No Changes")
sum$Recommendation<-c("Ease Off")
sum$Recommendation[sum$Rating=="Overused" & sum$ILL_Rating %in% c("High Demand", "Very High Demand", "Extremely High Demand")]<-c("Growth Opportunity")
sum$Recommendation[sum$Rating=="Underused" & sum$ILL_Rating %in% c("High Demand", "Very High Demand", "Extremely High Demand")]<-c("Change in Purchasing")
sum$Recommendation[sum$Rating=="Overused" & sum$ILL_Rating=="Low Demand"]<-c("No Changes")

##Write to an excel file
write.csv(sum, paste(lib, "Summary.csv", sep=""))

##Find mean, standard deviation, and maximum values for the YTD and total checkouts for each category
range<-ddply(bookssub, ~Description, summarise, YTD_Mean=mean(YTD_checkout_tot), YTD_SD=sd(YTD_checkout_tot), YTD_Max=max(YTD_checkout_tot), Mean=mean(checkout_total), SD=sd(checkout_total), Max=max(checkout_total))

##Merge the summary set with the descriptions using the category data set
range<-merge(range, category, by="Description")

##Sort the summary set by call number
range$LC_Number.x<-as.numeric(range$LC_Number.x)
range<-range[with(range, order(LC_Subcategory, LC_Number.x)),]
range$Category<-factor(range$match, levels=range$match)

##Create a plot of average usage by category with standard deviation (error bars) and maximum value (points)
pdf(paste(lib, "Plot.pdf", sep=""), width=40, height=20)
b1<-ggplot(range, aes(x=Category, y=Mean))+
  geom_bar(position=position_dodge(), stat="identity", fill="red1")+
  geom_errorbar(aes(ymin=Mean-SD, ymax=Mean+SD), width=.2, position=position_dodge(.9))+
  theme(axis.text.x=element_text(angle=50, vjust=0.5))+
  geom_point(data=range, aes(x=match, y=Max))
b1
dev.off()
