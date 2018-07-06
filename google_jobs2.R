# Importing the dataset
dataset = read.csv('/Users/guptac/Downloads/google-job-skills/job_skills.csv')
# print the top 6 row from the dataframe
head(dataset)


# Compute and show the number of jobs in the cloud computing sector
dataset$cloud_flag <- ifelse( grepl("Google Cloud", dataset$Title)==TRUE, "cloud",
                                    ifelse( grepl( "Google Cloud", dataset$Pref_qual) == TRUE, "cloud",   
                                            ifelse( grepl( "Google Cloud", dataset$Responsibilities) == TRUE, "cloud",  
                                                    "regular"  )))
table(dataset$cloud_flag)
ggplot(dataset) + geom_bar(aes(x = cloud_flag))

# most popular language list 
programing_language_list = list('python', 'java', 'c++', 'php', 'javascript', 'objective-c', 'ruby', 'perl','c','c#', 'sql','kotlin')
programming_language_table = data.frame(Languages_known=c('python', 'java', 'c++', 'php', 'javascript', 'objective-c', 'ruby', 'perl','c','c#', 'sql','kotlin'),no_of_counts = 0 )
#programming_language_table[1,1]
#match(c('python',),programing_language_list)

# get our Minimum Qualifications column and convert all of the values to a list
minimum_qualifications = list(dataset[,6])

# let's join our list to a single string and lower case the letter
miniumum_qualifications_string = sapply(paste(unlist(minimum_qualifications),sep=', ',collapse = ""),tolower)
#install.packages("stringr")
library(stringr)

# find out which language occurs in most in minimum Qualifications string


res_min = regmatches(miniumum_qualifications_string,gregexpr("[\\w'+#-]+|[.!?;']",miniumum_qualifications_string,perl = TRUE))

# this is the frequency table of the list res_min
res_min2=table(res_min)
res_min2=sort(res_min2, decreasing = TRUE)
programming_language_table[1,2]=res_min2["python"]
programming_language_table[2,2]=res_min2["java"]
programming_language_table[3,2]=res_min2["c++"]
programming_language_table[4,2]=res_min2["php"]
programming_language_table[5,2]=res_min2["javascript"]
programming_language_table[6,2]=res_min2["objective-c"]
programming_language_table[7,2]=res_min2["ruby"]
programming_language_table[8,2]=res_min2["perl"]
programming_language_table[9,2]=res_min2["c"]
programming_language_table[10,2]=res_min2["c#"]
programming_language_table[11,2]=res_min2["sql"]
programming_language_table[12,2]=res_min2["kotlin"]


programming_language_popularity=programming_language_table[order(-programming_language_table$no_of_counts),]
colnames(programming_language_popularity)[1] = "Language"
colnames(programming_language_popularity)[2] = "Popularity"

capFirst <- function(s) {
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "")
}

programming_language_popularity$Language = capFirst(programming_language_popularity$Language)

#install.packages("ggplot2")
# library(ggplot2)
# barplot(height =1,width =1,programming_language_popularity, names.arg ="Language" ,xlab="Language",ylab="Popularity",col="blue",
#         main="Programming Languages popularity at Google Jobs",border="red")
# {
#  if(w=='python'){
#    programming_language_table[1,2]=programming_language_table[1,2]+1
#  }
# }


#Plot the language popularity distribution
#install.packages("plotly")
library(plotly)

plot_ly(programming_language_popularity, x = ~Language, y = ~Popularity, type = 'bar', name = 'Programming Languages popularity at Google Jobs')  %>%
  layout(yaxis = list(title = 'Popularity'), barmode = 'group')

#Degree Popularity in Google Jobs
degree_list = list("BA", "BS", "Bachelor's", "PhD")
degree_list_table = data.frame(Degrees_earned=c("BA", "BS", "Bachelor's", "PhD"),no_of_counts = 0 )
# this is the frequency table of the list res_min
res_min = regmatches(miniumum_qualifications_string,gregexpr("[\\w'+#-]+|[.!?;']",miniumum_qualifications_string,perl = TRUE))
res_min3=table(res_min)
res_min3=sort(res_min3, decreasing = TRUE)
degree_list_table[1,2]=res_min3["ba"]
degree_list_table[2,2]=res_min3["bs"]
degree_list_table[3,2]=res_min3["bachelor's"]
degree_list_table[4,2]=res_min3["phd"]

degree_popularity=degree_list_table[order(-degree_list_table$no_of_counts),]
colnames(degree_popularity)[1] = "Degree"
colnames(degree_popularity)[2] = "Popularity"


#Plot the degree popularity 
library(plotly)

plot_ly(degree_popularity, x = ~Degree, y = ~Popularity, type = 'bar', name = 'Degree popularity at Google Jobs')  %>%
  layout(yaxis = list(title = 'Popularity'), barmode = 'group')

# Google Engineers:
#Google is famous for hiring engineers for all types of roles. How many roles need a technical or Engineering degree in Engineering?#
 # As seen in the graph below, 35% of roles need technical expertise, including those in marketing and non-engineering roles. 
#```{r, echo=FALSE, include=TRUE}
dataset$tech_flag <- ifelse( grepl("Engineering", dataset$Minimum.Qualifications) == TRUE, "tech",
                                   ifelse( grepl("technical", dataset$Minimum.Qualifications) == TRUE, "tech",
                                           ifelse( grepl("Engineering", dataset$Preferred.Qualifications) == TRUE, "tech",
                                                   ifelse( grepl("technical", dataset$Preferred.Qualifications)==TRUE, "tech", "other"))))

ggplot(dataset, aes(tech_flag)) + 
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("relative frequencies")

#Experience Popularity in Google

another_res_min = regmatches(miniumum_qualifications_string,gregexpr('([0-9]+) year',miniumum_qualifications_string,perl = TRUE))
res_min4= table(another_res_min)
res_min4 = sort(res_min4,decreasing = TRUE)

experience_popularity = as.data.frame(res_min4) 
colnames(experience_popularity)[1] = "Years_of_Experience"
colnames(experience_popularity)[2] = "Popularity"

#Plot the experience popularity in Google

plot_ly(experience_popularity, x = ~Years_of_Experience, y = ~Popularity, type = 'bar', name = 'Experience popularity at Google Jobs')  %>%
  layout(yaxis = list(title = 'Popularity'), barmode = 'group')

#Let's find out which job category Google wants more experiences
dataset$Experience = str_extract(dataset$Minimum.Qualifications,'([0-9]+) year')
dataset$Qualifications= str_extract(dataset$Minimum.Qualifications,"B(A/BS)|(BA)|(BS)|(Bachelor)|(PhD)")

#Convert the categorical variables to levels as numeric
dataset$Qualifications = match(dataset$Qualifications,unique(dataset$Qualifications))
dataset$Category = match(dataset$Category,unique(dataset$Category))
dataset2 = dataset
#dataset$Qualifications= str_extract(dataset$Minimum.Qualifications,'BS')

# Remove all NA from Experience column of dataset
completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}
# Create a dataframe with two columns Experience, Category
dff=completeFun(dataset,"Experience")
dff = subset(dff,select = c("Experience","Category"))

#plot Experience vs Category 
library(plotly)
plot_ly(dff, x = ~Category, y = ~Experience, type = 'bar', name = 'Experience per Category at Google Jobs')  %>%
  layout(yaxis = list(title = 'Experience per Category'), barmode = 'group')

#Draw a pie chart on no. of full times and internees 
dataset$job_fte <- ifelse( (grepl("Intern ", dataset$Title)) == TRUE, "Internships",
                                 ifelse( (grepl("Intern,", dataset$Title)) == TRUE, "Internships", "Full-time"))


mytable <- table(dataset$job_fte )
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="Pie Chart of Full-time versus part-time")


# where is most job located
threshold = 10
dataset_job_location = dataset$Location
dataset_table_location=table(dataset_job_location)
dataset_table_location=sort(dataset_table_location,decreasing = TRUE)
df_table_loc=as.data.frame(dataset_table_location)

#Plot location Popularity
plot_ly(df_table_loc, x = ~dataset_job_location, y = ~Freq, type = 'bar', name = 'Location Popularity at Google Jobs')  %>%
  layout(yaxis = list(title = 'Location Popularity'), barmode = 'group')

#Most Popular Job Category
dataset_job_category = dataset$Category
dataset_job_category = table(dataset_job_category)
dataset_job_category = sort(dataset_job_category,decreasing = TRUE)
df_table_category = as.data.frame(dataset_job_category)


#Plot category Popularity
plot_ly(df_table_category, x = ~dataset_job_category, y = ~Freq, type = 'bar', name = 'Job Category Popularity at Google Jobs')  %>%
  layout(yaxis = list(title = 'Job Category Popularity'), barmode = 'group')


#Plot category Popularity
plot_ly(dataset, x = ~Category, y = ~Location, type = 'bar', name = 'Job Category location wise Popularity at Google Jobs')  %>%
  layout(yaxis = list(title = 'Job Category location wise Popularity'), barmode = 'group')


# install.packages("wordcloud")
# install.packages("RColorBrewer")
#install.packages("tm")
#install.packages("SnowballC")

library(wordcloud)
library(RColorBrewer)
library(tm)
library(SnowballC)

# Responsibilities - Word Cloud:
#Let us create a word cloud to see what skills are most needed for the Cloud engineering roles:
#  We see that words like "partner", "custom solutions", "cloud", strategy", "experience" are more frequent than any specific technical skills. 
#This shows that the Google cloud roles are best filled by senior resources where leadership and business skills become more significant than expertise in a specific technology.

#```{r, echo=FALSE, include=FALSE}
# Create text_document for all jobs related to Google Cloud
text_dict_source <- subset(dataset, cloud_flag == "cloud" )
# text_dict_source <- subset(google_skills, exp_data >= 12 )
# text_dict_source <- subset(google_skills, Category == "Hardware Engineering")
row.names(text_dict_source) <- NULL

create_text<- function( datadf )
{
  print(nrow(datadf))
  # creating empty array
  textarrdf = c(" ")
  
  for(i in 1:nrow(datadf))
  { 
    temp_arr1 = datadf[i, "Responsibilities"]
    temp_arr2 = datadf[i, "Minimum.Qualifications"]
    temp_arr3 = datadf[i, "Preferred.Qualifications"]
    
    textarrdf = paste(textarrdf, temp_arr1, temp_arr2, temp_arr3, sep = " ")
    
  }
  
  return(textarrdf)
} 


text_sourcedf <- create_text( text_dict_source)



# Clean up the text:
# remove non-ascii characters, if any:
text_sourcedf2 <- iconv(text_sourcedf, "latin1", "ASCII", sub="")

# convert all text to lowercase:
text_sourcedf2 = tolower(text_sourcedf2)


# create word corpus and perform cleaning and pre-processing
wordCorpus <- Corpus(VectorSource(text_sourcedf2))
summary(wordCorpus)

# processing and clean the text
wordCorpus <- tm_map(wordCorpus, removePunctuation)
wordCorpus <- tm_map(wordCorpus, removeNumbers)
wordCorpus <- tm_map(wordCorpus, content_transformer(tolower))
wordCorpus <- tm_map(wordCorpus, removeWords, stopwords("english"))
wordCorpus <- tm_map(wordCorpus, stripWhitespace)
wordCorpus <- tm_map(wordCorpus, stemDocument) 



# create word clouds 
wordCorpus1 <- tm_map(wordCorpus, stemDocument)
# code to create a word cloud:
#```

#```{r, echo=FALSE, include=TRUE}
wordcloud(wordCorpus1, scale=c(5,0.5), max.words=100, random.order=FALSE, 
rot.per=0.35, use.r.layout=FALSE, colors = brewer.pal(8, "Dark2"))



#dataset_table_location = list(dataset_table_location)








#Split the dataset into train and test set

#Remove the "year" keyword from the column Ex 
# dataset2$Experience<-gsub("year","",dataset2$Experience)
# dataset2$Experience =sapply(dataset2[,8], as.numeric)

# Split the dataset into train and test
 library(caTools)
 # set.seed(123)
 # split = sample.split(dataset2$Category,SplitRatio = 4/5)
 # training_set = subset(dataset2, split == TRUE)
 # test_set = subset(dataset2, split == FALSE)

#Removing all NA from train and test set
 library(tidyr)

 # training_set <- training_set%>% drop_na()
 # test_set<- test_set%>%drop_na()
#Fitting a model on the train set
library(rpart)
 
#install.packages("randomForest")
library(randomForest)
 # classifier =  randomForest(x = training_set[,8:9],y=training_set$Category,ntree = 100)

# Predicting the Test set results
#  y_pred = predict(classifier, newdata = test_set[,8:9],type ="class")
# caret::confusionMatrix(as.data.frame(y_pred),as.data.frame(test_set$Category))
# Making the Confusion Matrix
# cm = table(test_set[, 8:9], y_pred)
