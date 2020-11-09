#I do not have the complete data for my project yet, so I went to ICPSR and found 
#a data set with lead level tests in Flint, MI. The variables 
#are not immediately pertinent, but it was still pretty interesting. There are 
#relatively limited variables here, so I focused on the lead test results, zip codes,
#and pipe material. 
library(tidyverse)
library(here)

here()


water_data <- da36955.0002

#means of lead water contamination by zip code
mean1 <- mean(water_data$RESULT_LEAD[water_data$ZIP_CODE==48503])
mean2 <- mean(water_data$RESULT_LEAD[water_data$ZIP_CODE==48504])
mean3 <- mean(water_data$RESULT_LEAD[water_data$ZIP_CODE==48505])
mean4 <- mean(water_data$RESULT_LEAD[water_data$ZIP_CODE==48506])
mean5 <- mean(water_data$RESULT_LEAD[water_data$ZIP_CODE==48507])
mean6 <- mean(water_data$RESULT_LEAD[water_data$ZIP_CODE==48532])

means <- c(mean1, mean2, mean3, mean4, mean5, mean6) #vector for means
zip_codes <- c(48503, 48504, 48505, 48506, 48507, 48532) #vector for ZipCode

zip_means <- data.frame(means, zip_codes) #dataframe for means and Zip

#This graph shows the mean lead levels detected by zipcode. Importantly, 
#48504 is a poorer neighborhood that is majority African American, which 
#clearly far exceeds the 5ppm threshold. The red line added indicates the 
#5ppb lead threshold for water safety. 

mean_lead_graph <- 
  ggplot(zip_means, aes(x=factor(zip_codes), y=means, label=means)) + 
  geom_bar(stat='identity', width=.5) +
  labs(title="Lead Levels by Zip Code", y="Lead (ppb)", x="Zip Code") +
  theme(axis.text.x=element_text(angle=90, hjust=1))
mean_lead_graph

mean_lead_graph +  geom_hline(yintercept=5, color="red")


#Total lead levels detected by pipe material. Interestingly, Copper piping had the 
#most tests for high lead levels, which supports the notion that the water 
#contamination was permeating beyond lead lines and into service lines, which 
#are typically made of copper.
mean_lead_material <- 
  ggplot(water_data, aes(x=factor(SERVICE_LINE_MATERIAL), 
          y=RESULT_LEAD, label=RESULT_LEAD)) + 
          geom_bar(stat='identity', width=.5) +
          labs(title="Lead Levels by Line Material", y="Lead (ppb)", x="Material") +
          theme(axis.text.x=element_text(angle=90, hjust=1))
mean_lead_material  


#This is not that revealing, since I have graphed the total lead measured by date 
#that the test was conducted. Those dates with the highest number are likely 
#due to more tests being conducted that day. Going forward, I should aim to 
#take the mean lead tested per day, but I would need help writing this code 
#out. 
ggplot(water_data, aes(x=DATE_SUBMITTED)) + 
  geom_histogram(stat = 'count', binwidth = 2,
                 color="black", fill="white") +
                 labs(title = "Lead by Date Tested", y = "Total Lead", x = "Date Tested") +
                 theme(axis.text.x=element_text(angle=90, hjust=1, size = 4))

                 