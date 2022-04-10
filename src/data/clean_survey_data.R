library(tidyverse)

x <- read.csv('Memorization+Tasks+Survey_April+10,+2022_13.22.csv')
# Clear out the rows with metadata:



# Clear out the timing columns for now
x <- select(x, -matches('Click'))
# x <- select(x, -matches('Submit'))


# Remove previews
x <- filter(x, Status != 'Survey Preview')
x <- select(x, -Status, -IPAddress, -RecordedDate, -ResponseId, -starts_with('Recipient'), -ExternalReference, -DistributionChannel, -UserLanguage, -Q_RecaptchaScore)

names(x) <- tolower(names(x))
x <- rename(x, 
			score=sc0,
			duration.secs = 'duration..in.seconds.',
			date.start = startdate,
			date.end = enddate,
			location.lat = locationlatitude,
			location.long = locationlongitude,
			# Rename questions:
			age.demographic = q1,
			accept.disclaimer = q16,
			highest.education = q2,
			gender = q3,
			english.native = q4
)


# Set correct data types
x$finished          <- as.logical(x$finished)
x$english.native    <- recode(x$english.native, 'Yes'=TRUE, 'No'=FALSE)
x$accept.disclaimer <- recode(x$accept.disclaimer, 'Yes'=TRUE, 'No'=FALSE, .default=NA)
x <- x %>% mutate(across(c(gender, highest.education, age.demographic), factor))
x$date.start <- as.POSIXct(x$date.start, tz='America/Denver')
x$date.end <- as.POSIXct(x$date.end, tz='America/Denver')

# Attrition?
table(x$finished)

# Encoding treatment as a factor with 4 levels: 'negative', 'positive', 'neutral', 'control'
x <- x %>% unite('treatment', starts_with('fl'), remove = F, sep='') 
x$treatment <- as.factor(x$treatment)

# There are some blank values, TODO: remove them, these are weird failures of the Qualtrics system
levels(x$treatment) <- c('', 'control', 'negative', 'neutral', 'positive')
# Now remove the random flow indicator columns
x <- select(x, -starts_with('fl'))


# Select the columns with the words
select(x, -Q12_DO, -Q25, -Q12)

# Filter out invalid data:
#   * Henceforth use only the completed responses
x <- filter(x, finished == TRUE)


# Get time columns:
x <- rename(x, pretask.time = q14_page.submit, posttask.time = q15_page.submit)
