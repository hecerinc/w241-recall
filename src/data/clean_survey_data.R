library(tidyverse)

setwd("C:/dev/mids/w241/project/")

# Clear out the rows with metadata:
x <- read.csv('data/raw/raw_qualtrics_data_20220410.csv')
x <- x[-c(1,2),] # remove the first two data lines (they're details of each column)



# Clear out the timing columns for now
x <- select(x, -matches('Click'))


# Remove previews
x <- filter(x, Status != 'Survey Preview')
x <- select(x, -Status, -IPAddress, -RecordedDate, -ResponseId, -starts_with('Recipient'), -ExternalReference, -DistributionChannel, -UserLanguage, -Q_RecaptchaScore, -SC0)


names(x) <- tolower(names(x))
x <- rename(x, 
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

# Q12_DO is an extra column that contains the actual order in which the bank of words was presented to the user, we can safely remove it
x <- select(x, -q12_do)


# Get time columns:
x <- rename(x, pretask.time = q14_page.submit, posttask.time = q15_page.submit)

# Remove rest of the timing columns
x <- select(x, -matches('submit'))

# Set correct data types
x$finished          <- as.logical(x$finished)
x$english.native    <- recode(x$english.native, 'Yes'=TRUE, 'No'=FALSE)
x$accept.disclaimer <- recode(x$accept.disclaimer, 'Yes'=TRUE, 'No'=FALSE, .default=NA)

x <- x %>% mutate(across(c(gender, highest.education, age.demographic), factor))
x <- x %>% mutate(across(c(location.lat, location.long, duration.secs, progress, pretask.time, posttask.time, random.id), as.numeric))

x$date.start <- as.POSIXct(x$date.start, tz='America/Denver')
x$date.end <- as.POSIXct(x$date.end, tz='America/Denver')


# Encoding treatment as a factor with 4 levels: 'negative', 'positive', 'neutral', 'control'
x <- x %>% unite('treatment', starts_with('fl'), remove = F, sep='') 
x$treatment <- as.factor(x$treatment)

# There are some blank values, removed below
levels(x$treatment) <- c('', 'control', 'negative', 'neutral', 'positive')
# Now remove the random flow indicator columns
x <- select(x, -starts_with('fl'))




recall_list_1 <- c('action', 'quote', 'factory', 'curtain', 'content', 'preference', 'attachment', 'colony', 'scrape', 'final', 'ethnic', 'court', 'revolution', 'diameter', 'food', 'suppress', 'gasp', 'sow', 'lobby', 'promise',  'format', 'conference', 'ghost', 'structure', 'by', 'indulge', 'pain', 'acid', 'stitch', 'chimney',  'edge', 'communist', 'partner', 'smash', 'continental', 'therapist', 'yard', 'include', 'abolish', 'complex', 'behavior', 'strap', 'sulphur', 'kid', 'distribute', 'river', 'tread', 'rough', 'mutation', 'lump')
recall_list_2 <- c('shame', 'foundation', 'brake', 'award', 'salon', 'flavor', 'tent', 'peace', 'glide', 'cord', 'mountain', 'recommendation', 'pick', 'judicial', 'belt', 'category', 'favorable', 'bite', 'minimum', 'calendar', 'area', 'convention', 'gear', 'sensitivity', 'inch', 'tolerate', 'admission', 'neglect', 'glacier', 'pour', 'head', 'mercy', 'falsify', 'gate', 'bleed', 'creed', 'monkey', 'force', 'swop', 'range', 'weigh', 'contempt', 'prison', 'execution', 'tile', 'kitchen', 'privilege', 'display', 'particle', 'pound')	

score_answers <- function(answers, recall_list) {
	# Answers is a column of a single string comma-separated
	answer.coll <- unlist(strsplit(answers, split=','))
	return (sum(answer.coll %in% recall_list))
}


# These columns are the number of right answers out of the 50 words from the word bank
x$q12.score <- sapply(x$q12, score_answers, recall_list_1, USE.NAMES = F)
x$q25.score <- sapply(x$q25, score_answers, recall_list_2, USE.NAMES = F)


# Remove the actual answers
x <- select(x, -q12, -q25)



# Filter out invalid data:
# --------------------------------------------------------------------------------------
#   * Remove the rows that did not accept the disclaimer
#   * Remove pilot data (< 2022-03-17 07:30:00)
#   * Remove anything that's less than 5 minutes (300 seconds) long, as this is the length of the treatment
x <- filter(x, accept.disclaimer == T)
x <- filter(x, date.start >= '2022-03-17 07:30:00')
x <- filter(x, duration.secs > 300)

# Recalculate levels for treatment var
x <- droplevels(x)
x$treatment <- relevel(x$treatment, ref='control')

# Attrition? TODO: @lauracheng will do this
# Q12 | Q25 | Finished
# 0 0 0 -> Missing data (because they didn't get any treatment)
# 0 0 1 -> Missing data
# 0 1 0 -> Missing data
# 0 1 1 -> Missing data
# 1 0 0 -> Attrition
# 1 0 1 -> Attrition
# 1 1 0 -> Complete
# 1 1 1 -> Complete



# Save as RData
# save(x, file='data/processed/survey_data_clean_20220410.RData')
