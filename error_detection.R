################################################################################
# TOPIC:  Error detection in trend data
# AUTHOR: Katherine Hernandez, Ph.D.
# DATE:   02/06/2024
################################################################################

# Package imports
library(dplyr)
library(openxlsx)
library(tidyr)
library(stringr)
library(EnvStats)

# Loading Data
setwd("O:/Katherine on O/Stats Programs/Error Detection in Trend Data")
data <- read.xlsx('ALL_Export_ForDashboard.xlsx')

# Data Cleaning
data <- data %>% filter(Status == 'Operates jail') %>%
  select(County, Qtr.Year, Qtr, Measure, Dimension, Val, N, Agency.Notes) %>%
  mutate_if(is.character, str_trim) %>%                                         # Removes excess whitespace
  mutate_if(is.character, str_to_title) %>%                                     # Standardizes capitalization
  mutate(Qtr.Full = Qtr.Year + (0.25)*(Qtr - 1),
         N = as.numeric(N)) %>%                                                 # Combines Qtr.Year and Qtr into single variable, Makes N a numeric value
  select(County, Qtr.Full, Measure, Dimension, Val, N, Agency.Notes)

# Changing search criteria to Factors
data$County <- as.factor(data$County)
data$Measure <- as.factor(data$Measure)
data$Dimension <- as.factor(data$Dimension)
data$Val <- as.factor(data$Val)

# Sort and save data.frame
data <- data %>% arrange(County, Qtr.Full, Measure, Dimension, Val)
save(data, file='cleaned_data.Rdata')

# Initializing Output file
filename <- 'error_detection_output.txt'
first_line = paste("Error Checking for Jail Data --- ", format(Sys.time(), "%a %b %d %Y at %X"), sep='')
write(first_line, file = filename, append=FALSE)
write('\n', file = filename, append=TRUE)

# Beginning Process - Loading data to be used
load('cleaned_data.Rdata')

# Note: Was having issues in previous script with R choosing to ignore certain Val values seemingly with no root cause --- Figured out that issue; I was being silly.
# The following is designed to circumvent that issue
county_list <- as.character(unique(data$County))
county_list <- county_list[!is.na(county_list)]
measure_list <- as.character(unique(data$Measure))
measure_list <- measure_list[!is.na(measure_list)]
dim_val_list <- list()
for (dim in as.character(unique(data$Dimension))) {
  if (is.na(dim)) {next}

  temp <- data %>% filter(Dimension == dim)
  dim_val_list[[dim]] <- as.character(unique(temp$Val))
}

# Begin by iterating over the counties
for (county in county_list) {

  # Filter data and add line to output file marking our current county
  data_county <- data %>% filter(County == county)
  county_text <- paste('*** Checking for errors in ', county, ' County...', sep='')
  write(county_text, file=filename, sep='', append=TRUE)
  print(county_text)

  # Iterate over the available measures for this county:
  for (measure in measure_list) {

    # Filter data and add line to output file marking our current measure
    data_measure <- data_county %>% filter(Measure == measure)
    measure_text <- paste('  * ', measure)
    measure_error <- FALSE

    # Create empty vectors to append error/warning messages - Will help with organization and cut down on write() calls
    duplicate_text <- c()
    summation_text <- c()
    outlier_text <- c()
    extreme_third_text <- c()
    oops_all_outliers_text <- c()
    single_value_text <- c()
    two_or_three_text <- c()
    intermediate_zero_text <- c()
    missing_text <- c()

    # The COVID-19 Pandemic had a notable impact on underlying distributions of Bookings and Releases data
    if (measure %in% c('Bookings', 'Releases')) {
      data_measure <- data_measure %>% filter(Qtr.Full > 2020.25)
    }

    # Check that summations across each dimension are (close to) correct
    if (measure %in% c('Average Los Misdemeanors', 'Average Los Felonies')) {
      # Do nothing in this case
    } else {
      summ <- data_measure %>% select(Qtr.Full, Dimension, Val, N)
      summ$Qtr.Full.Factor <- as.factor(summ$Qtr.Full)

      # The following accumulates N across all levels of Val (left) and joins to the reported Dimension==Total counts (right)
      x <- summ %>% filter(Dimension != 'Total') %>% group_by(Qtr.Full.Factor, Dimension, Qtr.Full) %>% summarise(N = sum(N), Val = n(), .groups = 'keep')
      y <- summ %>% filter(Dimension == 'Total') %>% mutate(Total = N) %>% select(Qtr.Full.Factor, Total)
      summ <- left_join(x, y, by = 'Qtr.Full.Factor', relationship = 'many-to-many')  # Need this because some duplicate rows when Dimension == Total throws a warning; we catch this later

      # Add column to determine if Dimension sum is close enough to Total
      summ <- summ %>% mutate(Error = ifelse((abs(Total - N) >= Val) * (N > 0), TRUE, FALSE),
                              Year = floor(Qtr.Full),
                              Quarter = paste('Q', (Qtr.Full - Year)*4 + 1, sep='')) %>% filter(Error == TRUE)

      # Write lines to file specifying any present summation errors
      for (dimension in names(dim_val_list)) {
        temp <- summ %>% filter(Dimension == dimension)

        # Do not need to iterate through the rows: paste() can output a character vector if provided vectors in input!
        if (nrow(temp)) {
          measure_error = TRUE
          summation_text <- c(summation_text, paste("  | SUMMATION ERROR: '", dimension, "' at ", temp$Year, "-", temp$Quarter, sep=''))
        }
      }
    } # End of SUMMATION ERROR section

    # Iterate over all dimensions
    for (dimension in names(dim_val_list)) {

      # Filter data, then iterate over all vals, then filter again
      data_dimension <- data_measure %>% filter(Dimension == dimension)

      for (val in dim_val_list[[dimension]]) {
        data_val <- data_dimension %>% filter(Val == val)
        data_subset <- paste(county, measure, dimension, val, sep=' > ')

        # Check for missing data
        if (any(is.na(data_val$N))) {
          measure_error = TRUE
          missing_text <- c(missing_text, paste('  | Warning: Missing data in ', data_subset, sep=''))

          # Remove rows with missing data
          data_val <- data_val %>% filter(!is.na(N))
        }

        # Check for duplicate data
        if (length(unique(data_val$Qtr.Full)) != length(data_val$Qtr.Full)) {
          measure_error = TRUE
          duplicate_text <- c(duplicate_text, paste('  | DUPLICATE ERROR: Multiple reported values for a quarter in ', data_subset, sep=''))
        }

        # Remove leading zeroes
        data_val <- data_val %>% mutate(N.Acc = cumsum(N)) %>% filter(N.Acc > 0) %>% mutate(N.Acc = NULL)
        num_distinct <- length(unique(data_val$N))
        num_nonzero <- sum(data_val$N != 0)

        if (num_nonzero == 1) {
          # We have precisely 1 nonzero value in our vector - FLAG THIS
          measure_error = TRUE
          single_value_text <- c(single_value_text, paste('  | Warning: Only a single nonzero datapoint in ', data_subset, sep=''))
          next
        }
        if (num_distinct %in% c(0,1)) {
          # We have either all 0's (DO NOT FLAG) or a constant vector of length at least 2 (DO NOT FLAG)
          next
        }
        if (num_nonzero < nrow(data_val)) {
          # We have an intermediate 0 - report it, then remove them.
          measure_error = TRUE

          data_val <- data_val %>% filter(N > 0)
          nonzero_range <- range(data_val$N)
          intermediate_zero_text <- c(intermediate_zero_text, paste('  | Warning: Intermediate 0 in ', data_subset,
                                                                    '\n  |          Nonzero range: ', nonzero_range[1], ' - ', nonzero_range[2], '.', sep=''))

          num_distinct <- num_distinct - 1  # Update num_distinct to reflect the loss of zeroes
        }
        if (num_distinct < 4) {
          # We should probably put eyes on any distribution w/ 2 or 3 distinct values
          measure_error = TRUE
          two_or_three_text <- c(two_or_three_text, paste('  | Warning: Only 2 or 3 distinct values in ', data_subset, sep=''))
          next
        }

        # ---------------------------------------------------------------------- At least 4 distinct values, all nonzero
        my_dist = data_val$N
        my_max = max(my_dist)
        my_min = min(my_dist)
        my_extreme = ifelse(my_max + my_min > 2*mean(my_dist), my_max, my_min)  # Midpoint/Mean comparison to determine extreme point
        n_extreme = sum(my_dist == my_extreme)

        if (n_extreme > length(my_dist)/3) {
          # Our extreme point makes up over 33% of the data
          measure_error = TRUE
          extreme_third_text <- c(extreme_third_text, paste('  | Warning: Most extreme value makes up >33% of ', data_subset, sep=''))
        } else {
          third = ceiling(length(my_dist)/3)
          rosner = rosnerTest(my_dist, k=third, alpha=0.05, warn=FALSE)$all.stats %>% filter(Outlier == TRUE)

          if (nrow(rosner) == third) {
            oops_all_outliers_text <- c(oops_all_outliers_text, paste('  | Warning: Outlier test is flagging >33% of ', data_subset, sep=''))
          }

          if (nrow(rosner)) {
            # We found an outlier
            measure_error = TRUE
            outlier_text <- c(outlier_text, paste('  | OUTLIER ERROR: Most extreme (nonzero) value found to be outlier in ', data_subset, sep=''))
          } else {
            # My extreme point wasn't an outlier, so I guess we trust the test and move on.
            # Do nothing
          }
        }

      } # End of VAL loop
    } # End of DIMENSION loop

    # Write measure text to file if present
    if (measure_error) {
      # Get Agency Notes:
      agency_notes_data <- unique((data_measure %>% filter(Agency.Notes != "Agency Notes:"))$Agency.Notes)
      if (length(agency_notes_data)) {
        agency_notes_data <- paste('  | ', agency_notes_data[!is.na(agency_notes_data)], sep='')
        agency_notes_data <- c('  | ', agency_notes_data)
      }

      write(c(measure_text,
              duplicate_text,
              summation_text,
              outlier_text,
              extreme_third_text,
              oops_all_outliers_text,
              single_value_text,
              two_or_three_text,
              intermediate_zero_text,
              missing_text,
              agency_notes_data,
              '  @\n'), file=filename, append=TRUE)
    }
  } # End of MEASURE loop
} # End of COUNTY loop

last_line = paste("Error Checking for Jail Data Completed at ", format(Sys.time(), "%a %b %d %Y at %X"), sep='')
write('', file=filename, sep='', append=TRUE)
write(last_line, file=filename, sep='', append=TRUE)
