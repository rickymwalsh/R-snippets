
# Function to do various date conversions using a specific corporate fiscal calendar.

# To call this function from another script, first source the script: source('~/GitHub/R-Snippets/convert_date.R')  and then you will be able to call the function.

################# Examples of usage & results ###################################################
# > convert_date(input = '2015-04-15', from = 'date', to = 'week ending')
# [1] "2015-04-18"
#
# > convert_date(input = c('01.2018', '02.2018', '03.2018'), from = 'fiscal week', to = 'week ending')
# [1] "2018-01-06" "2018-01-13" "2018-01-20"
################################################################################################

convert_date <- function(input, from, to){
  ####### Possible conversions ##############
  #   from             to
  # 'date'  ->   'fiscal week'
  # 'date'  ->   'week ending'
  # 'date'  ->   'week beginning'
  # 'date'  ->   'fiscal month'
  # 'date'  ->   'fiscal year'
  # 'fiscal week' ->   'week ending'
  # 'fiscal week' ->   'fiscal month'
  # 'fiscal week' ->   'week beginning'
  ###########################################
  
  ######## input format ##################################################################################################
  # if from is 'date' then input should be a date or string in the format 'YYYY-MM-DD'
  # if from is 'fiscal week' then input should be a string in the format 'WW.YYYY', e.g. week 1 in 2018 would be '01.2018'
  ########################################################################################################################
  
  ################## Extending the date scope of the function ##########################################################################
  # To extend the date range of this function, the following variables must be updated: min_date, max_date, offset, years_with_53_weeks.
  ######################################################################################################################################
  
  library(lubridate)  # used for the week(), year(), floor_date(), and ceiling_date() functions.
  library(dplyr)  # used for the case_when function (for readability)
  
  # convert the function arguments to lowercase.
  from <- tolower(from); to <- tolower(to)
  
  # Check arguments.
  stopifnot(from %in% c('date', 'fiscal week'), to %in% c('week beginning', 'week ending', 'fiscal week', 'fiscal month', 'fiscal year'))
  
  # Parse input and handle errors.
  if(from == 'date'){
    input <- tryCatch(as.Date(input), error = function(e) {
      e$message <- paste0(e$message, "\n input should be in the format 'YYYY-MM-DD' when from is set to 'date'")
      stop(e)
      })
    # Extract the calendar year of the input date, which will be used in various conditions & calculations.
    input_year <- year(input)
  } else if(from =='fiscal week'){
    input_week <- tryCatch(as.integer(substr(input, 1, 2)), error = function(e) {
      e$message <- paste0(e$message, "\n input should be in the format 'WW.YYYY' when from is set to 'fiscal week'")
      stop(e)
    })
    input_year <- tryCatch(as.integer(substr(input, 4, 7)), error = function(e) {
      e$message <- paste0(e$message, "\n input should be in the format 'WW.YYYY' when from is set to 'fiscal week'")
      stop(e)
    })
  }

  
  # Check date is within the range of dates handled by the function.
  min_date <- '2014-01-01'
  max_date <- '2021-12-31'
  
  if(from == 'date'){
    if(any(input < min_date) | any(input > max_date))
      stop(paste0("At least one date is outside the range of the function (", min_date, "to ", max_date, ")."))
    else if(from == 'fiscal week') {
      if(any(input_year < year(min_date)) | any(input_year > year(max_date)))
        stop(paste0("At least one date is outside the range of the function (", min_date, "to ", max_date, ")."))
    }
  }
  
  # The offset is the number which we would add to the first day of the fiscal year in order to convert it to the first day of the calendar year.
  # E.g. the 2019 fiscal year started on 2018-12-30. To move from this date to the first day of the calendar year (2019-01-01) we need to add 2.
  offset <- case_when(
    input_year == 2014 ~ -4,
    input_year == 2015 ~ -3,
    input_year == 2016 ~ -2,
    input_year == 2017 ~ 0,
    input_year == 2018 ~ 1,
    input_year == 2019 ~ 2,
    input_year == 2020 ~ -4,
    input_year == 2021 ~ -2,
    TRUE ~ NA_real_
  )
  
  # List of years with 53 weeks.
  years_with_53_weeks <- c(2013, 2019)
    
# Do conversions from 'date' input. ---------------------------------------
  
  if(from == 'date'){
    if(to == 'week beginning') 
      return(floor_date(input, unit = 'week', week_start = 7))
    else if(to == 'week ending')
      # ceiling_date returns the date of the next Sunday, so we subtract one day to get the week ending date (i.e. the next Saturday)
      return(ceiling_date(input, unit = 'week', week_start = 7) - 1) 
    else{
      # Get the fiscal week and fiscal year. lubridate::week() returns the number of weeks since January 1st of that year.
      fiscal_week <- week(input + offset)
      fiscal_year <- year(input + offset)
      
      # Handle edge cases at the start & end of the year
      tmp_week <- case_when(
        # If the converted date lies in the previous year and that year is a 53 week year then it should be week 53.
        fiscal_year < input_year & fiscal_year %in% years_with_53_weeks ~ 53,
        # Otherwise, if the converted date lies in the previous year then it should be week 52.
        fiscal_year < input_year ~ 52,
        # If fiscal_week is 53 and the year is not a 53 week year, push it into week 1 of the following year.
        fiscal_week == 53 & !input_year %in% years_with_53_weeks ~ 1,
        # If input+offset has ended up in the following year, and the current year is a 53 week year then bring it back to week 53.
        fiscal_year > input_year & input_year %in% years_with_53_weeks ~ 53,
        TRUE ~ fiscal_week
      )
      
      fiscal_year <- case_when(
        # If input+offset has ended up in the following year, and the current year is a 53 week year then bring it back to week 53.
        fiscal_year > input_year & input_year %in% years_with_53_weeks ~ input_year, 
        # If input+offset has ended up in the previous year, leave as is. This conditions is present to avoid overlaps with the subsequent condition.
        fiscal_year < input_year ~ fiscal_year,
        # If fiscal_week is 53 and the year is not a 53 week year, push it into week 1 of the following year.
        fiscal_week == 53 & !input_year %in% years_with_53_weeks ~ fiscal_year + 1,
        TRUE ~ fiscal_year)
      
      # Assign the newly calculated fiscal week. The original fiscal_week is used in the fiscal_year conditions above so assignment must occur here.
      fiscal_week <- tmp_week
      
      if(to == 'fiscal year')
        return(fiscal_year)
      
      if(to == 'fiscal week'){
        # Add a leading zero for weeks 1-9
        fiscal_week_string <- ifelse(fiscal_week < 10, paste0('0', fiscal_week), as.character(fiscal_week))
        fiscal_week_string <- paste(fiscal_week_string, fiscal_year, sep = ".")
        return(fiscal_week_string)
      }
      
      # Check if the year is a 53 week year.
      is_53_week_year <- ifelse(fiscal_year %in% years_with_53_weeks, 1, 0)

      # Extract the fiscal month from fiscal week based on a 4-4-5 recurring pattern. If the year is a 53 week year then there are 5 weeks in August.
      fiscal_month_number <- case_when(
        fiscal_week <= 4 ~ '01',
        fiscal_week <= 8 ~ '02',
        fiscal_week <= 13 ~ '03',
        fiscal_week <= 17 ~ '04',
        fiscal_week <= 21 ~ '05',
        fiscal_week <= 26 ~ '06',
        fiscal_week <= 30 ~ '07',
        fiscal_week <= 34 + is_53_week_year ~ '08',
        fiscal_week <= 39 + is_53_week_year ~ '09',
        fiscal_week <= 43 + is_53_week_year ~ '10',
        fiscal_week <= 47 + is_53_week_year ~ '11',
        fiscal_week <= 52 + is_53_week_year ~ '12',
        TRUE ~ NA_character_
      )
        
      # Put together the full fiscal month.
      fiscal_month <- as.Date(paste(fiscal_year, fiscal_month_number, "01", sep = "-"))
      
      if(to == 'fiscal month')
        return(fiscal_month)
    }
  }
    
# Do conversions from 'fiscal week' input. ----------------------------------------------------------------------
  
  if(from == 'fiscal week'){
    # Get the first day of the fiscal year based on the offset.
    start_date <- as.Date(paste0(input_year, "-01-01")) - offset

    if(to == 'week ending'){
      # Calculate the date by starting at the first day of the fiscal year and incrementing by the number of weeks.
      # We subtract 1 day as we want to find the last day of the week. 
      week_ending <- start_date + input_week*7 - 1
      return(week_ending)
    }
    
    if(to == 'week beginning'){
      # Calculate the date by incrementing by the number of weeks. Week 1 will be the first day of the fiscal year.
      week_beginning <- start_date + (input_week-1)*7
      return(week_beginning)
    }
    
    if(to == 'fiscal month'){
      # Check if the year is a 53 week year.
      is_53_week_year <- ifelse(input_year %in% years_with_53_weeks, 1, 0)
      
      # Extract the fiscal month number from fiscal week based on a 4-4-5 recurring pattern.
      fiscal_month_number <- case_when(
        input_week <= 4 ~ '01',
        input_week <= 8 ~ '02',
        input_week <= 13 ~ '03',
        input_week <= 17 ~ '04',
        input_week <= 21 ~ '05',
        input_week <= 26 ~ '06',
        input_week <= 30 ~ '07',
        input_week <= 34 + is_53_week_year ~ '08',
        input_week <= 39 + is_53_week_year ~ '09',
        input_week <= 43 + is_53_week_year ~ '10',
        input_week <= 47 + is_53_week_year ~ '11',
        input_week <= 52 + is_53_week_year ~ '12',
        TRUE ~ NA_character_
      )
      
      # Put together the full fiscal month.
      fiscal_month <- as.Date(paste(input_year, fiscal_month_number, "01", sep = "-"))

      return(fiscal_month)
    }
  }
}
