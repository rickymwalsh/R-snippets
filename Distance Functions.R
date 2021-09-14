


# Functions for calculating distance, and the closest instance between sets of lat/longs #

# Functions:
#   degreesToRadians(degrees)
#   findDistance(lat1, long1, lat2, long2, measure = "kilometres")
#   distanceMatrix(dfA, dfB, measure = "kilometres")
#   findNearestInstance(dfA, dfB, ID = "ID", measure = "kilometres")

# degreesToRadians(...) ---------------------------------------------------

# Define function to convert between Degrees and Radians.
degreesToRadians <- function(degrees){
  radians <- degrees*pi/180
  return(radians)
}


# findDistance(...) -------------------------------------------------------


# Function for calculating distance between two lat/longs, with option to specify "kilometres" or "miles".
# Default is "kilometres".
# The function returns the distance.

findDistance = function(lat1, long1, lat2, long2, measure = "kilometres"){
  # In Excel: ACOS(COS(RADIANS(90-Lat1)) * COS(RADIANS(90-Lat2)) + SIN(RADIANS(90-Lat1)) * SIN(RADIANS(90-Lat2)) * COS(RADIANS(Long1-Long2))) * 6371
  
  # Define the radius of the Earth to be used in calculations.
  if(measure %in% c("kms", "km", "kilometres", "kilometre", "kilometers")){
    radius = 6371
  } else if(measure %in% c("miles", "mi", "Miles")){
    radius = 6371/1.609
  } else{
    stop("Please specify measure to be either 'kms' or 'miles' ")
    return(0)
  }
  # Calculate the distance using a Great-Circle Formula which compensates for the curvature of the Earth.
  distance = acos(cos(degreesToRadians(90 - lat1)) * cos(degreesToRadians(90 - lat2)) + sin(degreesToRadians(90 - lat1)) * sin(degreesToRadians(90 - lat2)) *
                    cos(degreesToRadians(long1 - long2))) * radius
  
  return(distance)
}



# distanceMatrix(...) -----------------------------------------------------


# Construct a matrix of distances between all possible combinations of dfA and dfB.
# Each row corresponds to a row in dfA, columns correspond to rows in dfB.

distanceMatrix <- function(dfA, dfB, measure = "kilometres"){
  
  # Check measure is valid.
  if(! measure %in% c("kms", "km", "kilometres", "kilometre", "kilometers", "miles", "mi", "Miles")){
    print("Error: Please specify measure to be either 'kms' or 'miles' ")
    return(0)
  }
  
  # Create a matrix of distances for each combination of lat/longs in the two datasets.
  d = mapply(FUN = function(x, y) mapply(FUN= function(w, z)  return(findDistance(x,y,w,z,measure)), dfA$Latitude, dfA$Longitude), 
               dfB$Latitude, dfB$Longitude)
  
  # Return the matrix of distances.
  return(d)
}


# findNearestInstance(...) ------------------------------------------------


# Function to find both the closest instance of dfB to each instance of dfA.
# Returns a two element list. The first element holds the dfB IDs closest to dfA locations, 
# and the second element holds the distances to those dfB IDs.
# ID should be specified as the name of the column where IDs are held in dfB (e.g. "StoreID")
# If the Distance Matrix has already been calculated, then supply as argument distMatrix. This will reduce run-time of this function.

findNearestInstance <- function(dfA, dfB, ID = "ID",  distMatrix = FALSE, measure = "kilometres"){

  # Check measure is valid.
  if(! measure %in% c("kms", "km", "kilometres", "kilometre", "kilometers", "miles", "mi", "Miles")){
    print("Error: Please specify measure to be either 'kms' or 'miles' ")
    return(0)
  }
  # Check ID is valid
  if(! is.character(ID)){
    print("Error in 3rd argument: Please specify, as a string, the column of dfB where IDs are held.")
    return(0)
  }
  else if(is.null(dfB[1, ID])){
    print("Error in 3rd argument: Please specify the column of dfB where IDs are held.")
    return(0)
  }
  
  # If the distance matrix isn't supplied as an argument then calculate.
  if(! distMatrix)  distMatrix = distanceMatrix(dfA, dfB, measure)
  
  # Find the minimum distance to some instance of dfB for each occurrence of dfA.
  dist = apply(distMatrix, MARGIN = 1, min)
  
  # Find the index of the closest instance of dfB to each occurrence of dfA.
  index = unlist(apply(distMatrix, MARGIN = 1, which.min))
  
  # Find the ID of the closest occurrence of dfB for each row of dfA (or vice-versa).
  closestInstance = dfB[index, ID]
  
  # Output as a list with two elements, the first is a vector of distances, and the second is a vector of ID's.
  output = list(closestInstance, dist)
  
  return(output)
}

















