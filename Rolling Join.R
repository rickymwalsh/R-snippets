
# Function to conduct rolling joins on dataframes.
# Outputs the joined dataset with data.table type.

#  Inputs: 
#     df1: The master dataset. Each record will be matched to the nearest relevant record in df2.
#     df2: The dataset to be joined with df1.
#     joinKey1: The relevant column name in df1 on which to join the datasets.
#     joinKey2: The relevant column name in df2 on which to join the datasets.
#     rollDistance: The distance to roll by. How far to look for the closest record. If positive, will find the closest 
#                   value of joinKey2 which is less than the joinKey1 value. If negative, then will find joinKey2 s.t. joinKey2 >= joinKey1.
#                   For no limit to rollDistance, specify Inf or -Inf.
#                   To find the nearest match, set rollDistance to "nearest"

#  Example.
#     df1:                                      df2:
#       Key       Characteristic1           Key ID         Characteristic2
#        1              0.045                  2                "A"
#        2              0.1                    5                "B"
#        3              0.01                  -1                "C"
#        4              0.5

# > df1 = data.frame(Key = c(1,2,3,4), Char1 = c(0.1,0.2,0.3,0.4))
# > df2 = data.frame(ID = c(2,5,-1), char2 = c("A","B","C"))

# > rollingJoin(df1, df2, "Key", "Key ID")
#     ID char2 joinKey Key Char1
# 1: -1     C       1   1   0.1
# 2:  2     A       2   2   0.2
# 3:  2     A       3   3   0.3
# 4:  2     A       4   4   0.4

# > rollingJoin(df1, df2, "Key", "Key ID", - Inf)
#     ID char2 joinKey Key Char1
# 1:  2     A       1   1   0.1
# 2:  2     A       2   2   0.2
# 3:  5     B       3   3   0.3
# 4:  5     B       4   4   0.4


rollingJoin <- function(df1, df2, joinKey1, joinKey2, rollDistance = Inf){
  
  if(!require(data.table)) print("Error: Problem in loading package 'data.table'. Please ensure that this package is installed.")
  
  # Convert dataframes to data.table format.
  df1 = as.data.table(df1);  df2 = as.data.table(df2)
  
  # Duplicate the joining variables - this ensures a record is kept of the original variables.
  n = length(joinKey1)
  key_names = paste0('joinKey',1:n)
  
  df1[,(key_names)] = df1[, joinKey1, with = FALSE]
  df2[,(key_names)] = df2[, joinKey2, with = FALSE]

  # Set the keys for the joins.
  setkeyv(df1, key_names)
  setkeyv(df2, key_names)

  # Check if there are duplicates in the joining keys and print a warning if so.
  if(sum(duplicated(df1[,..key_names]) > 0 )) print(paste("Warning: Duplicates found in joinKey for", quote(df1)))
  if(sum(duplicated(df2[,..key_names]) > 0 )) print(paste("Warning: Duplicates found in joinKey for", quote(df2)))  
  
  # Conduct the rolling join.
  combined <- df2[df1, roll = rollDistance]

  return(combined)
}


