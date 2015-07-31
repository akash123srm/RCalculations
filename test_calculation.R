
#function to read the csv file
set.seed(100)
data_sample = read.csv(file.choose(),head=TRUE)
attach(data_sample)

set.seed(100)  
calculate_time_validity <- function(){
  time_validity_count_high = 0
  time_validity_count_medium = 0
  time_validity_count_low = 0
  #seq_1 = 1:length(x)
  #vec <- vector()
  for(i in 1:length(Time.Validity))
  {
    if(Time.Validity[i]=="Low")
      time_validity_count_low = time_validity_count_low + 1
    else if (Time.Validity[i]=="Medium")
      time_validity_count_medium = time_validity_count_medium + 1
    else
      time_validity_count_high = time_validity_count_high + 1
  }
  return(c(time_validity_count_low,time_validity_count_medium,time_validity_count_high))
  
}
calculate_time_validity()

