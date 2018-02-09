train_details = read.csv("isl_wise_train_detail_03082015_v1.csv", stringsAsFactors = FALSE)
train_details$station.Code = trimws(train_details$station.Code, which = c("both", "left", "right"))

getStationDetails <- function(x)
{
  ## type of input :: "'00851'"
  trainNumFilter = subset(train_details, Train.No. == x)
  return (data.frame(trainNumFilter$station.Code, trainNumFilter$Station.Name))
}

getTimeSlot <- function(x,y)
{
  ## type of input 
  ##  x :: '00851'
  ##  y :: BBS
  tempfill = filter(train_details,  Train.No. == x , station.Code == y)
  return(data.frame(tempfill$Arrival.time, tempfill$Departure.time))
}

readvalues <- function()
{
  trainNum = readline(prompt = "Enter train number as string: ")
  trainNumsearch = getStationDetails(trainNum)
  print(trainNumsearch)
  
  stationcode = readline(prompt = "Enter station code of your interest from above list as string :")
  trainTimeSlotsearch = getTimeSlot(trainNum, stationcode)
  print(trainTimeSlotsearch)
}