read_data = function(){
  train_data = read.csv("train_data_two_class.csv", header = TRUE)
  test_data = read.csv("test_data_two_class.csv", header = TRUE)
  
  factors = c("Marital.status", "Daytime.evening.attendance.", "Displaced", "Educational.special.needs",
              "Debtor", "Tuition.fees.up.to.date", "Gender", "Scholarship.holder", "International")
  train_data[factors] = lapply(train_data[factors], factor)  ## as.factor() could also be used
  test_data[factors] = lapply(test_data[factors], factor)  ## as.factor() could also be used
  
  return(list(train_data = train_data, test_data = test_data, factors=factors))
}