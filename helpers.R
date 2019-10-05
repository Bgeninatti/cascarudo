library(purrr)

tryParseNumeric <- function(data) {
  if (class(data) != "numeric") {
    d <- as.numeric(gsub(",", ".", data))
    return(d)
  }
  return(data)
}

loadCsv <- function(path) {
  df <- read.csv(path, dec = ".")
  df$Fecha <- as.Date(df$Fecha, format = "%d-%m-%Y")
  df$Capital <- tryParseNumeric(df$Capital)
  df$Rendimiento <- tryParseNumeric(df$Rendimiento)
  df$Flujo <- tryParseNumeric(df$Flujo)
  df$Saldo <- tryParseNumeric(df$Saldo)
  return(df)
}


PV <- function(start_date, nominal, pay_dates, pay_amounts, rate) {
  if(length(pay_dates) != length(pay_amounts)) {
    print("`pay_dates` and `pay_amounts` must have the same length")
    return()
  }
  time_factors <- unlist(map(pay_dates, function(x){ 1/(1+rate)^(as.numeric(x - start_date)/365)}))
  time_factors[time_factors > 1] <- 0
  pv <- sum(time_factors*pay_amounts)
  return(pv)
}

projectPV <- function(start_date, nominal, dates, capital, interest, rate) {
  input_df <- data.frame(date=dates, capital=capital, interest=interest, balance=capital + interest)
  histDates <- seq(start_date, max(dates), "days")
  histCapital <- unlist(map(histDates, function(x){nominal - sum(input_df[input_df$date < x,]$capital)}))
  present_values <- unlist(map(histDates, function(start_date){PV(start_date, nominal, dates, input_df$balance, rate)}))
  prices <-  100 * present_values / histCapital
  df <- data.frame("Fecha"=histDates,
                   "Capital"=round(x = histCapital,digits = 2),
                   "VP"=round(x = present_values,digits = 2),
                   "Precio"=round(x = prices,digits = 2))
  return(df)
}