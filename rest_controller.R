#' @post /backtest
function(Stg = "bb", StgID = "ppp", UID = "ss", Share = "IRO1APPE0001", Timeframe = "hourly",StartDate = "2014-01-01",EndDate = Sys.Date(),Vol = 1000,MaxPos = 10,Fee = T,Over = T,ReEnterType = 0,ReEnterAmm = 0) {
  res <- Backtest(Stg,UID,Share,Timeframe = Timeframe,StartDate = StartDate,EndDate = EndDate,Vol = Vol,MaxPos = MaxPos,Fee = Fee,Over = Over,ReEnterType = ReEnterType,ReEnterAmm = ReEnterAmm)
  mes <- paste(StgID, UID, Share, Timeframe, res$overal_result$SuccessRate)
  service_log("Backtest", 0, mes)
  res
}


