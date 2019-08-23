#* @apiTitle Billionaire Internal Services

#* Backtest the strategies
#* @param Stg detail of Strategies
#* @param StgID ID of Strategy
#* @param UID User ID
#* @param Share Instrument ID
#* @param Timeframe time frame that strategy should be run
#* @param StartDate start date of backtest
#* @param EndDate end date of backtest
#* @param Vol volume of each order
#* @param MaxPos maximum opened position
#* @param ReEnterType Reenter type in the backtest
#* @param ReEnterAmm reenter ammount in the backtest

#* @post /backtest
function(Stg = "bb", StgID = "ppp", UID = "ss", Share = "IRO1APPE0001", Timeframe = "hourly",StartDate = "2014-01-01",EndDate = Sys.Date(),Vol = 1000,MaxPos = 10,Fee = T,Over = T,ReEnterType = 0,ReEnterAmm = 0) {
  res <- Backtest(Stg,UID,Share,Timeframe = Timeframe,StartDate = StartDate,EndDate = EndDate,Vol = Vol,MaxPos = MaxPos,Fee = Fee,Over = Over,ReEnterType = ReEnterType,ReEnterAmm = ReEnterAmm)
  mes <- paste(StgID, UID, Share, Timeframe, res$overal_result$SuccessRate)
  service_log("Backtest", 0, mes)
  res
}


