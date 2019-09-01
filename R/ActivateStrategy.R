ActivateStrategy <- function(Stg, Email,StgID, UID,Share, Timeframe, StartDate = Sys.Date(), EndDate, Vol, MaxPos, ReEnterType = 0,ReEnterAmm = 0, activation = 1) {
  user_strategies <- "/home/rstudio/R/Billionaire-Jobs/data/active_strategies.rda"
  strategies_dir <- "/home/rstudio/R/Billionaire-Jobs/data/strategies.rda"
  time <- Sys.time()
  time <- gsub(" ", "", time)
  time <- gsub(":", "", time)
  time <- gsub("-", "", time)
  if(file.exists(user_strategies)){
    load(user_strategies)
    load(strategies_dir)
    activation_id <- paste0(StgID, Timeframe, Share, time)
    newActStg <- data.frame(User = UID, StgID = StgID, Share = Share, Timeframe = Timeframe, StartDate = StartDate, EndDate = EndDate, Volume = Vol, MaxPosition = MaxPos, ReEnterType = ReEnterType, ReEnterAmm = ReEnterAmm, Status = activation, ActivationID = activation_id)
    actives <- rbind(actives, newActStg)
    actives <- unique(actives)
    save(actives, file = user_strategies)
    newStg <- data.frame(User = UID, StgID = StgID, Stg = as.character(Stg))
    strategies <- rbind(strategies, newStg)
    strategies <- unique(strategies)
    save(strategies, file = strategies_dir)
  } else {
    activation_id <- paste0(StgID, Timeframe, Share, time)
    actives <- data.frame(User = UID, StgID = StgID, Share = Share, Timeframe = Timeframe, StartDate = StartDate, EndDate = EndDate, Volume = Vol, MaxPosition = MaxPos, ReEnterType = ReEnterType, ReEnterAmm = ReEnterAmm, Status = activation, ActivationID = activation_id)
    save(actives, file = user_strategies)
    if(file.exists(strategies_dir)) {
      load(strategies_dir)
      newStg <- data.frame(User = UID, StgID = StgID, Stg = as.character(Stg))
      strategies <- rbind(strategies, newStg)
      strategies <- unique(strategies)
      save(save(actives, file = user_strategies), file = strategies_dir)
    } else {
      strategies <- data.frame(User = UID, StgID = StgID, Stg = as.character(Stg))
      save(strategies, file = strategies_dir)
    }
  }
  list(result = "Strategy is activated")
}
