Backtest <- function(Stg,UID,Share,Timeframe = "hourly",StartDate = "2014-01-01",EndDate = Sys.Date(),Vol = 1000,MaxPos = 10,Fee = T,Over = T,ReEnterType = 0,ReEnterAmm = 0){
  requireNamespace("jsonlite")
  library(zoo)
  library(xts)
  library(TTR)
  library(quantmod)
  library(dplyr)
  library(MASS)
  Report <- function(Result,dd,MaxPos){
    n <- nrow(Result) / 2
    Trade <- rep(c(1:n),each = 2)
    SuccessRate <- nrow(Result[Result[,4] > 0 & !is.na(Result[,4]),]) / n
    MeanProfit <- mean(Result[Result[,4] > 0 & !is.na(Result[,4]),4])
    MeanLost <- mean(Result[Result[,4] <= 0 & !is.na(Result[,4]),4])
    MaxDrwDwn <- min(Result[Result[,4] <= 0 & !is.na(Result[,4]),4])
    TotalRet <- cumprod(1 + Result[!is.na(Result[,4]),4])[n] - 1
    TurnOver <- sum(Result[,1]) * as.numeric(Vol)
    TotalVol <- n * as.numeric(Vol)
    OpenPos <- vector()
    a1 <- c(1:n)
    a2 <- 2*a1
    reE <- Result[a2,]
    reS <- Result[a1,]
    reE[,3] <- as.POSIXct(reE[,3])
    reS[,3] <- as.POSIXct(reS[,3])
    for (i in 1:n) {
      t <- reE[i,3]
      tt <- reS[j,3]
      a <- which((reE[,3] <= tt) & (reE[,3] >= t))
      OpenPos[i] <- length(a)
    }
    MaxOpenPos <- max(OpenPos)
    MaxOpenPos <- min(MaxOpenPos,MaxPos)
    if(MaxOpenPos == 0){
      MaxOpenPos <- 1
    }
    temtar <- vector()
    doreneg <- vector()
    for (i in 1:n) {
      baz <- paste(as.Date(reS[i,3]),as.Date(reE[i,3]),sep = "/")
      doreneg[2*i] <- length(index(dd[baz]))
      temtar <- c(temtar,index(dd[baz]))
    }
    m <- length(unique(temtar))
    l <- nrow(dd)
    AcToDeAc <- m / (l - m)
    radif <- c(1:nrow(Result))
    Nat <- vector()
    Nat[a2] <- (Result[a2,1]-Result[a1,1])*as.numeric(Vol)
    lis <- data.frame(radif,Result[,3],Result[,2],Result[,1],rep(as.numeric(Vol),nrow(Result)),(Result[,1]*as.numeric(Vol)),Nat,doreneg,Trade,Result[,4])
    colnames(lis) <- c("RowNumber","DateTime","Side","Price","OrderVolume","OrderValue","ProfitOrLoss","PreservePeriods","BuyRowNumber","Return")
    Natije <- data.frame(MaxOpenPos,MaxDrwDwn,AcToDeAc,SuccessRate,MeanProfit,TotalRet,TurnOver,MeanLost,TotalVol)
    names(Natije) <- c("MaxOpenPosition","MaxConsecutiveDecline","ActiveToDeactiveDaysRatio","SuccessRate","MeanProfit","TotalReturn","TurnOver","MeanLoss","TotalVolume")
    finalresult <- list(overal_result = Natije, details= lis)
    finalresult
  }
  bb <- instrument_data(ticker = Share, timeframe = Timeframe)
  dd <- instrument_data(ticker = Share, timeframe = "daily")
  # get the Strategy
  x <- as.character(Stg)
  Stg <- jsonlite::fromJSON(x)
  if(Stg$BUY$Status == "Set"){
    EnRuls <- Stg$BUY$Enter$Rules
    EnRels <- Stg$BUY$Enter$Rels
    ExRuls <- Stg$BUY$Exit$Rules
    ExRels <- Stg$BUY$Exit$Rels
    StpLst <- Stg$BUY$Exit$StopLost
    TkPrft <- Stg$BUY$Exit$TakeProfit
    n <- length(EnRuls)
    for (i in 1:n) {
      m <- length(EnRuls[[i]]$Indicator)
      qqq <-"Ind_1"
      for (j in 1:m) {
        Ind <- EnRuls[[i]]$Indicator[[j]]$Indicator
        l <- length(EnRuls[[i]]$Indicator[[j]]$Parameters)
        #TODO fix this
        #indslag <- EnRuls[[i]]$Indicator[[j]]$lag
        indslag <- 0
        qq <- ""
        if(l > 0){
          for (t in 1:l) {
            qq <- paste(qq,EnRuls[[i]]$Indicator[[j]]$Parameters[[t]][1,2],sep = ",")
          }
        }
        k <- which(Indo[,21] == Ind)
        if(indslag > 0){
          b <- paste("Ind_",j," <- Lag(Indis(bb = bb,FUN = Indo[k,1]",qq, ")[,Indo[k,22]],",indslag,")", sep = "")
        } else {
          b <- paste("Ind_",j," <- Indis(bb = bb,FUN = Indo[k,1]",qq, ")[,Indo[k,22]]", sep = "")
        }
        eval(parse(text = b))
      }
      m <- m - 1
      if(EnRuls[[i]]$Math[[m]] == "cross<"){
        for (s in 1:m) {
          qqq <- paste(qqq,EnRuls[[i]]$Math[[s]],"Ind_",s+1,sep = "")
        }
        a1 <- gsub("cross<",">=",qqq)
        b <- paste("c1 <- ",a1,sep = "")
        eval(parse(text = b))
        a2 <- gsub("cross<","<",qqq)
        b <- paste("c2 <- ",a2,sep = "")
        eval(parse(text = b))
        c2 <- lag(c2,1)
        b <- paste("rull_",i," <- (c1 & c2)",sep = "")
        eval(parse(text = b))
      }else if(EnRuls[[i]]$Math[[m]] == "cross>"){
        for (s in 1:m) {
          qqq <- paste(qqq,EnRuls[[i]]$Math[[s]],"Ind_",s+1,sep = "")
        }
        a1 <- gsub("cross>","<=",qqq)
        b <- paste("c1 <- ",a1,sep = "")
        eval(parse(text = b))
        a2 <- gsub("cross>",">",qqq)
        b <- paste("c2 <- ",a2,sep = "")
        eval(parse(text = b))
        c2 <- lag(c2,1)
        b <- paste("rull_",i," <- (c1 & c2)",sep = "")
        eval(parse(text = b))
      }else{
        for (s in 1:m) {
          qqq <- paste(qqq,EnRuls[[i]]$Math[[s]],"Ind_",s+1,sep = "")
        }
        b <- paste("rull_",i," <- (",qqq,")",sep = "")
        eval(parse(text = b))
      }
    }
    q <- "rull_1"
    if(n > 1){
      n <- n - 1
      for (s in 1:n) {
        q <- paste(q,EnRels[[s]],"rull_",s+1,sep = "")
      }
      q <- gsub("OR", " | ", q)
      q <- gsub("ADD", " & ", q)
    }
    b <- paste("BUY_Enter <- (",q,")",sep = "")
    eval(parse(text = b))
    if(length(BUY_Enter) < 1){
      lis <- data.frame(0,0,0,0,0,0,0,0,0,0)
      colnames(lis) <- c("RowNumber","DateTime","Side","Price","OrderVolume","OrderValue","ProfitOrLoss","PreservePeriods","BuyRowNumber","Return")
      Natije <- list(0,0,0,0,0,0,0,0,0,lis)
      names(Natije) <- c("MaxOpenPosition","MaxConsecutiveDecline","ActiveToDeactiveDaysRatio","SuccessRate","MeanProfit","TotalReturn","TurnOver","MeanLoss","TotalVolume","Detail")
      return(Natije)
    }
    BB <- BUY_Enter[which(BUY_Enter),]
    if(length(BB) < 1){
      lis <- data.frame(0,0,0,0,0,0,0,0,0,0)
      colnames(lis) <- c("RowNumber","DateTime","Side","Price","OrderVolume","OrderValue","ProfitOrLoss","PreservePeriods","BuyRowNumber","Return")
      Natije <- list(0,0,0,0,0,0,0,0,0,lis)
      names(Natije) <- c("MaxOpenPosition","MaxConsecutiveDecline","ActiveToDeactiveDaysRatio","SuccessRate","MeanProfit","TotalReturn","TurnOver","MeanLoss","TotalVolume","Detail")
      return(Natije)
    }
    #check overbuy of the signal
    if(Over){
      ta <- as.Date(index(BB))
      n <- length(ta)
      ov <- vector("numeric",n)
      ov[dd[ta,4] / dd[ta,1] < 1.05] <- 1
      BB <- BB[ov == 1,]
    }
    #ReEnter in a position
    if(ReEnterType =="Percentage_Above" | ReEnterType =="Percentage_Below" | ReEnterType =="PriceTick_Above" | ReEnterType =="PriceTick_Below"){
      B <- ReEnt(B=BB,bb=bb,type = ReEnterType,amm = ReEnterAmm)
    }else{
      n <- nrow(BB)
      tar <- index(BB)
      nn <- -c(1:n)
      pp <- as.numeric(bb[tar,4])
      BB <- data.frame(Price = pp,Trade = nn)
      B <- xts(BB,order.by = tar)
    }
    #Evaluate Exit Conditions
    C <- vector()
    nEstp <- vector()
    nEstpP <- vector()
    nEstpt <- vector()
    nEtkp <- vector()
    nEtkpP <- vector()
    nEtkpt <- vector()
    if(nrow(B) > 0){
      if(is.null(ExRuls)) {
        n = 0
      } else {
        n <- length(ExRuls)
      }
      if(n > 0){
        for (i in 1:n) {
          m <- length(ExRuls[[i]]$Indicator)
          qqq <-"Ind_1"
          for (j in 1:m) {
            Ind <- ExRuls[[i]]$Indicator[[j]]$Indicator
            l <- length(ExRuls[[i]]$Indicator[[j]]$Parameters)
            qq <- ""
            for (t in 1:l) {
              qq <- paste(qq,ExRuls[[i]]$Indicator[[j]]$Parameters[[t]][1,2],sep = ",")
            }
            k <- which(Indo[,21] == Ind)
            b <- paste("Ind_",j," <- Indis(bb = bb,FUN = Indo[k,1]",qq, ")[,Indo[k,22]]", sep = "")
            eval(parse(text = b))
          }
          m <- m - 1
          if(ExRuls[[i]]$Math[[m]] == "cross<"){
            for (s in 1:m) {
              qqq <- paste(qqq,ExRuls[[i]]$Math[[s]],"Ind_",s+1,sep = "")
            }
            a1 <- gsub("cross<",">=",qqq)
            b <- paste("c1 <- ",a1,sep = "")
            eval(parse(text = b))
            a2 <- gsub("cross<","<",qqq)
            b <- paste("c2 <- ",a2,sep = "")
            eval(parse(text = b))
            c2 <- lag(c2,1)
            b <- paste("rull_",i," <- (c1 & c2)",sep = "")
            eval(parse(text = b))
          }else if(ExRuls[[i]]$Math[[m]] == "cross>"){
            for (s in 1:m) {
              qqq <- paste(qqq,ExRuls[[i]]$Math[[s]],"Ind_",s+1,sep = "")
            }
            a1 <- gsub("cross>","<=",qqq)
            b <- paste("c1 <- ",a1,sep = "")
            eval(parse(text = b))
            a2 <- gsub("cross>",">",qqq)
            b <- paste("c2 <- ",a2,sep = "")
            eval(parse(text = b))
            c2 <- lag(c2,1)
            b <- paste("rull_",i," <- (c1 & c2)",sep = "")
            eval(parse(text = b))
          }else{
            for (s in 1:m) {
              qqq <- paste(qqq,ExRuls[[i]]$Math[[s]],"Ind_",s+1,sep = "")
            }
            b <- paste("rull_",i," <- (",qqq,")",sep = "")
            eval(parse(text = b))
          }
        }
        q <- "rull_1"
        if(length(ExRuls) > 1){
          n <- n - 1
          for (s in 1:n) {
            q <- paste(q,ExRels[[s]],"rull_",s+1,sep = "")
          }
          q <- gsub("OR", " | ", q)
          q <- gsub("ADD", " & ", q)
        }
        b <- paste("BUY_ExitRu <- (",q,")",sep = "")
        eval(parse(text = b))
        C <- index(BUY_ExitRu[which(BUY_ExitRu),])
      }
      #Take Profit and Stop Lost
      n <- nrow(B)
      for (i in 1:n) {
        tar <- index(B)[i]
        pri <- B[i,1]
        if(is.null(StpLst)){
          Stp <- as.numeric(pri * 0)
        }else {
          if(StpLst[1,1] == "Percent"){
            Stp <- as.numeric(floor(pri * ((100 - as.numeric(StpLst[1,2]))/100)))
          }else if(StpLst[1,1] == "PriceTick"){
            Stp <- as.numeric(pri - as.numeric(StpLst[1,2]))
          } else {
            Stp <- 0
          }
        }
        if(is.null(TkPrft)){
          Prf <- as.numeric(pri * 1000)
        }else {
          if(TkPrft[1,1] == "Percent"){
            Prf <- as.numeric(floor(pri * ((100 + as.numeric(TkPrft[1,2]))/100)))
          }else if(TkPrft[1,1] == "PriceTick"){
            Prf <- as.numeric(pri + as.numeric(TkPrft[1,2]))
          } else {
            Prf <- as.numeric(pri * 1000)
          }
        }
        baz <- paste(tar,EndDate,sep = "/")
        temp <- bb[baz]
        m <- index(temp)[which(temp[,3] < Stp)[1]]
        if(is.na(m)){
          m <- index(tail(temp,1))
        }
        l <- index(temp)[which(temp[,2] > Prf)[1]]
        if(is.na(l)){
          l <- index(tail(temp,1))
        }
        nEstp[i] <- as.character(m)
        nEstpP[i] <- as.numeric(temp[m,4])
        nEstpt[i] <- i
        nEtkp[i] <- as.character(l)
        nEtkpP[i] <- as.numeric(temp[l,4])
        nEtkpt[i] <- i
      }
      C <- as.character(C)
      Etar <- c(C,nEstp,nEtkp)
      etemp <- rep(0,length(C))
      Etra <- c(etemp,nEstpt,nEtkpt)
      Epri <- vector()
      m <- length(Etar)
      for (i in 1:m) {
        Epri[i] <- as.numeric(bb[Etar[i],4])
      }
      val <- rep(1,m)
      forush <- xts(data.frame(Price = Epri,Trade = Etra, valid = val),order.by = as.POSIXlt(Etar))
      n <- nrow(B)
      S <- as.data.frame(B[1,])
      STar <- vector()
      for (i in 1:n) {
        s <- index(B)[i]
        f1 <- as.vector(forush[,2] == 0)
        f2 <- as.vector(forush[,2] == i)
        f3 <- index(forush) > s
        f4 <- f1+f2 >= 1
        f5 <- as.vector(forush[,3] > 0)
        ff <- which((f3 * f4 * f5) > 0)[1]
        forush[ff,3] <- 0
        S[i,] <- data.frame(as.numeric(forush[ff,1]),i)
        STar[i] <- as.character(index(forush[ff,]))
      }
      S <- xts(S,order.by = as.POSIXct(STar))
      val <- MaxPosition(B = B,S = S,MaxPos = as.numeric(MaxPos))
      BBB <- B[val > 0,]
      SSS <- S[val[S[,2]] > 0,]
      n <- nrow(BBB)
      Tar <- vector()
      Transaction <- as.data.frame(B[1,])
      TT <- rep(c("B","S"),n)
      Ret <- vector()
      for (i in 1:n) {
        ii <- (2 * i) - 1
        jj <- 2 * i
        Tar[jj] <- as.character(index(SSS[i,]))
        Tar[ii] <- as.character(index(BBB[i,]))
        Transaction[ii,] <- B[i,]
        Transaction[jj,] <- S[i,]
        Ret[[ii]] <- NA
        Ret[jj] <- (as.numeric(S[i,1]) / as.numeric(B[i,1])) - 1
      }
      Result <- data.frame(Transaction,TT,Tar,Ret)
      Result <- Result[,-2]
      rownames(Result) <- NULL
      repo <- Report(Result = Result,dd = dd,MaxPos = MaxPos)
    }
  }
  repo
}
