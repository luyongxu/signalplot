source("./Posts/1.001 Initial Functions and Libraries.R")

tws <- twsConnect()
tws
reqCurrentTime(tws)
serverVersion(tws)
twsDisconnect(tws)
