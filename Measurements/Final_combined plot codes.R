####Morphological plots squeezed####
#load objects from "Final_BodySize_Selection_NL.R". "Final_TestesVol_Selection_NL.R". "Final_SpermLength_Selection_NL.R"first

####BODY SIZE####
par(mfrow=c(1,2), mar=c(5, 4, 3, 1) + 0.1)
#repeat plotting codes above

## Sex plots with RS & S on same plot
#change F pch to 18 to differentiate btwn M & F
#Plot M only 
plotCI(y=MBSRSmean, x=(1:3), uiw=MBSRSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=15,
       col="#FF6600", ylim=c(0.75,0.95), xlim=c(0.9,3.1), lty=3,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Male Body Size")
#to plot trendline
plotCI(y=MBSRSmean, x=(1:3), uiw=MBSRSSE, err='y', xaxt='n', xlab="", pch=15, col="#FF6600",lty=1,type="b",add=TRUE)

plotCI(y=MBSSmean, x=(1:3), uiw=MBSSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=15,
       col="#3399CC", xlim=c(0.9,3.1), ylim=c(0.75,0.95), lty=3,cex=1.5,add=TRUE)
#to plot trendline
plotCI(y=MBSSmean, x=(1:3), uiw=MBSSSE, err='y', xaxt='n', pch=15, col="#3399CC", lty=1,cex=1.5,type='b',add=TRUE)

legend(
  x=0.9, # x coordinate of the top left of the legend
  y=0.78, # y coordinate of the top left of the legend
  legend=c("Control","Predation"), # sequence of text for the legend
  pch=c(15,15), # sequence of point types for the legend; -1 is a nonexistent point
  col = c("#FF6600","#3399CC"),# sequence of fill colours for the points
  pt.cex=c(1.5,1.5),
  lty=c(1,1)
)


#Plot F only
plotCI(y=FBSRSmean, x=(1:3), uiw=FBSRSSE, err='y', xaxt='n', xlab="Generation", ylab="", pch=18,
       col="#FF6600", xlim=c(0.9,3.1), ylim=c(0.75,0.95), lty=3,cex=2)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Female Body Size")
#to plot trendline
plotCI(y=FBSRSmean, x=(1:3), uiw=FBSRSSE, err='y', xaxt='n', xlab="",  col="#FF6600", pch=18, lty=1,type="b",add=TRUE)

plotCI(y=FBSSmean, x=(1:3), uiw=FBSSSE, err='y', xaxt='n', xlab="Generation", ylab="Head Width (mm)", pch=18,
       col="#3399CC", xlim=c(0.9,3.1), ylim=c(0.75,0.95), lty=3,cex=2, add=TRUE)
#to plot trendline
plotCI(y=FBSSmean, x=(1:3), uiw=FBSSSE, err='y', xaxt='n', xlab="", col="#3399CC", pch=18, lty=1,type="b",add=TRUE)


legend(
  x=0.9, # x coordinate of the top left of the legend
  y=0.78, # y coordinate of the top left of the legend
  legend=c("Control","Predation"), # sequence of text for the legend
  pch=c(15,15), # sequence of point types for the legend; -1 is a nonexistent point
  col = c("#FF6600","#3399CC"),# sequence of fill colours for the points
  pt.cex=c(1.5,1.5),
  lty=c(1,1)
)


####TESTES SIZE####
par(mfrow=c(1,2), mar=c(5, 4.25, 3, 1) + 0.1)


plotCI(y=TVRSmean, x=(1:3), uiw=TVRSSE, err='y', xaxt='n', xlab="Generation", ylab=expression("Volume of Testes " ~ (mm^{3})), pch=15,col="#FF6600",ylim=c(0.003,0.008), xlim=c(0.9,3.1),
       pt.bg="#FF6600", lty=1,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Testes Volume")

#to plot trendline
plotCI(y=TVRSmean, x=(1:3), uiw=TVRSSE, err='y', gap=0,xaxt='n', xlab="", pch=15, pt.bg="#FF6600",  col="#FF6600",lty=1,type="b",add=TRUE)

plotCI(y=TVSmean, x=(1:3), uiw=TVSSE, err='y', gap=0, xaxt='n', xlab="", pch=15,pt.bg="#3399CC",col="#3399CC",
       cex=1.5,add=TRUE,lty=1,type="b")

legend(
  x=1, # x coordinate of the top left of the legend
  y=0.0038, # y coordinate of the top left of the legend
  legend=c("Control","Predation"), # sequence of text for the legend
  pch=c(15,15), # sequence of point types for the legend; -1 is a nonexistent point
  col=c("#FF6600","#3399CC"), # sequence of fill colours for the points
  pt.cex=c(1.5,1.5),
  lty=c(1,1)
)


####SPERM LENGTH####

plotCI(y=SLRSmean, x=(1:3), uiw=SLRSSE, err='y', xaxt='n', xlab="Generation", ylab=expression("Length of Sperm " ~(mu ~ M)), pch=15,col="#FF6600",xlim=c(0.9,3.1),
       ylim=c(240,320), pt.bg="#FF6600", lty=1,cex=1.5)
axis(1, at=1:3, labels=c("G0","G5","G10"), las=1)
title(main="Sperm Length")

#to plot trendline
plotCI(y=SLRSmean, x=(1:3), uiw=SLRSSE, err='y', xaxt='n', xlab="", pch=15,col="#FF6600",  pt.bg="#FF6600", lty=1, type="b",add=TRUE)


plotCI(y=SLSmean, x=(1:3), uiw=SLSSE, err='y', xaxt='n', xlab="", pch=15,pt.bg="#3399CC",col="#3399CC",
       cex=1.5,lty=1,type="b",add=TRUE)

legend(
  x=1, # x coordinate of the top left of the legend
  y=253, # y coordinate of the top left of the legend
  legend=c("Control","Predation"), # sequence of text for the legend
  pch=c(15,15), # sequence of point types for the legend; -1 is a nonexistent point
  col=c("#FF6600","#3399CC"), # sequence of fill colours for the points
  pt.cex=c(1.5,1.5),
  lty=c(1,1)
)

