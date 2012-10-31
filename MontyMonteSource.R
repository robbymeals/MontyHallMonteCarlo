### Function: Run simulation n times and plot results in stacked bar histograms
montyMonte <- function(n,titleSize=7,legendTitle=5,ytextSize=5,xtextSize=5){
	Picks <- c()
	Opens <- c()
	WinningDoor <- c()
	WinsIfSwitch <- c()
	PlacedDf <- matrix(nrow=n, ncol=3)
	OpensDf <- matrix(nrow=n, ncol=3)
	PicksDf <- matrix(nrow=n, ncol=3)
	Doors <- c('Door 1', 'Door 2', 'Door 3')
	colnames(PlacedDf) <- Doors
	colnames(PicksDf) <- Doors
	colnames(OpensDf) <- Doors
	
	##########   Simulation Loop  #########
	for (i in seq(n)){
		## 1. Randomly place prize behind one of three doors
		PlacePrize <- c(1,0,0)[sample(1:3,3)]
		## 2. Randomly pick one of three doors
		YouPick <- Doors[sample(1:3,1)]
		## 3. Monty either randomly opens one of the two doors left over if
		## you happen to pick the correct door or picks the only door left
		## if you pick one of two incorrect doors
		MontyOpens <- ifelse(PlacePrize[Doors==YouPick]==1,
		Doors[!Doors%in%YouPick][sample(1:2,1)],
		Doors[(!Doors%in%c(YouPick,Doors[PlacePrize==1]))])
		PrizeIsBehind <- Doors[PlacePrize==1]
		## 4. If the prize is behind the leftover door, you win if you switch.
		## Else you win if you stick on your original choice.
		WinIfSwitch <- ifelse(PlacePrize[!Doors%in%c(YouPick,MontyOpens)]==1,1,0)
		Picks <- c(Picks, YouPick)
		Opens <- c(Opens, MontyOpens)
		WinningDoor <- c(WinningDoor, PrizeIsBehind)
		WinsIfSwitch <- c(WinsIfSwitch, WinIfSwitch)
		### Write results to data frames
		PlacedDf[i,] <- PlacePrize
		PicksDf[i,YouPick] <- 2
		OpensDf[i,MontyOpens] <- 3
	}
	########## End Simulation Loop #########
	
	WinsIfSwitches <- ifelse(WinsIfSwitch==1,
	'Switch Door = Win','Switch Door = Lose')
	Games <- data.frame(Picks, Opens, WinningDoor, WinsIfSwitches)
	Wins <- sum(WinsIfSwitch)/n
	Games <- melt(Games,measure.vars=c('Picks', 'Opens', 
	'WinningDoor', 'WinsIfSwitches'))
	Games$variable <- ordered(Games$variable, levels=c('WinsIfSwitches',
	'WinningDoor',
	'Opens','Picks'))
	PicksDf[is.na(PicksDf)] <- 0
	OpensDf[is.na(OpensDf)] <- 0
	ResultsDf <- rbind(
	data.frame('Type'=rep('Placed',n*3),melt(PlacedDf,measure.vars=Doors)),
	data.frame('Type'=rep('Picked',n*3),melt(PicksDf,measure.vars=Doors)),
	data.frame('Type'=rep('Opens',n*3),melt(OpensDf,measure.vars=Doors)))
	colnames(ResultsDf) <- c('Type','Trial','Door','value')
	# Plot stacked bar histograms of your picks, monty's opens, winning doors
	# and win if switch
	ggplot(Games, aes(x=variable, fill=factor(value))) 
	last_plot() + geom_histogram() 
	last_plot() + scale_x_discrete(labels=rev(c('Your Picks',"Monty's Opens",
	'Winning Door','Switch=Win/Lose')))
	last_plot() + scale_fill_brewer(type='qual',palette=6) + xlab('') + ylab('')
	last_plot() + theme_bw() + coord_flip()
	last_plot() + opts(title = 
	paste('Monty Hall Monte Carlo Total Simulation Results, N = ',n,', Pct Switches Win = ',Wins,sep=''),
	legend.position='bottom',legend.title=theme_blank())
	last_plot() + opts(plot.title = theme_text(size=titleSize),
	legend.text = theme_text(size=legendTitle),
	axis.text.y = theme_text(size=ytextSize),
	axis.text.x = theme_text(size=xtextSize))
	ggsave(paste('MontyMonteHistograms',n,'.png',sep=''),width=5, height=3)
	WinsIfSwitches <- factor(WinsIfSwitches)
	ResultsDf$lineTypes <- ordered(rep(WinsIfSwitches,3*3),
	levels=rev(levels(WinsIfSwitches)))
	ResultsDf$Trial <- ordered(ResultsDf$Trial,levels=rev(seq(nrow(ResultsDf))))
	return(ResultsDf)
}

### Function: Tile plot of each result
montyMonteTilePlot <- function(ResultsDf, plotWidth, plotHeight, 
titleSize=7, legendTitle=5, ytextSize=5, xtextSize=5){
n <- nrow(ResultsDf)/9
Wins <- data.frame(table(ResultsDf$lineTypes)/nrow(ResultsDf))[1,2]
fillCols <- brewer.pal(3,'Set1')
ggplot(ResultsDf, aes(x=factor(Door), y=Trial, fill=factor(value))) 
last_plot() + geom_tile(color='black', aes(linetype=lineTypes)) 
last_plot() + facet_grid(. ~Type)
last_plot() + scale_fill_manual(values=c('0'='lightgray','1'=fillCols[1],
'2'=fillCols[2],'3'=fillCols[3]),
labels=c('','Car Placed','You Picked', 'Monty Opened')) 
last_plot() + xlab('') + ylab('') + theme_bw()
last_plot() + opts(title =  paste('Monty Hall Monte Carlo,',
' Each Trial Results, N = ', n,', Pct Switches Win = ', Wins, sep=''),
legend.position='top',legend.title=theme_blank())
last_plot() + opts(plot.title = theme_text(size=titleSize),
legend.text = theme_text(size=legendTitle),
axis.text.y = theme_text(size=ytextSize),
axis.text.x = theme_text(size=xtextSize))
ggsave(paste('MontyMonteTilePlot',n,'.png',sep=''),
width = plotWidth, height=plotHeight)}
