require(lattice)
require(ggplot2)
# analyzing trajectories of sequential action learning experiments
# (motion-SRT paradigm aka statistical movement learning)
# George Kachergis 12.23.2013

# first run: python parase_data_exp2.py
# exp2 = read.table("exp2_saffran_traj_data.txt", header=T)
# save(exp2, file="exp2_saffran_traj_data.RData")
# test = read.table("exp2_saffran_traj_test_data.txt", header=T) # 28 / 26 columns
# save(test, file="exp2_saffran_traj_test.RData")
load("exp2_saffran_traj_data.RData") # exp2 - 45 Ss, 3 between-S conditions

# Loc doesn't change until it reaches the next stimulus
# Correct is only 1 at the first time S touches the target, then 0 until S leaves that location (then -1)


add_movement_time <- function(d) {
	load("exp2_all_hits.RData") # these have PositionInWord, StartNewWord, CurrentWord
	#obj_gets_touched = which(diff(d$objTouched)!=0) # 18256
	# median(diff(obj_gets_touched)) = 64 points between *16ms = 1024ms
	median(diff(which(d$Correct==1))) # 56 points between *16ms = 896ms
	hiti = which(d$Correct==1) # subject arrived at next target; next 500ms of movement are predictive
	d$predictive = FALSE
	d$moveTimeInd = NA
	d$PositionInWord = NA
	d$StartNewWord = NA
	d$CurrentWord = NA
	hitsInd = as.integer(row.names(hits))
	notFound = 0
	for(i in 1:(length(hiti)-1)) {
		if((hiti[i]+31) < nrow(d)) {
			d[hiti[i]:(hiti[i]+31),]$predictive = TRUE
		}
		d[hiti[i]:(hiti[i+1]-1),]$moveTimeInd = 1:length(hiti[i]:(hiti[i+1]-1))
		
		# there are far more hiti than hits, for some reason (took the first one only?), so when we have a 
		# hiti, we need to look for that point in the hits (and fail gracefully)
		found = which(hitsInd==hiti[i])
		if(length(found)!=0) {
			d[hiti[i]:(hiti[i+1]-1),]$PositionInWord = hits[found,]$PositionInWord
			d[hiti[i]:(hiti[i+1]-1),]$StartNewWord = hits[found,]$StartNewWord
			d[hiti[i]:(hiti[i+1]-1),]$CurrentWord = hits[found,]$CurrentWord
		} else {
			notFound = notFound +1
		}
		
		if(i%%100 == 0) {
			print(i)
		}
	}
	print(paste("Appropriate entry in hits not found",notFound,"times"))
	return(d)
}


add_move_position_index_and_dist <- function() {
	# problem is Correct doesn't always just turn 1 for a single time point
	# when it stays on for many points, we want those points to be part of the NEXT move,
	# not the current one
	
	load("exp2_with_moveInd.RData") #exp2
	exp2$trial = exp2$trial + 1
	Ntr = length(unique(exp2$trial)) # 80
	exp2$half = ifelse(exp2$trial < (Ntr/2), "First", "Second")
	exp2$Block <- cut(exp2$trial, 4,labels=F) 
	print(table(exp2$predictive)) # 1.47 million false, 1.4 million true (2880058 total)
	exp2$Dist = c(NA, with(exp2, sqrt(diff(xPos)^2 + diff(yPos)^2)))
	exp2$Condition = with(exp2, ifelse(cond=="NoQue", "No Cue", ifelse(cond=="AlignedQue", "Aligned Cue", "Misaligned Cue")))
	exp2$moveInd = NA
	exp2$duplicateHit = 0
	for(s in unique(exp2$subject)) {
		for(t in unique(exp2$trial)) {
			tind = with(exp2, which(subject==s & trial==t))
			# Correct==1 might be better (curTarget changes after a delay..)
			changes = which(exp2[tind,]$Correct==1)
			
			cutpoints = which(diff(exp2[tind,]$moveTimeInd)<0)
			duplicatehits = which(diff(exp2[tind,]$moveTimeInd)==0)
			if(length(duplicatehits)>0) {
				exp2[tind[duplicatehits],]$duplicateHit = 1
			}
			cat("subj",s, "trial", t, "hits:",length(changes), "\n")
			for(i in 1:length(cutpoints)) {
				if(i==1) {
					exp2[tind[1]:tind[cutpoints[1]],]$moveInd = i
				} else {
					exp2[tind[(cutpoints[i-1]+1)]:tind[cutpoints[i]],]$moveInd = i
				}
			}
		}
	}
	exp2$cond = NULL
	exp2$Position = exp2$moveInd %% 4
	exp2$Position = with(exp2, ifelse(Position==0, 4, Position))
	save(exp2, file="exp2_with_moveIndPos.RData")
	return(exp2)
}

dd = add_move_position_index_and_dist()

predictive_graph <- function() {
	# there should be 45*80*12 = 43200 hits
	
	# let's try to graph just predictive movements (before next stimuli comes up)
	#exp2 <- add_movement_time(exp2) # adds moveTimeInd, an index for every 16ms of a movement (from
	#   one hit to the next), and 'predictive' which flags the first 500ms of each movement
	
  	# we might ideally graph the total distance travelled (average per subj) before vs after the 
  	# target pops up (predictive/postdictive), by block: do subjects move more predictively over time?
  	load("exp2_with_moveIndPos.RData")
	dd = exp2
	# nrow(d) = 2880058  nrow(na.omit(d)) = 2797912  (moveTimeInd)
	#dd = d[which(diff(d$trialTime)>0 & diff(d$trialTime)<20),] # 1147636
	dd = dd[which(!is.na(dd$Dist)),] # elims 1
	mean(dd$Dist) # med 12.15
	
	# trim super-slow movements...
	table(dd$moveTimeInd)
	dd = subset(dd, moveTimeInd<265) # only one move up to 364 point...2 up to 304
	
	require(lme4)
	#print(lmer("Dist ~ Condition*half*predictive + (1|subject)", data=dd))
	#print(lmer("Dist ~ Condition*half*predictive + (1+cond|subject)", data=dd))
	summary(lm("Dist ~ Condition*half*predictive", data=dd))
	summary(lm("Dist ~ Condition*half*predictive", data=subset(dd, Dist>0)))
	
	sd(dd$Dist) # sd = 14.04
	#thresh = median(dd$Dist) + 2*sd(dd$Dist) # 31.11 # kept 2673146 (2673146/2880057 = 92.8% of data)
	thresh = median(dd$Dist) + 3*sd(dd$Dist) # 43.14
	dd = subset(dd, Dist<thresh) # 2822495/ = 98.0% of data
	
	# using the new Position column
	by_pos = with(dd, aggregate(Dist, list(Block=Block, Condition=Condition, subject=subject, trial=trial, Position=Position, predictive=predictive), sum))
	by_pos$x = by_pos$x / 3 # 3 words per trial
	dist_s = with(by_pos, aggregate(x, list(Block=Block, Condition=Condition, Position=Position, subject=subject, predictive=predictive), mean))
	totdist = with(dist_s, aggregate(x, list(Block=Block, Condition=Condition, Position=Position, predictive=predictive), mean))
	totdist$sd = with(dist_s, aggregate(x, list(Block=Block, Condition=Condition, predictive=predictive), sd))$x
	totdist$SE = totdist$sd / sqrt(15-1)
	dodge <- position_dodge(width=.3)
	totdist$Pred = ifelse(totdist$predictive==TRUE, "Pre-stimulus", "Post-stimulus")
	preddist = subset(totdist, predictive==TRUE)
	limits <- with(preddist, aes(ymax=x+SE, ymin=x-SE))
	a = qplot(as.factor(Position), x, data=preddist, colour=Condition, position=dodge) + labs(x="Blocks of 20 Training Trials", y="Distance Traveled (pixels)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) + theme_bw() + facet_grid(. ~ Block)
	
	# increase in dist traveled during predictive portion of trial (block 4-block 1)
	b1 = subset(dist_s, Block==1)
	b4 = subset(dist_s, Block==4)
	b4$Diff = b4$x - b1$x
	totdist = with(b4, aggregate(Diff, list(Block=Block, Condition=Condition, Position=Position, predictive=predictive), mean))
	totdist$sd = with(b4, aggregate(Diff, list(Block=Block, Condition=Condition, predictive=predictive), sd))$x
	totdist$SE = totdist$sd / sqrt(15-1)
	dodge <- position_dodge(width=.3)
	totdist$Pred = ifelse(totdist$predictive==TRUE, "Pre-stimulus", "Post-stimulus")
	limits <- with(totdist, aes(ymax=x+SE, ymin=x-SE))
	a = qplot(as.factor(Position), x, data=totdist, colour=Condition, position=dodge) + labs(x="Position in Word", y="Mean Change in Distance Traveled (Block 4 - Block 1)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) + theme_bw() + facet_grid(. ~ Pred)
	print(a)
	ggsave("exp2_change_in_distance_by_position_b1_to_b4.pdf", width=5, height=4.3)
	dev.off()
	# ^ this one went in the ICDL paper (Fig 8) ^
	
	by_pos = with(dd, aggregate(Dist, list(half=half, Condition=Condition, subject=subject, trial=trial, Position=Position, predictive=predictive), sum))
	by_pos$x = by_pos$x / 3 # 3 words per trial
	dist_s = with(by_pos, aggregate(x, list(half=half, Condition=Condition, Position=Position, subject=subject, predictive=predictive), mean))
	totdist = with(dist_s, aggregate(x, list(half=half, Condition=Condition, Position=Position, predictive=predictive), mean))
	totdist$sd = with(dist_s, aggregate(x, list(half=half, Condition=Condition, predictive=predictive), sd))$x
	totdist$SE = totdist$sd / sqrt(15-1)
	dodge <- position_dodge(width=.3)
	totdist$Pred = ifelse(totdist$predictive==TRUE, "Pre-stimulus", "Post-stimulus")
	preddist = subset(totdist, predictive==TRUE)
	limits <- with(preddist, aes(ymax=x+SE, ymin=x-SE))
	a = qplot(as.factor(Position), x, data=preddist, colour=Condition, position=dodge) + labs(x="Blocks of 20 Training Trials", y="Distance Traveled (pixels)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) + theme_bw() + facet_grid(. ~ half)
	
	
	
	a = qplot(as.factor(Block), x, data=totdist, colour=Condition, position=dodge) + labs(x="Blocks of 20 Training Trials", y="Distance Traveled (pixels)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) + theme_bw() + facet_grid(. ~ Pred) # + ylim(0,.82)
	print(a)
	ggsave("exp2_before_after_stimulus_distance_by_position_n_block.pdf", width=4.5, height=4)
	dev.off()
	
	
	by_tr = with(dd, aggregate(Dist, list(Block=Block, Condition=Condition, subject=subject, trial=trial, predictive=predictive), sum)) # can divide by 12 for per-movement averages (each trial was 12 movements)
	by_tr$DistPerMove = by_tr$x / 12
	totdist_s = with(by_tr, aggregate(DistPerMove, list(Block=Block, Condition=Condition, subject=subject, predictive=predictive), mean))
	
	qplot(as.factor(Block), x, data=totdist_s, colour=Condition, group=subject, geom='line') + labs(x="Blocks of 20 Training Trials", y="Distance Traveled (pixels)", fill="Condition") + theme_bw() +  stat_smooth(aes(group = Condition, colour=Condition)) + facet_grid(. ~ predictive)
	
	
	totdist = with(totdist_s, aggregate(x, list(Block=Block, Condition=Condition, predictive=predictive), mean))
	totdist$sd = with(totdist_s, aggregate(x, list(Block=Block, Condition=Condition, predictive=predictive), sd))$x
	totdist$SE = totdist$sd / sqrt(15-1)
	dodge <- position_dodge(width=.3)
	totdist$Pred = ifelse(totdist$predictive==TRUE, "Pre-stimulus", "Post-stimulus")
	limits <- with(totdist, aes(ymax=x+SE, ymin=x-SE))
	a = qplot(as.factor(Block), x, data=totdist, colour=Condition, position=dodge) + labs(x="Blocks of 20 Training Trials", y="Distance Traveled (pixels)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) + theme_bw() + facet_grid(. ~ Pred) # + ylim(0,.82)
	print(a)
	ggsave("exp2_before_after_stimulus_distance_by_block.pdf", width=4.5, height=4)
	dev.off()
	
	
	# hist(dd$Dist) shows there are a lot (30%?) of points with no movement; maybe analyze these separately?
	nomov = subset(dd, Dist==0) # 655014 points
	table(nomov$Condition, nomov$half) # aligned cue don't show much of a decrease (unlike other two conds)
	movs = subset(dd, Dist>0) # 2018132
	dist_s <- with(movs, aggregate(Dist, list(Half=half, Condition=Condition, subject=subject), mean))
	dist = with(dist_s, aggregate(x, list(Half=Half, Condition=Condition), mean))
	dist$sd = with(dist_s, aggregate(x, list(Half=Half, Condition=Condition), sd))$x
	dist$SE = dist$sd / sqrt(15-1)
	
	# including 0-movement points
	dist_s <- with(dd, aggregate(Dist, list(Half=half, Condition=cond, subject=subject), mean))
	dist = with(dist_s, aggregate(x, list(Half=Half, Condition=Condition), mean))
	dist$sd = with(dist_s, aggregate(x, list(Half=Half, Condition=Condition), sd))$x
	dist$SE = dist$sd / sqrt(15-1)
	
	pt = table(d$predictive, d$half) # less predictive in the first half than the second half...but is it correct?
	table(d$predictive, d$half, d$cond)
	pred_s <- with(d, aggregate(predictive, list(Half=half, Condition=Condition, subject=subject), mean))
	pred = with(pred_s, aggregate(x, list(Half=Half, Condition=Condition), mean))
	pred$sd = with(pred_s, aggregate(x, list(Half=Half, Condition=Condition), sd))$x
	pred$SE = pred$sd / sqrt(15-1)
	dodge <- position_dodge(width=.2)
	limits <- with(pred, aes(ymax=x+SE, ymin=x-SE))
	# ggplot has useful functions: cut_interval = equal-length intervals, cut_number = equal number of points
	a = qplot(as.factor(Half), x, data=pred, colour=Condition, position=dodge) + labs(x="Training Half", y="Proportion of Predictive Movements", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) + theme_bw() # + ylim(0,.82)
	print(a)
	ggsave("exp2_predictive_moves_vs_half.pdf", width=4, height=4)
	dev.off()
	
	pred_s <- with(dd, aggregate(predictive, list(Block=Block, Condition=Condition, subject=subject), mean))
	summary(aov(x ~ (Block * Condition) + Error(subject/(Block*Condition)), data = pred_s))
	pred = with(pred_s, aggregate(x, list(Block=Block, Condition=Condition), mean))
	pred$sd = with(pred_s, aggregate(x, list(Block=Block, Condition=Condition), sd))$x
	pred$SE = pred$sd / sqrt(15-1)
	dodge <- position_dodge(width=.3)
	limits <- with(pred, aes(ymax=x+SE, ymin=x-SE))
	# ggplot has useful functions: cut_interval = equal-length intervals, cut_number = equal number of points
	a = qplot(as.factor(Block), x, data=pred, colour=Condition, position=dodge) + labs(x="Blocks of 20 Training Trials", y="Proportion of Predictive Movements", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) + theme_bw() # + ylim(0,.82)
	print(a)
	ggsave("exp2_predictive_moves_vs_block.pdf", width=4, height=4)
	dev.off()
  
  
	gd = subset(exp2, (xPos<850 & xPos>170) & (yPos>30 & yPos<760)) # 2763815 not outside a big box
  # get the ones in the response boxes
	box = subset(gd, (xPos >= 217 & xPos <= 297 & yPos >= 89 & yPos <= 169) | (xPos >= 217 & xPos <= 297 & yPos >= 609 & yPos <= 689) | (xPos >= 737 & xPos <= 817 & yPos >= 609 & yPos <= 689) | (xPos >= 737 & xPos <= 817 & yPos >= 89 & yPos <= 169)) # 1769489
	# outside the response boxes
  gd = subset(gd, !(xPos > 217 & xPos < 297 & yPos > 89 & yPos < 169) & !(xPos > 217 & xPos < 297 & yPos > 609 & yPos < 689) & !(xPos > 737 & xPos < 817 & yPos > 609 & yPos < 689)  & !(xPos > 737 & xPos < 817 & yPos > 89 & yPos < 169)) # 1769489
	egd = subset(gd, moveTimeInd<=31) # first 31 points of "Time" -- each point is 16ms (60fps), 500/16 = 31.25
	# only 550425 points!
	
	# should do a chi-sq test on the number of points within- vs. outside the squares during the predictive
	# part of the trial (pre-stimulus), by condition and by training half (and Subj); it looks like there's 
	# more prediction in the aligned cue condition (see the below graph)
	
  # subject-level count, and then t.test on those counts?
  table(box$predictive, box$half) # during non-predictive portion of trial, first half had more in-box than second half...
	table(gd$predictive, gd$half) # during predictive portion, second half of training had more out-of-box than in
  library(plyr)
	oobs <- count(gd, c('cond','half','predictive','subject'))
  boxs <- count(box, c('cond','half','predictive','subject'))
	summary(aov(freq ~ (half * cond * predictive) + Error(subject/(half*predictive + cond)), data = oobs))  # cond: **, half*predictive .09...
	summary(aov(freq ~ (half * cond) + Error(subject/(half + cond)), data = subset(oobs, predictive==T))) # half*
	with(subset(oobs, predictive==T), aggregate(freq, list(half=half), mean)) # divide by 800 movements?
	with(oobs, aggregate(freq, list(cond=cond), mean)) # NoCue spent less time outside boxes...
	with(oobs, aggregate(freq, list(half=half, predictive=predictive), mean)) # first half pred is least time, first half non-pred is most
	
  summary(aov(freq ~ (half * cond * predictive) + Error(subject/(half*predictive + cond)), data = boxs)) # cond**, pred***
	with(boxs, aggregate(freq, list(cond=cond), mean)) # alignedcue spends most time in the boxes...
	with(boxs, aggregate(freq, list(predictive=predictive), mean))
  
	egd$xPos = egd$xPos - 217
  	egd$yPos = egd$yPos - 89
	axlim = c(-20,600)
	axlimr = c(600,-20)
	mq1 = subset(egd, cond=="MisalignedQue" & half=="First")
	aq1 = subset(egd, cond=="AlignedQue" & half=="First")
	nq1 = subset(egd, cond=="NoQue" & half=="First")
	mq2 = subset(egd, cond=="MisalignedQue" & half=="Second")
	aq2 = subset(egd, cond=="AlignedQue" & half=="Second")
	nq2 = subset(egd, cond=="NoQue" & half=="Second")
	pdf("predictive_moves_exp2.pdf")
	par(mfrow=c(3,2), oma=c(0,0,3,0), mar = c(2,1,1,1))
	with(nq1, smoothScatter(xPos, yPos, main="No Cue Early", xlim=axlim, ylim=axlimr, nrpoints=0, xlab="", ylab="", xaxt='n', yaxt='n'))
	with(nq2, smoothScatter(xPos, yPos, main="No Cue Late", xlim=axlim, ylim=axlimr, nrpoints=0, xlab="", ylab="", xaxt='n', yaxt='n'))
	with(aq1, smoothScatter(xPos, yPos, main="Aligned Cue Early", xlim=axlim, ylim=axlimr, nrpoints=0, xlab="", ylab="", xaxt='n', yaxt='n'))
	with(aq2, smoothScatter(xPos, yPos, main="Aligned Cue Late",xlim=axlim, ylim=axlimr, nrpoints=0, xlab="", ylab="", xaxt='n', yaxt='n'))
	with(mq1, smoothScatter(xPos, yPos, main="Misaligned Cue Early", xlim=axlim, ylim=axlimr, nrpoints=0, xlab="", ylab="", xaxt='n', yaxt='n'))
	with(mq2, smoothScatter(xPos, yPos, main="Misaligned Cue Late",xlim=axlim, ylim=axlimr, nrpoints=0, xlab="", ylab="", xaxt='n', yaxt='n'))
	dev.off()
}


preprocess <- function(d) {
	d$score <- NULL
	d$trial = d$trial + 1
	Ntr = length(unique(d$trial)) # 80
	d$half = ifelse(d$trial < (Ntr/2), "First", "Second")
	test = subset(d, cond=="test") # 5% - test trials!
	# test trials take a lot longer than the ~14000ms of training trials
	#prevTarget / timeTargetHit # identity / ms since prev location was touched
	#d$TargetHL = NA # 500ms after a target is reached, the next target stimulus turns green
	d$Time <- cut(d$trialTime, seq(-1,max(d$trialTime),20),labels=F) # levels=seq(-1,max(all$TrTime),20) # 20ms bins..
	d$Time50 <- cut(d$trialTime, 500, labels=F) # 50 time bins per trial (unequal sizes, then)
	d$curTarget = as.factor(d$curTarget)
	# 20 trials/block
	# 0=upper_left, 1=upper_right, 2=lower_left, 3=lower_right
	stim_diam = 80 
	stim_offset = 260
	center=c(1024,768) / 2
	xPos = rep(c(center[1]-stim_offset-stim_diam/2, center[1]+stim_offset-stim_diam/2), 2)
	yPos = c(rep(center[2]-stim_offset-stim_diam/2, 2), rep(center[2]+stim_offset-stim_diam/2, 2)) 
	#orientation = c("v","d","v","v","d","v","h","d","h", "d") # vertical, diagonal, horizontal
	#dir = c("u","ld","u","d","lu","d","l","ru","l", "rd") # up, down, left, right, left-down, etc
	
	#d$orient = NA
	#d$dir = NA
	#for(i in 1:(length(NB_t)-1)) {
	#	movind = with(d, which(curTarget==NB_t[i] & nextTarget==NB_t[i+1]))
	#	d[movind,]$orient = orientation[i]
	#	d[movind,]$dir = dir[i]
	#}
	
	# need to transform x and y coordinates based on prevTarg and nextTarg
	# move to 0,0, then flip around axes if necessary
	#d$x0 = d$xPos - 217
	#d$y0 = d$yPos - 89
	# leave diagonal for now (need to subtract x that depends on their y?)
	# switch x and y of 
	
	return(d)
}

process_hits <- function(d) {	
	hits = subset(d, Correct==1)
	subjs = unique(hits$subject)
	hits$targetRT = NA # how long to reach target (after it turns green)
	for(s in subjs) {
		inds = with(hits, which(subject==s))
		targetRTs = diff(hits[inds,"timeTargetHit"])
		hits[inds,]$targetRT = c(NA,targetRTs)
	}
	
	hits = subset(hits, !is.na(targetRT)) # 77594 down to 77549
	
	# table(hits$subject, hits$trial) # some subjects have far more than 10 hits per trial; early arrival??
	# 45 subjects * 80 trials * 12 locs/tr = 43200  (and only 11 on the first trial, so 43155)
	hits = subset(hits, targetRT>0) # remove the weird ones (with >>12 hits per trial) -- removes a lot (43155)
	
	#Words = as.factor(c("1201","2010","3101","0103","0312","3120"))
	Words = as.factor(c("2312","3121","4212","1214","1423","4231"))
	#Sequence = c("1201","2010","3101","0103","0312","3120","3101","0312","0103","2010","3120","1201","0103", "1201","0312","3101","3120","2010","1201","0103","0312","3101","2010","3120")
	# as.numeric(Sequence) + 1111 # locations are number 1-4 in the paper, not 0-3
	Sequence = c("2312","3121","4212","1214","1423","4231","4212","1423","1214","3121","4231","2312","1214", "2312","1423","4212","4231","3121","2312","1214","1423","4212","3121","4231")
	hits$CurrentWord = NA
	hits[hits$trial<9,]$CurrentWord = rep(c(rep(Sequence[1],3),rep(Sequence[2:24],1,each=4)),45) #first trial misses the first hit on every subject
	hits[hits$trial>8,]$CurrentWord = rep(rep(Sequence,9,each=4),45)
	
	hits$StartNewWord = NA # starting a new word should get harder, and within-word transitions get faster
	hits[hits$trial==1,]$StartNewWord = c(0,0,0,1,0,0,0,1,0,0,0)
	hits[hits$trial>1,]$StartNewWord = c(1,0,0,0,1,0,0,0,1,0,0,0)
	
	hits$PositionInWord = NA # first movement in a word should be hard, but successive movements may get faster (esp in aligned condition?)
	hits[hits$trial==1,]$PositionInWord = c(2,3,4,1,2,3,4,1,2,3,4)
	hits[hits$trial>1,]$PositionInWord =  c(1,2,3,4,1,2,3,4,1,2,3,4)
	
	print(paste("mean RT:",mean(hits$targetRT))) # mean=1144
	print(paste("median RT:",median(hits$targetRT),"sd:",sd(hits$targetRT))) # median=1056, sd=1651 
	#maxRT = median(hits$targetRT) + sd(hits$targetRT) # now use median, see if we can reduce noise
	#hits = subset(hits, targetRT<maxRT) # remove 190 (186 w mean) slow moves (maxRT=2795), 42965 (42969 mean) remain
	hits$Block <- cut(hits$trial, 4,labels=F) 
	hits$Condition = with(hits, ifelse(cond=="NoQue", "No Cue", ifelse(cond=="AlignedQue", "Aligned Cue", "Misaligned Cue")))
	save(hits, file="exp2_all_hits.RData")
	return(hits)
}

d = preprocess(exp2)
hits = process_hits(d)

within_to_between_word_improvement_graph <- function() {
	bw_s <- with(subset(hits, StartNewWord==1), aggregate(targetRT, list(Block=Block, Condition=Condition, subject=subject), median))
	ha_s <- with(subset(hits, StartNewWord==0), aggregate(targetRT, list(Block=Block, Condition=Condition, subject=subject), median))
	ha_s$bwwi = bw_s$x - ha_s$x # how much faster each subject is at within vs between word movements
	ha <- with(ha_s, aggregate(bwwi, list(Block=Block, Condition=Condition), mean))
	ha_sd <- with(ha_s, aggregate(bwwi, list(Block=Block, Condition=Condition), sd))
	ha$SE = ha_sd$x / sqrt(15-1) # 15 subjects per condition
	dodge <- position_dodge(width=.2)
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	qplot(Block, x, data=ha, colour=Condition, position=dodge) + labs(x="Blocks of 20 Training Trials", y="Mean Speedup Within- vs. Between-word Moves (ms)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)
	ggsave("exp2_between_minus_withinWord_by_block.pdf", width=5, height=5)
	dev.off()
	
	summary(aov(bwwi ~ (Block * Condition) + Error(subject/Block) + Condition, data = ha_s))
	with(ha_s, aggregate(bwwi, list(Condition=Condition), mean))
	with(ha_s, aggregate(bwwi, list(Block=Block), mean))
}

traditional_rt <- function(hits) {	
	ha_s <- with(subset(hits, StartNewWord==0), aggregate(targetRT, list(Block=Block, cond=cond, subject=subject), median))
	ha <- with(ha_s, aggregate(x, list(Block=Block, cond=cond), mean))
	ha_sd <- with(ha_s, aggregate(x, list(Block=Block, cond=cond), sd))
	ha$SE = ha_sd$x / sqrt(15-1) # 15 subjects per condition
	dodge <- position_dodge(width=.2)
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	# ggplot has useful functions: cut_interval = equal-length intervals, cut_number = equal number of points
	ha$Condition = with(ha, ifelse(cond=="NoQue", "No Cue", ifelse(cond=="AlignedQue", "Aligned Cue", "Misaligned Cue")))
	qplot(Block, x, data=ha, colour=Condition, position=dodge) + labs(x="Blocks of 20 Training Trials", y="Mean of Median Response Time (ms)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)
	ggsave("exp2_RT_withinWord_over_training.pdf", width=5, height=5)
	dev.off()
	
	ha_s <- with(hits, aggregate(targetRT, list(subject=subject, cond=cond, Block=Block, StartNewWord=StartNewWord), median))
	ha <- with(ha_s, aggregate(x, list(Block=Block, cond=cond, StartNewWord=StartNewWord), mean))
	summary(ha)
	#Median of mean RT by block, two-way analysis of variance, between group within block + interaction
	print(summary(aov(x ~ (Block * StartNewWord * cond) + Error(subject/(Block*StartNewWord)) + cond, data = ha_s)))
	# block***, StartNewWord***, and cond** -- but no interactions
	
	ha_s <- with(hits, aggregate(targetRT, list(subject=subject, cond=cond, Block=Block, PositionInWord=PositionInWord), median))
	ha <- with(ha_s, aggregate(x, list(PositionInWord=PositionInWord, cond=cond, Block=Block), mean))
	print(summary(aov(x ~ (Block * PositionInWord * cond) + Error(subject/(Block*PositionInWord)) + cond, data = ha_s)))
  
	# just within-words -- still just block significant
	#ha_s <- with(subset(hits, StartNewWord==0), aggregate(targetRT, list(subject=subject, cond=cond, Block=Block), median))
	#ha <- with(ha_s, aggregate(x, list(Block=Block, cond=cond), mean))
	#print(summary(aov(x ~ (Block * cond) + Error(subject/(Block)) + cond, data = ha_s)))
	
	with(hits, aggregate(targetRT, list(half=half, cond=cond, StartNewWord=StartNewWord), mean))
	
	ha_s = with(hits, aggregate(targetRT, list(half=half, cond=cond, StartNewWord=StartNewWord, subject=subject), median))
	ha = with(ha_s, aggregate(x, list(half=half, cond=cond, StartNewWord=StartNewWord), mean))
	summary(aov(x ~ (half * StartNewWord * cond) + Error(subject/(half*StartNewWord)) + cond, data = ha_s))
	print(xyplot(x~cond|StartNewWord, groups=half, data=ha, auto.key=T, type='l', ylab="Time (ms)"))
	
	require(nlme)
	hits$Condition = factor(hits$Condition)
	hits$Condition = relevel(hits$Condition, ref="No Cue")
	contrasts(hits$Condition)
	summary(lme(targetRT ~ Block * Condition * StartNewWord, random=~1|subject, data=hits))
	# Block-41, Block*Aligned+11, Block*Misaligned+9.7, Block*StartNewWord+17, Aligned*StartNew+45.9, Misaligned*StartNew+24, Block*Misaligned*StartNew-9.5  AIC=575009 BIC=575130
	summary(lme(targetRT ~ Block * Condition * PositionInWord, random=~1|subject, data=hits)) # AIC=575100 BIC=575221
	# Block-21, Block*PositionInWord-6.4, Aligned*PositionInWord-15.6
	#summary(lme(targetRT ~ Condition * half * StartNewWord, random=~1|subject, data=hits))
	
	# should add a reward column for each condition
	hits$Position = factor(hits$PositionInWord) # BETTER
	summary(lme(targetRT ~ Block * Condition * Position, random=~1|subject, data=hits)) # AIC=574648
	
	condpos = with(hits, aggregate(targetRT, list(Condition=Condition, Position=PositionInWord, Half=half), mean))
	condpos$Imp = NA
	condpos[1:12,"Imp"] = condpos[1:12,"x"] - condpos[13:24,"x"]
	condpos = condpos[1:12,]
	qplot(Position, Imp, data=condpos, colour=Condition, position=dodge) + labs(x="Position", y="Mean of Median Response Time (ms)", fill="Condition") #+ geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)

	blpos = with(hits, aggregate(targetRT, list(Block=Block, Position=PositionInWord), mean))
	qplot(Block, x, data=blpos, colour=Position, position=dodge) + labs(x="Block", y="Mean Response Time (ms)", fill="Position") #+ geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)

	
	wwh = subset(hits, StartNewWord==0) # just the within-word times
	summary(lme(targetRT ~ Block * Condition * Position, random=~1|subject, data=wwh))
	summary(lme(targetRT ~ Block * Condition * PositionInWord, random=~1|subject, data=wwh))
	
	with(wwh, aggregate(targetRT, list(Condition=Condition, curTarget=curTarget, nextTarget=nextTarget, half=half), mean))

	
	summary(lme(targetRT ~ Block * Condition * Movement, random=~1|subject, data=subset(hits, StartNewWord==0)))	
	mv0 = with(subset(hits, StartNewWord==1 & half=="Second"), aggregate(targetRT, list(Condition=Condition, Movement=Movement), mean))
	mv1 = with(subset(wwh, half=="First"), aggregate(targetRT, list(Condition=Condition, Movement=Movement), mean))
	mv2 = with(subset(wwh, half=="Second"), aggregate(targetRT, list(Condition=Condition, Movement=Movement), mean))
	mv1$Imp = mv1$x - mv2$x
	
	#summary(lme(targetRT ~ Movement, random=~1|subject, data=subset(hits, StartNewWord==0))) # 429888.5	
	#summary(lme(targetRT ~ Block, random=~1|subject, data=subset(hits, StartNewWord==0)))	 # 432731.4
	#summary(lme(targetRT ~ Condition, random=~1|subject, data=subset(hits, StartNewWord==0)))	 # 433996.8


}

traditional_rt(hits)


graph_move_improvements <- function(hits) {
	load("exp2_hits.RData")
	hits$prevTarget = as.numeric(as.character(hits$prevTarget)) + 1
	hits$curTarget = as.numeric(as.character(hits$curTarget)) + 1
	hits$Movement = with(hits, paste(as.character(prevTarget), as.character(curTarget), sep='-'))
	ha_s <- with(subset(hits, StartNewWord==0), aggregate(targetRT, list(Block=Block, Condition=Condition, Movement=Movement, subject=subject), median))
	
	imps = subset(ha_s, Block==4)
	imps$Imp = NA
	for(s in unique(ha_s$subject)) {
		imps[which(imps$subject==s),]$Imp = ha_s[which(ha_s$subject==s & ha_s$Block==1),]$x - ha_s[which(ha_s$subject==s & ha_s$Block==4),]$x
	}
	
	ha <- with(imps, aggregate(Imp, list(Condition=Condition, Movement= Movement), mean))
	ha_sd <- with(imps, aggregate(Imp, list(Condition=Condition, Movement = Movement), sd))
	ha$SE = ha_sd$x / sqrt(15-1) # 15 subjects per condition
	
	dodge <- position_dodge(width=.3)
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	# ggplot has useful functions: cut_interval = equal-length intervals, cut_number = equal number of points
	qplot(Movement, x, data=ha, colour=Condition, position=dodge) + labs(x="Movement", y="Mean RT Improvement (Block 1 - Block 4) (ms)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)
	ggsave("exp2_RTimprov_movement_by_cond.pdf", width=5.5, height=5)
	dev.off()
}


graph_improvements <- function(hits) {
	load("exp2_hits.RData")
	ha_s <- with(hits, aggregate(targetRT, list(Block=Block, Condition=Condition, Position=PositionInWord, subject=subject), median))
	
	imps = subset(ha_s, Block==4)
	imps$Imp = NA
	for(s in unique(ha_s$subject)) {
		imps[which(imps$subject==s),]$Imp = ha_s[which(ha_s$subject==s & ha_s$Block==1),]$x - ha_s[which(ha_s$subject==s & ha_s$Block==4),]$x
	}
	
	ha <- with(imps, aggregate(Imp, list(Condition=Condition, Position=Position), mean))
	ha_sd <- with(imps, aggregate(Imp, list(Condition=Condition, Position=Position), sd))
	ha$SE = ha_sd$x / sqrt(15-1) # 15 subjects per condition
	
	dodge <- position_dodge(width=.2)
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	# ggplot has useful functions: cut_interval = equal-length intervals, cut_number = equal number of points
	qplot(Position, x, data=ha, colour=Condition, position=dodge) + labs(x="Position", y="Mean RT Improvement (Block 1 - Block 4) (ms)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)
	ggsave("exp2_RTimprov_pos_by_cond.pdf", width=5, height=5)
	dev.off()
	
	summary(aov(Imp ~ (Position * Condition) + Error(subject/Position) + Condition, data = imps))
}


# make a nice graph of 1 trial for introduction
# SHOULD UPDATE THIS TO DRAW EACH WORD (for Exp2) IN A DIFFERENT COLOR
graph_one_trial <- function(d, subject_num, trial_num, name="") {
  axlim = c(-20,600)
  tr2 = subset(d, prevTarget==2 & curTarget==0 & nextTarget==1 & yPos >550 & yPos < 580 & half=="Second")
  tr2$xPos = tr2$xPos - 217
  tr2$yPos = tr2$yPos - 89

  #print(with(tr2, plot(xPos, yPos, type='p', pch=nextTarget+2, col=heat.colors(dim(tr2)[1])[Time], xlim=axlim, ylim=axlim, xlab="X Cursor Position", ylab="Y Cursor Position"))) # as.factor(ckey[curTarget])
  print(with(tr2, plot(xPos, yPos, type='p', pch='.', col=heat.colors(dim(tr2)[1])[Time], xlim=axlim, ylim=axlim, xlab="X Cursor Position", ylab="Y Cursor Position"))) # as.factor(ckey[curTarget])
  leftx = c(0,80,80,0); rightx = c(520,600,600,520)
  boty = c(0,0,80,80); topy = c(600,600,520,520)
  polygon(leftx, boty); polygon(leftx, topy)
  polygon(rightx, boty); polygon(rightx, topy)
}

graph_one_trial(d, 1, 1, name="1trial_s1tr7_rand") # random



heatmap_for_movement <- function(d) {
  axlim = c(-20,600)
  tr2 = subset(d, (prevTarget==2 & curTarget==0 & nextTarget==1 & half=="First") )
  tr2$xPos = tr2$xPos - 217
  tr2$yPos = tr2$yPos - 89
  
  smoothScatter(tr2$xPos,tr2$yPos, nrpoints=0, main="Heat map for first half", xlab="X Cursor Position", ylab="Y Cursor Position", xlim=axlim, ylim=axlim)
  
  leftx = c(0,80,80,0); rightx = c(520,600,600,520)
  boty = c(0,0,80,80); topy = c(600,600,520,520)
  polygon(leftx, boty); polygon(leftx, topy)
  polygon(rightx, boty); polygon(rightx, topy)
}

heatmap_for_movement(d) 








heatmap_for_all_movements <- function(d) {
  axlim = c(-20,600)
  tr2 = subset(d, (half=="Second" & !(xPos > 217 & xPos < 297 & yPos > 89 & yPos < 169) & !(xPos > 217 & xPos < 297 & yPos > 609 & yPos < 689) & !(xPos > 737 & xPos < 817 & yPos > 609 & yPos < 689)  & !(xPos > 737 & xPos < 817 & yPos > 89 & yPos < 169)        ))
  tr2$xPos = tr2$xPos - 217
  tr2$yPos = tr2$yPos - 89
  
  #print(with(tr2, plot(xPos, yPos, type='p', pch='.', col=heat.colors(dim(tr2)[1])[Time], xlim=axlim, ylim=axlim, xlab="X Cursor Position", ylab="Y Cursor Position"))) # as.factor(ckey[curTarget])
  smoothScatter(tr2$xPos,tr2$yPos, nrpoints=0, main="Heat map for first half", xlab="X Cursor Position", ylab="Y Cursor Position", xlim=axlim, ylim=axlim)
  
  leftx = c(0,80,80,0); rightx = c(520,600,600,520)
  boty = c(0,0,80,80); topy = c(600,600,520,520)
  polygon(leftx, boty); polygon(leftx, topy)
  polygon(rightx, boty); polygon(rightx, topy)
}

heatmap_for_all_movements(d) 





heatmap_hits <- function(hits) {
  axlim = c(-20,600)
  tr2 = hits
  tr2$xPos = tr2$xPos - 217
  tr2$yPos = tr2$yPos - 89
  
  print(with(tr2, plot(xPos, yPos, type='p', pch='.', col=CurrentWord, xlim=axlim, ylim=axlim, xlab="X Cursor Position", ylab="Y Cursor Position"))) # as.factor(ckey[curTarget])
  #smoothScatter(tr2$xPos,tr2$yPos, nbin=256, bandwidth=8, nrpoints=0, main="Heat map for first half", xlab="X Cursor Position", ylab="Y Cursor Position", xlim=axlim, ylim=axlim)
  
  leftx = c(0,80,80,0); rightx = c(520,600,600,520)
  boty = c(0,0,80,80); topy = c(600,600,520,520)
  polygon(leftx, boty); polygon(leftx, topy)
  polygon(rightx, boty); polygon(rightx, topy)
}

heatmap_hits(hits) 









heatmap_plot_by_word <- function(hits) {
  axlim = c(-20,600)
  axlimrev = c(600,-20)
  
  tr2 = hits
  tr2$xPos = tr2$xPos - 217
  tr2$yPos = tr2$yPos - 89
  
  
  par(mfrow=c(2,3), oma=c(0,0,3,0))
  
  for (word in unique(hits$CurrentWord)){
  
  tr <- subset(tr2, CurrentWord==word & half=="Second")
  
  #print(with(tr, plot(xPos, yPos, main=paste('word',word), type='p', pch='.', col=CurrentWord, xlim=axlim, ylim=axlim, xlab="X Cursor Position", ylab="Y Cursor Position"))) # as.factor(ckey[curTarget])
  smoothScatter(tr$xPos,tr$yPos, nbin=256, bandwidth=2, nrpoints=0, main=paste('word',word), xlab="", ylab="", xaxt='n', yaxt='n', xlim=axlim, ylim=axlimrev)
      
  }
  mtext("Second half", 3, 0, outer=TRUE, cex=1)

}

heatmap_plot_by_word(hits) 







test_halves <- function(d) {
  
half1 = subset(d, prevTarget==2 & curTarget==0 & nextTarget==1 & yPos >575 & yPos < 580 & half=="First")
half2 = subset(d, prevTarget==2 & curTarget==0 & nextTarget==1 & yPos >575 & yPos < 580 & half=="Second")

test_results <- t.test(half1$xPos,half2$xPos)

return(test_results)

}

test_halves(d)
# Hard to tell with so many data points, but significant difference between x-values in this band of y-values





agg1 <- subset(d, prevTarget==3 & curTarget==1 & cond=="AlignedQue" & yPos >550 & yPos < 580)
agg <- with(agg1, aggregate(xPos, list(subject=subject, cond=cond, half=half), FUN=median, na.rm=T))


for (i in 1:nrow(agg1)){
  if (agg1$cond[i] == "AlignedQue"){
  agg1$condnum[i] <- 1
  }
  else if (agg1$cond[i] == "MisalignedQue"){
    agg1$condnum[i] <- 2
  }
  else {
    agg1$condnum[i] <- 3
  }
  
}





get_movement_time_index <- function(dat) {
	# ggplot has useful functions: cut_interval = equal-length intervals, cut_number = equal number of points
	#dat$normMove = NA # normalize movement: equal-
	dat$normTime = NA # normalize time: same number of bins per movement
	dat$moveTime = NA # 
	for(s in unique(dat$subject)) {
		print(paste("Subject",s))
		s_ind = which(dat$subject==s)
		for(t in unique(dat[s_ind,]$trial)) {
			t_ind = which(dat$subject==s & dat$trial==t)
			for(m in unique(dat[t_ind,]$Movement)) {
				m_ind = which(dat$subject==s & dat$trial==t & dat$Movement==m)
				dat[m_ind,]$moveTime = dat[m_ind,]$trialTime - dat[m_ind[1],]$trialTime
				dat[m_ind,]$normTime = cut_interval(dat[m_ind,]$moveTime, n=40, labels=F)
			}
		}
	}
	return(dat)
}

analysis_by_position_in_word <- function() {
	load("exp2_hits.RData")
	hits$StartNewWord = ifelse(hits$StartNewWord==1, "Yes", "No")
	ha_s <- with(hits, aggregate(targetRT, list(Block=Block, Condition=Condition, subject=subject, StartNewWord=StartNewWord), median))
	ha <- with(ha_s, aggregate(x, list(Block=Block, Condition=Condition, StartNewWord=StartNewWord), mean))
	ha_sd <- with(ha_s, aggregate(x, list(Block=Block, Condition=Condition, StartNewWord=StartNewWord), sd))
	ha$SE = ha_sd$x / sqrt(15-1) # 15 subjects per condition
	dodge <- position_dodge(width=.2)
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	# ggplot has useful functions: cut_interval = equal-length intervals, cut_number = equal number of points
	qplot(Block, x, data=ha, colour=Condition, linetype=StartNewWord, position=dodge) + labs(x="Blocks of 20 Training Trials", y="Mean of Median Response Time (ms)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) 
	ggsave("exp2_RT_by_block_byStartNewWord.pdf", width=5, height=4.2)
	dev.off()
	
	ha_s <- with(hits, aggregate(targetRT, list(Half=half, Condition=Condition, subject=subject, PositionInWord=PositionInWord), median))
	ha <- with(ha_s, aggregate(x, list(Half=Half, Condition=Condition, PositionInWord=PositionInWord), mean))
	ha_sd <- with(ha_s, aggregate(x, list(Half=Half, Condition=Condition, PositionInWord=PositionInWord), sd))
	ha$SE = ha_sd$x / sqrt(15-1) # 15 subjects per condition
	dodge <- position_dodge(width=.3)
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	# ggplot has useful functions: cut_interval = equal-length intervals, cut_number = equal number of points
	qplot(PositionInWord, x, data=ha, colour=Condition, linetype=Half, position=dodge) + labs(x="Position in Word", y="Mean of Median Response Time (ms)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) 
	ggsave("exp2_RT_by_PositionInWord_by_half.pdf", width=5, height=4.2)
	dev.off()
	
	######### USE THIS ANOVA #########
	with(hits, aggregate(targetRT, list(Block=Block, cond=cond, PositionInWord=PositionInWord), mean))
	ha_s = with(hits, aggregate(targetRT, list(Block=Block, cond=cond, PositionInWord=PositionInWord, subject=subject), median))
	ha = with(ha_s, aggregate(x, list(Block=Block, cond=cond, PositionInWord=PositionInWord), mean))
	summary(aov(x ~ (Block * PositionInWord * cond) + Error(subject/(Block*PositionInWord)) + cond, data = ha_s))
	
	with(hits, aggregate(targetRT, list(cond=cond), mean))
	with(hits, aggregate(targetRT, list(PositionInWord=PositionInWord), mean))
	with(hits, aggregate(targetRT, list(Block=Block), mean))
	
	#print(xyplot(x~cond|Block, groups=PositionInWord, data=ha, auto.key=T, type='l', ylab="Time (ms)"))
	#print(xyplot(x~as.factor(PositionInWord)|as.factor(Block), groups=cond, data=ha, auto.key=T, type='l', ylab="Time (ms)"))
	#print(xyplot(x~as.factor(Block)|as.factor(PositionInWord), groups=cond, data=ha, auto.key=T, type='l', ylab="Time (ms)"))
	
	##### make this a ggplot and include it (marginally significant interaction that it is)
	blco = with(ha_s, aggregate(x, list(Block=Block, PositionInWord=PositionInWord), mean))
	print(xyplot(x~as.factor(Block), groups=PositionInWord, data=blco, auto.key=T, type='l', ylab="Time (ms)"))
	####
	
	
	#with(hits, aggregate(targetRT, list(half=half, cond=cond, PositionInWord=PositionInWord), mean))
	#ha_s = with(hits, aggregate(targetRT, list(half=half, cond=cond, PositionInWord=PositionInWord, subject=subject), median))
	#ha = with(ha_s, aggregate(x, list(half=half, cond=cond, PositionInWord=PositionInWord), mean))
	#summary(aov(x ~ (half * PositionInWord * cond) + Error(subject/(half*PositionInWord)) + cond, data = ha_s))
	#print(xyplot(x~cond|PositionInWord, groups=half, data=ha, auto.key=T, type='l', ylab="Time (ms)"))
	
	# if I can change the contrast (helmert?), this might be a better way to present the results than ANOVA
	require(nlme)
	summary(lme(targetRT ~ Block * Condition * PositionInWord, random=~1|subject, data=hits))
}


word_improvement_graph <- function(){
	load("exp2_hits.RData")
	
	ha_s <- with(subset(hits, StartNewWord==0), aggregate(targetRT, list(Word=CurrentWord, Block=Block, Condition=Condition, subject=subject), median))
	
	imps = subset(ha_s, Block==4)
	imps$Imp = NA
	for(s in unique(ha_s$subject)) {
		imps[which(imps$subject==s),]$Imp = ha_s[which(ha_s$subject==s & ha_s$Block==1),]$x - ha_s[which(ha_s$subject==s & ha_s$Block==4),]$x
	}
	dodge <- position_dodge(width=.3)
	ha <- with(imps, aggregate(Imp, list(Word=Word, Condition=Condition), mean))
	ha_sd <- with(imps, aggregate(Imp, list(Word=Word, Condition=Condition), sd))
	ha$SE = ha_sd$x / sqrt(15-1) # 15 subjects per condition
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	a = qplot(Word, x, data=ha, colour=Condition, position=dodge) + labs(x="Words", y="Mean Improvement from Block 1 to Block 4 (ms)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)
	print(a)
	ggsave("exp2_withinWordRT_improve_by_cond.pdf", width=5.5, height=5)
	dev.off()
}



Word_graphs <- function(){
	load("exp2_hits.RData")
	
	ha_s <- with(subset(hits, StartNewWord==0), aggregate(targetRT, list(Word=CurrentWord, Block=Block, Condition=Condition, subject=subject), median))
	ha <- with(ha_s, aggregate(x, list(Word=Word, Block=Block), mean))
	ha_sd <- with(ha_s, aggregate(x, list(Word=Word, Block=Block), sd))
	ha$SE = ha_sd$x / sqrt(15-1) # 15 subjects per condition
	dodge <- position_dodge(width=.3)
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	a = qplot(Block, x, data=ha, colour=Word, position=dodge) + labs(x="Blocks of 20 Training Trials", y="Mean of Median Response Time (ms)") + geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)
	print(a)
	ggsave("exp2_withinWordRT_over_training.pdf", width=5.5, height=5)
	dev.off()

	ha <- with(ha_s, aggregate(x, list(Word=Word, Condition=Condition), mean))
	ha_sd <- with(ha_s, aggregate(x, list(Word=Word, Condition=Condition), sd))
	ha$SE = ha_sd$x / sqrt(15-1) # 15 subjects per condition
	limits <- with(ha, aes(ymax=x+SE, ymin=x-SE))
	a = qplot(Word, x, data=ha, colour=Condition, position=dodge) + labs(x="Words", y="Mean of Median Response Time (ms)", fill="Condition") + geom_errorbar(limits,  width=0.2, position=dodge) # + ylim(0,.82)
	print(a)
	ggsave("exp2_withinWordRT_per condition.pdf", width=5.5, height=5)
	dev.off()
}
 

test_graphs <- function(){
	load("exp2_saffran_traj_test.RData")
	test$cond = with(test,ifelse(subject%%3==1, "Misaligned Cue", ifelse(subject%%3==2,"Aligned Cue","No Cue")))
	test$scorediff=NA
	for (s in unique(test$subject)){
		test[test$subject==s,]$scorediff = c(NA,diff(test[test$subject==s,]$score))
	}
	
	colls = subset(test,scorediff!=0) #with(colls,table(subject,trial)) 	# all times object was hit
	hits = subset(colls,scorediff>=0) #with(hits,table(subject,trial))		# all times target was hit
	misses = subset(colls,scorediff<0) #with(misses,table(subject,trial))	# all times distracor was hit
	reps = NA #with(reps,table(subject,trial))	# all times a word was succesfully completed
	for (s in unique(hits$subject)){
		for (t in unique(hits$trial)){
			if (nrow(subset(hits,trial==t & subject==s))==4){
				reps = rbind(reps,subset(hits,trial==t & subject==s))
			} 
		}
	}
	
	# THIS FOR LOOP DOESN'T WORK ~GEORGE
	reps$CurrentWord = NA # The word a participant succesfully reproduced
	for (s in unique(reps$subject)){
		for (t in unique(reps$trial)){
			if (nrow(reps[reps$subject==s&reps$trial==t,])!=0){
				reps[reps$subject==s&reps$trial==t,]$CurrentWord = rep(paste(subset(reps,subject==s & trial==t)$objTouched,collapse=''),4)
			}
		}
	}

	a_s <- as.data.frame(with(reps,table(trial,CurrentWord,cond,subject))) # Number of reproductions per trial,word,condition and subject
	ha_s$Freq = ha_s$Freq / 4 # frequency reflected number of hits (= 4 per word)
	ha <- with(ha_s,aggregate(Freq,list(Word=CurrentWord,cond=cond),sum)) 
	a <- ggplot(data=ha) + geom_boxplot(aes(x=cond,y=x)) + labs(x="Training Condition", y="Number of reproduced words")
	print(a)
	ggsave("exp2_reproductions_per_condition.pdf", width=5, height=5)
	dev.off()

	ha <- with(ha_s,aggregate(Freq,list(Word=CurrentWord,cond=cond),sum)) 
	a <- ggplot(data=ha) + geom_boxplot(aes(x=Word,y=x)) + labs(x="Word", y="Number of reproductions")
	print(a)
	ggsave("exp2_reproductions_per_word.pdf", width=5, height=5)
	dev.off()
}


# curTarget is where you should be headed right now (updated after it's turned green--I think)
# prevTarget is where you just left (or are still leaving)
# nextTarget is where you should go after you arrive where you're going now

# should be looking at prev to cur for movement direction
# cur to next for context effects (predictive)

#d$yPos = d$yPos - 89
#d$xPos = d$xPos - 217
	# NB: 3 1 2 0 2 1 3 2 1 0
	# compare 2-0 to 3-1 movement for context effects
#d$Movement = with(d, paste(as.numeric(as.character(prevTarget))+1,"-",as.numeric(as.character(curTarget))+1, sep='')) # "0-4" was -1 in prevTarget...maybe remove those?
#d$Half = as.factor(d$half)	
#d$Movement = as.factor(d$Movement)
#d = get_movement_time_index(d) # SLOW PART!
#save(d, file="exp2_trajs.RData")
load("exp2_trajs.RData")
 