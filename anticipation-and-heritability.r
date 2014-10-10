options(stringsAsFactors=FALSE)
require(sqldf)

# parent-child ("pc") pairs
pc = read.table('/humgen/atgu1/fs03/eminikel/039famil/parent-child-pairs.txt',header=FALSE,sep='\t')
colnames(pc) = c('child_id','parent_id','child_ad','parent_ad','child_yob','parent_yob','child_yod','parent_yod')

# basic sanity check on parent/child ages of death - should all be between 0 and 120,
# and parent should not die before child is born (-1 to allow rare cases of father dying after conception)
error_free_pairs = pc$child_ad >= 0 & pc$child_ad <= 120 & pc$parent_ad >=0 & pc$parent_ad <= 120 & pc$parent_yod >= pc$child_yob - 1
sum(!error_free_pairs) # check the number of pairs that appear to have errors
pc = pc[error_free_pairs,] # remove the errors

t.test(pc$parent_ad, pc$child_ad, paired=TRUE, alternative='two.sided')

anticipation_vs_min_age = data.frame(min_age_of_death=integer(0),anticipation=numeric(0),p=numeric(0))
for (min_age_of_death in 0:80) {
    subset = pc$child_ad >= min_age_of_death & pc$parent_ad >= min_age_of_death
    t_test_result = t.test(pc$parent_ad[subset], pc$child_ad[subset], paired=TRUE, alternative='two.sided')
    anticipation = as.numeric(t_test_result$estimate) # mean paired difference
    p = as.numeric(t_test_result$p.value)
    anticipation_vs_min_age = rbind(anticipation_vs_min_age, c(min_age_of_death,anticipation,p))
}
colnames(anticipation_vs_min_age) = c('min_age_of_death','anticipation','p')

# plot how the "anticipation" signal changes depending on the minimum age you set to include pairs in the analysis
plot(NA,NA,xlim=c(0,80),ylim=c(-2,14),xaxs='i',yaxs='i',axes=FALSE,
    xlab='Minimum age of death for inclusion',
    ylab='Paired difference between ages of death',
    main='"Anticipation" mostly due to child mortality')
axis(side=1,at=(0:4)*20,labels=(0:4)*20,lwd=0,lwd.ticks=1)
axis(side=2,at=-1:13,labels=-1:13,lwd=0,lwd.ticks=1,las=1,cex.axis=.8)
abline(v=0,col='#777777',lwd=3)
abline(h=0,col='#777777',lwd=3)
abline(h=c(5,10),col='#777777')
text(x=0,y=.5,pos=4,label='Parent lives longer than child',col='#777777')
text(x=0,y=-.5,pos=4,label='Child lives longer than parent',col='#777777')
points(anticipation_vs_min_age$min_age_of_death, anticipation_vs_min_age$anticipation,type='l',lwd=5,col='red')

# all people ("ppl") with year of birth and death
ppl = read.table('/humgen/atgu1/fs03/eminikel/039famil/yob-yod-ad.txt',header=FALSE,sep='\t')
colnames(ppl) = c('yob','yod','ad')

# basic sanity check - people should be born and died before 2014 and have lived 0 to 120 years
error_free_ppl = ppl$yob <= 2014 & ppl$yod <= 2014 & ppl$ad >= 0 & ppl$ad <= 120
sum(!error_free_ppl) # check the number of individuals with errors
ppl = ppl[error_free_ppl,] # remove the errors

# use year of birth to explain age of death
m = lm(ppl$ad ~ ppl$yob)
summary(m)

life_expectancy_by_year = sqldf("
select   yob, avg(ad) mean_ad
from     ppl
group by 1
order by 1
;")

plot(NA,NA,xlim=c(1800,2014),ylim=c(0,120),xaxs='i',yaxs='i',axes=FALSE,
    xlab='Year of birth',
    ylab='Mean age of death',
    main='')
axis(side=1,at=c(1800,1850,1900,1950,2000,2014),labels=c(1800,1850,1900,1950,2000,2014),lwd=0,lwd.ticks=1,cex.axis=.8)
axis(side=2,at=(0:6)*20,labels=(0:6)*20,lwd=0,lwd.ticks=1,cex.axis=.8,las=1)
points(life_expectancy_by_year$yob,life_expectancy_by_year$mean_ad,type='l',lwd=5,col='#FF9912')


# correlation between parent and child year of birth
m = lm(pc$child_yob ~ pc$parent_yob)
summary(m)

m = lm(pc$child_ad ~ pc$parent_ad)
summary(m)

m = lm(pc$child_ad ~ pc$parent_ad + pc$child_yob)
summary(m)