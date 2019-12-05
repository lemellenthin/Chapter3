install.packages("xlxs")
install.packages("xlsx")
install.packages("ape")
install.packages("geomorph")
install.packages("rgl")


library(geomorph)

library(xlsx)
Test1 <- read.xlsx("~/Desktop/Test 1.xlsx", sheetName = "Mar 2")
is.factor(Test1$ID)
Test1$ID <- as.factor(Test1$ID)

Test1[which(Test1$ID == "160590"),13] <- NA # Replace tail lengths with NA b/c cut
Test1 <- Test1[,-16] # Delete column of notes for analysis
replicate <- as.factor(rep(1:3,15))


procD.lm(Test1$SVL ~ Test1$ID*replicate)
1-0.00055 # 99.945%

procD.lm(Test1$SE ~ Test1$ID*replicate)
1-0.00763 # 99.237%

procD.lm(Test1$SWL ~ Test1$ID*replicate)
1-0.02099 # 97.901%

procD.lm(Test1$SWV ~ Test1$ID*replicate)
1-0.14252 #85.748%

procD.lm(Test1$HL ~ Test1$ID*replicate)
1-0.00935 #99.065%

procD.lm(Test1$HWL ~ Test1$ID*replicate)
1-0.00429 #99.571%

procD.lm(Test1$HWV ~ Test1$ID*replicate)
1-0.02549 #97.451%

procD.lm(Test1$BWL ~ Test1$ID*replicate)
1-0.01105 #98.895%

procD.lm(Test1$BWV ~ Test1$ID*replicate)
1-0.01727 #98.273%

procD.lm(Test1$FLL ~ Test1$ID*replicate)
1-0.00615 # 99.385%

procD.lm(Test1$HLL ~ Test1$ID*replicate)
1-0.00304 # 99.696%

procD.lm(Test1$TWL ~ Test1$ID*replicate)
1-0.01631 # 98.369%

procD.lm(Test1$TWV ~ Test1$ID*replicate)
1-0.01525 # 98.475%

Test2 <- Test1[-which(Test1$ID == "160590"),]
replicate2 <- replicate[1:42]
procD.lm(Test2$TL ~ Test2$ID*replicate2)
1-0.00071 # 99.929%

procD.lm(Test1$SWV ~ Test1$ID*replicate)
1-0.01399 #98.601

length(replicate)
is.numeric(Test1$SVL)

jkflsd
?anova

anova(lm(Test1$SVL~Test1$ID*replicate))


EricaTest <- read.xlsx ("~/Desktop/EricaTest.xlsx", sheetName = "Sheet5")

EricaTest <- EricaTest[31:45,]
EricaTest[,1] <- as.factor(EricaTest[,1])

LaurenTest <- read.xlsx ("~/Desktop/Test 1.xlsx", sheetName = "Mar 6")

LaurenTest <- LaurenTest[, -16]

LaurenTest[,1] <- as.factor(LaurenTest[,1])

IDs <- as.character(EricaTest[,1])
IDs <- as.numeric(IDs)

match(EricaTest[,1],IDsOrdered)

X <- c("a", "b", "c", "d")
Y <- X[c(3,4,1,3)]

EricaTest[]
IDsOrdered <- sort(IDs)


plot(EricaTest$SVL~LaurenTest$SVL)
