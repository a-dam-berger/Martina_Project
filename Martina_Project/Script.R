
#Experiment 3 ----
#Jack, Williams82, Thorne
#PreTreatments:4C,10/8C,10/8+4C,none
#BNN 2% Sucrose 2%Sorbitol
#2,4-E:0, 40
#Incubation Temp:11C, 25C

#Knitr = General Purpose Package for Dynamic Report Generation
library(knitr)
#Broom = Convert Statistical Analysis Objects into Tidy Tibbles
library(broom)
#ggplot2 = The grammer of graphics
library(ggplot2)
#Car = Companion to Applied Regression
library(car)
#stringr is used for string (text) manipulation
library(stringr)
#graphics is used to create graphics displays
library(graphics)
#sqldf is used to run SQLite commands in R Script
library(sqldf)

directory <- getwd()
drive <- substring(directory, 1, 1)
directory
Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
head(Exp3)
View(Exp3)

#Creates table callled Exp3 from CSV file
Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
#runs glm model using Exp3
mr <- glm(PGC ~ (Genotype + Pre.Treat) ^ 2, family = binomial, data = Exp3)
#Anova 
#Type 1 = Sequential Sum of Squares, 
#Type 2 = tests each main effect is tested after other main effect
#Type 3 = tests for the presence of a main effect after the other main effect and interaction. This approach is therefore valid in the presence of significant interactions.
ar <- Anova(mr, type = 2)
#Summary is saved as SMR - which shows the significant asterisks
smr <- summary(mr)
#tidy outputs the subset of summary for the coefficients to a table
res <- tidy(mr)
#View(res)

#Creatss a 2nd version of the tidy (res) result for the control of Jack 10/8C & 4C
Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Pre.Treat == "10/8C", 3:3] <- c("Z10/8C")
mrZ <- glm(PGC ~ (Genotype + Pre.Treat) ^2, family = binomial, data = Exp3_r1)
AnZ <- Anova(mrZ, type = 2)
res2 <- tidy(mrZ)

#Creates a 3rd version of the tidy (res) result for the control of Jack-4C
Exp3_r2 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r2[Exp3_r2$Pre.Treat == "10/8C", 3:3] <- c("Z10/8C")
Exp3_r2[Exp3_r2$Pre.Treat == "10/8C & 4C", 3:3] <- c("Z10/8C & 4C")
mrZ2 <- glm(PGC ~ (Genotype + Pre.Treat) ^ 2, family = binomial, data = Exp3_r2)
AnZ2 <- Anova(mrZ2, type = 2)
res3 <- tidy(mrZ2)
#summary(mrZ2)

#Creates an empty data frame
results <- data.frame()
#populates the data frame by looping through the list of Genotypes & PreTreatments
for (geno in unique(Exp3$Genotype)) {
    for (treat in unique(Exp3$Pre.Treat)) {
      #Runs a prediction line for each combination & adds that information to the data frame
    pr <- predict.glm(mr, list(Genotype = geno, Pre.Treat = treat), type = "response", se.fit = T)
      df <- data.frame(Genotype = geno, Pre.Treat = treat, fit = pr$fit, se.fit = pr$se.fit, residual.scale = pr$residual.scale, TermName = paste("Genotype", geno, ":Pre.Treat", treat, sep = ""))
    results <- rbind(results, df)
  }
}
results

#builds the sql command as text then passes that text into the SQL operation
sqlcmd <- "SELECT DISTINCT * FROM (
            SELECT R.[Genotype], R.[Pre.Treat], R.fit, R.[se.fit], R.[residual.scale]
                    , C.estimate as [estimate]
                    , C.[std.error] as [std.error]
                    , C.[statistic] as [statistic]
                    , C.[p.value] as [p.value] 
                    , 'resA' as source
                    , 'Jack+10/8C' as Control
                    --, replace(substr(c.term,1,instr(c.term,':')-1),'Genotype','') as TestGenotype
                    --, replace(substr(c.term,instr(c.term,':')+1,length(c.term)),'Pre.Treat','') as TestTreat
                    FROM results as R 
                    LEFT JOIN (SELECT *
                                ,replace(substr(term,1,instr(term,':')-1),'Genotype','') as Genotype
                                ,replace(substr(term,instr(term,':')+1,length(term)),'Pre.Treat','') as [Pre.Treat]
                               FROM res) as C ON 
                               ((R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR
                                ('Z'||R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR 
                                ('Z'||R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]) OR
                                (R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]))
                    WHERE C.estimate is not null
            UNION ALL
            SELECT R.[Genotype], R.[Pre.Treat], R.fit, R.[se.fit], R.[residual.scale]
                    , C.estimate as [estimate]
                    , C.[std.error] as [std.error]
                    , C.[statistic] as [statistic]
                    , C.[p.value] as [p.value] 
                    , 'res2A' as source
                    , 'Jack-10/8C & 4C' as Control
                    FROM results as R 
                    LEFT JOIN (SELECT *
                                ,replace(substr(term,1,instr(term,':')-1),'Genotype','') as Genotype
                                ,replace(substr(term,instr(term,':')+1,length(term)),'Pre.Treat','') as [Pre.Treat]
                               FROM res2) as C ON 
                               ((R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR
                                ('Z'||R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR 
                                ('Z'||R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]) OR
                                (R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]))
                    WHERE C.estimate is not null
            UNION ALL
            SELECT R.[Genotype], R.[Pre.Treat], R.fit, R.[se.fit], R.[residual.scale]
                    , C.estimate as [estimate]
                    , C.[std.error] as [std.error]
                    , C.[statistic] as [statistic]
                    , C.[p.value] as [p.value] 
                    , 'res3A' as source
                    , 'Jack-4C' as Control
                    FROM results as R 
                    LEFT JOIN (SELECT *
                                ,replace(substr(term,1,instr(term,':')-1),'Genotype','') as Genotype
                                ,replace(substr(term,instr(term,':')+1,length(term)),'Pre.Treat','') as [Pre.Treat]
                               FROM res3) as C ON 
                               ((R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR
                                ('Z'||R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR 
                                ('Z'||R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]) OR
                                (R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]))
                    WHERE C.estimate is not null
            UNION ALL
            SELECT R.[Genotype], R.[Pre.Treat], R.fit, R.[se.fit], R.[residual.scale]
                    , C.estimate as [estimate]
                    , C.[std.error] as [std.error]
                    , C.[statistic] as [statistic]
                    , C.[p.value] as [p.value] 
                    , 'resB' as source
                    , 'Jack-10/8C' as Control
                    FROM results as R 
                    LEFT JOIN (SELECT *
                                ,CASE WHEN instr(term,'Genotype')>0 
                                      THEN substr(term,instr(term,'Genotype')+8,length(term))
                                      ELSE 'Jack' END as Genotype
                                ,CASE WHEN instr(term,'Pre.Treat')>0 
                                      THEN substr(term,instr(term,'Pre.Treat')+9,length(term))
                                      ELSE '10/8C' END as [Pre.Treat]
                               FROM res
                               WHERE instr(term,':') < 1
                               ) as C ON 
                               ((R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR
                                ('Z'||R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR 
                                ('Z'||R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]) OR
                                (R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]))
                    WHERE C.estimate is not null
            UNION ALL
            SELECT R.[Genotype], R.[Pre.Treat], R.fit, R.[se.fit], R.[residual.scale]
                    , C.estimate as [estimate]
                    , C.[std.error] as [std.error]
                    , C.[statistic] as [statistic]
                    , C.[p.value] as [p.value] 
                    , 'res2B' as source
                    , 'Jack-10/8C & 4C' as Control
                    FROM results as R 
                    LEFT JOIN (SELECT *
                                ,CASE WHEN instr(term,'Genotype')>0 
                                      THEN substr(term,instr(term,'Genotype')+8,length(term))
                                      ELSE 'Jack' END as Genotype
                                ,CASE WHEN instr(term,'Pre.Treat')>0 
                                      THEN substr(term,instr(term,'Pre.Treat')+9,length(term))
                                      ELSE '10/8C & 4C' END as [Pre.Treat]
                               FROM res2
                               WHERE instr(term,':') < 1
                               ) as C ON 
                               ((R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR
                                ('Z'||R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR 
                                ('Z'||R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]) OR
                                (R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]))
                    WHERE C.estimate is not null
            UNION ALL
            SELECT R.[Genotype], R.[Pre.Treat], R.fit, R.[se.fit], R.[residual.scale]
                    , C.estimate as [estimate]
                    , C.[std.error] as [std.error]
                    , C.[statistic] as [statistic]
                    , C.[p.value] as [p.value] 
                    , 'res3B' as source
                    , 'Jack-4C' as Control
                    FROM results as R 
                    LEFT JOIN (SELECT *
                                ,CASE WHEN instr(term,'Genotype')>0 
                                      THEN substr(term,instr(term,'Genotype')+8,length(term))
                                      ELSE 'Jack' END as Genotype
                                ,CASE WHEN instr(term,'Pre.Treat')>0 
                                      THEN substr(term,instr(term,'Pre.Treat')+9,length(term))
                                      ELSE '4C' END as [Pre.Treat]
                               FROM res3
                               WHERE instr(term,':') < 1
                               ) as C ON 
                               ((R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR
                                ('Z'||R.Genotype = C.Genotype AND R.[Pre.Treat] = C.[Pre.Treat]) OR 
                                ('Z'||R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]) OR
                                (R.Genotype = C.Genotype AND 'Z'||R.[Pre.Treat] = C.[Pre.Treat]))
                    WHERE C.estimate is not null            
                    ) AS AZ
            ORDER BY Genotype, [Pre.Treat]"
SqlResult <- sqldf(sqlcmd)
#View(SqlResult)
dcsv <- data.frame()
dcsv <- bind(dcsv, SqlResult)
#must specify the file directory where csv will be saved
filen <- "E:/Soybean Anther Data/Interaction_Coeficients_Exp3.csv"
write.csv(SqlResult, file = filen, append = FALSE) #append adds to a csv. thus it is set to off as false


#Genotype:D 0D to z0D
Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
mr2 <- glm(PGC ~ (Genotype + D) ^ 2, family = binomial, data = Exp3)
ar2 <- Anova(mr2, type = 2)
smr2 <- summary(mr2)
smr2

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$D == "0D", 4:4] <- c("Z0D")
mrZ3 <- glm(PGC ~ (Genotype + D) ^ 2, family = binomial, data = Exp3_r1)
AnZ3 <- Anova(mrZ, type = 2)
res4 <- tidy(mrZ)
res4

results <- data.frame()
for (geno in unique(Exp3$Genotype)) {
    for (d in unique(Exp3$D)) {
        pr <- predict.glm(mr, list(Genotype = geno, D = d), type = "response", se.fit = T)
        df <- data.frame(Genotype = geno, D = d, fit = pr$fit, se.fit = pr$se.fit, residual.scale = pr$residual.scale, TermName = paste("Genotype", geno, ":D", d, sep = ""))
        results <- rbind(results, df)
    }
}
results

sqlcmd <- "SELECT R.[Genotype], R.[Pre.Treat], R.fit,R.[se.fit], R.[residual.scale]
                    ,C.estimate, C.[std.error], C.[statistic], C.[p.value] 
                    FROM results as R 
                    LEFT JOIN res2 as C ON R.TermName LIKE C.term"
sqldf(sqlcmd)

#Genotype:Sorbitol

Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
mr3 <- glm(PGC ~ (Genotype + Sorbitol) ^ 2, family = binomial, data = Exp3)
ar3 <- Anova(mr3, type = 2)
smr3 <- summary(mr3)
smr3

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Genotype == "Jack", 2:2] <- c("ZJack")
mrZ4 <- glm(PGC ~ (Genotype + Sorbitol) ^ 2, family = binomial, data = Exp3_r1)
AnZ4 <- Anova(mrZ4, type = 2)
res5<- tidy(mrZ4)
res5

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Sorbitol == "0%", 8:8] <- c("Z0%")
mrZ5 <- glm(PGC ~ (Genotype + Sorbitol) ^ 2, family = binomial, data = Exp3_r1)
AnZ5 <- Anova(mrZ5, type = 2)
res6 <- tidy(mrZ5)
res6

#PreTreat:D

Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
mr6 <- glm(PGC ~ (Pre.Treat + D) ^ 2, family = binomial, data = Exp3)
ar6 <- Anova(mr6, type = 2)
smr6 <- summary(mr6)
smr6

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Pre.Treat == "10/8C", 3:3] <- c("Z10/8C")
mrZ6 <- glm(PGC ~ (Pre.Treat + D) ^ 2, family = binomial, data = Exp3_r1)
AnZ6 <- Anova(mrZ6, type = 2)
res7 <- tidy(mrZ6)
res7

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$D == "0D", 4:4] <- c("Z0D")
mrZ7 <- glm(PGC ~ (Pre.Treat + D) ^ 2, family = binomial, data = Exp3_r1)
AnZ7 <- Anova(mrZ7, type = 2)
res8 <- tidy(mrZ7)
res8

#PreTreat:Sorbitol
Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
mr7 <- glm(PGC ~ (Pre.Treat + Sorbitol) ^ 2, family = binomial, data = Exp3)
ar7 <- Anova(mr7, type = 2)
smr7 <- summary(mr7)
smr7

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Pre.Treat == "10/8C", 3:3] <- c("Z10/8C")
mrZ7 <- glm(PGC ~ (Pre.Treat + Sorbitol) ^ 2, family = binomial, data = Exp3_r1)
AnZ7 <- Anova(mrZ7, type = 2)
res8 <- tidy(mrZ7)
res8

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Sorbitol == "0%", 8:8] <- c("Z0%")
mrZ8 <- glm(PGC ~ (Pre.Treat + Sorbitol) ^ 2, family = binomial, data = Exp3_r1)
AnZ8 <- Anova(mrZ8, type = 2)
res9 <- tidy(mrZ8)
res9

#D * Time
Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
mr8 <- glm(PGC ~ (D + Time) ^ 2, family = binomial, data = Exp3)
ar8 <- Anova(mr8, type = 2)
smr8 <- summary(mr8)
smr8

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$D == "0D", 4:4] <- c("Z0D")
mrZ9 <- glm(PGC ~ (D + Time) ^ 2, family = binomial, data = Exp3_r1)
AnZ9 <- Anova(mrZ9, type = 2)
res10 <- tidy(mrZ9)
res10

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Time == "0 days", 5:5] <- c("Z0 days")
mrZ10 <- glm(PGC ~ (D + Time) ^ 2, family = binomial, data = Exp3_r1)
AnZ910<- Anova(mrZ10, type = 2)
res11 <- tidy(mrZ10)
res11

#Time:Sucrose
Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
mr9 <- glm(PGC ~ (Time + Sucrose) ^ 2, family = binomial, data = Exp3)
ar9 <- Anova(mr9, type = 2)
smr9 <- summary(mr9)
smr9

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Time == "0 days", 5:5] <- c("Z0 days")
mrZ11 <- glm(PGC ~ (Time + Sucrose) ^ 2, family = binomial, data = Exp3_r1)
AnZ911 <- Anova(mrZ11, type = 2)
res12 <- tidy(mrZ11)
res12

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Sucrose == "2%", 5:5] <- c("Z2%")
mrZ11 <- glm(PGC ~ (Time + Sucrose) ^ 2, family = binomial, data = Exp3_r1)
AnZ911 <- Anova(mrZ11, type = 2)
res12 <- tidy(mrZ11)
res12

#Genotype:Temp

Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
mr10 <- glm(PGC ~ (Genotype + Temp) ^ 2, family = binomial, data = Exp3)
ar10 <- Anova(mr10, type = 2)
smr10 <- summary(mr10)
smr10

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Genotype == "Jack", 2:2] <- c("ZJack")
mrZ12 <- glm(PGC ~ (Genotype + Temp) ^ 2, family = binomial, data = Exp3_r1)
AnZ12 <- Anova(mrZ12, type = 2)
res13 <- tidy(mrZ12)
res13

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Temp == "11C", 6:6] <- c("Z11C")
mrZ13 <- glm(PGC ~ (Genotype + Temp) ^ 2, family = binomial, data = Exp3_r1)
AnZ13 <- Anova(mrZ13, type = 2)
res14 <- tidy(mrZ13)
res14

#Genotype:Sucrose

Exp3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
mr11 <- glm(PGC ~ (Genotype + Sucrose) ^ 2, family = binomial, data = Exp3)
ar11 <- Anova(mr11, type = 2)
smr11 <- summary(mr11)
smr11

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Genotype == "Jack", 2:2] <- c("ZJack")
mrZ14 <- glm(PGC ~ (Genotype + Sorbitol) ^ 2, family = binomial, data = Exp3_r1)
AnZ14 <- Anova(mrZ14, type = 2)
res15 <- tidy(mrZ14)
res15

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Sucrose == "2%", 5:5] <- c("Z2%")
mrZ15 <- glm(PGC ~ (Time + Sucrose) ^ 2, family = binomial, data = Exp3_r1)
AnZ915 <- Anova(mrZ15, type = 2)
res16 <- tidy(mrZ15)
res16




M3 <- glm(PGC ~ Genotype * Pre.Treat, family = binomial, data = Exp3)
#M3 <- glm(PGC ~ Genotype * Pre.Treat, data = Exp3, contrasts = list(Genotype = "contr.sum"))
#Anova(M3, test = "Chisq")
#M3
A3 <- Anova(M3, type = 3)
#A3
A3v2 <- Anova(M3, type = 2)
#A3v2


#Exp3_1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
#M1 <- glm(PGC ~ (Genotype + Pre.Treat + D + Time + Temp + Sucrose + Sorbitol + Months) ^ 2, family = binomial, data = Exp3_1)
#A1 <- Anova(M1, type = 2)

mr <- glm(PGC ~ (Genotype + Pre.Treat) ^ 2, family = binomial, data = Exp3)
ar <- Anova(mr, type = 2)
smr <- summary(mr)
smr

#mr
res <- tidy(mr)
res

Exp3_r1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",", stringsAsFactors = FALSE)
Exp3_r1[Exp3_r1$Genotype == "Jack", 2:2] <- c("ZJack")
mrZ <- glm(PGC ~ (Genotype + Pre.Treat) ^2, family = binomial, data = Exp3_r1)
AnZ <- Anova(mrZ, type = 2)
res2 <- tidy(mrZ)
res2
res
#summary(mrZ, corr = TRUE)

mrZ <- glm(PGC ~ (Genotype + Pre.Treat) ^ 2, family = binomial, data = Exp3_r1)
tidy(mrZ)

results <- data.frame()
for (geno in unique(Exp3$Genotype)) {
    for (treat in unique(Exp3$Pre.Treat)) {
        pr <- predict.glm(mr, list(Genotype = geno, Pre.Treat = treat), type = "response", se.fit = T)
        df <- data.frame(Genotype = geno, Pre.Treat = treat, fit = pr$fit, se.fit = pr$se.fit, residual.scale = pr$residual.scale, TermName = paste("Genotype", geno, ":Pre.Treat", treat,sep = ""))
        results <- rbind(results, df)
    }
}
results
library(sqldf)
sqlcmd <- "SELECT R.[Genotype], R.[Pre.Treat], R.fit,R.[se.fit], R.[residual.scale]
                    ,C.estimate, C.[std.error], C.[statistic], C.[p.value] 
                    FROM results as R 
                    LEFT JOIN res2 as C ON R.TermName LIKE C.term"
sqldf(sqlcmd)
res2



M1_1 <- glm(PGC ~ Genotype, family = binomial, data = Exp3_1)
Anova(M1_1, test = "Chisq")
summary(M1_1)
Exp3_3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3_Z.csv", header = T, sep = ",")
M1_2 <- glm(PGC ~ Genotype, family = binomial, data = Exp3_3)
Anova(M1_2, test = "Chisq")
summary(M1_2)
Exp3_2 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3_y.csv", header = T, sep = ",")
View(Exp3_2)
M1_3 <- glm(PGC ~ Genotype, family = binomial, data = Exp3_2)
Anova(M1_3, test = "Chisq")
summary(M1_3)
M2 <- glm(PGC ~ Genotype * Pre.Treat * Months, family = binomial, data = Exp3_1)
Anova(M2, test = "Chisq")
M3 <- glm(PGC ~ Genotype * Pre.Treat, family = binomial, data = Exp3)
Anova(M3, test = "Chisq")
summary(M3)
predict(M3, list(Genotype = "Jack", Pre.Treat = "10/8C & 4C"), type = "response", se.fit = T)
predict(M3, list(Genotype = "Jack", Pre.Treat = "4C"), type = "response", se.fit = T)
predict(M3, list(Genotype = "Thorne", Pre.Treat = "10/8C & 4C"), type = "response", se.fit = T)
predict(M3, list(Genotype = "Thorne", Pre.Treat = "4C"), type = "response", se.fit = T)
predict(M3, list(Genotype = "Williams82", Pre.Treat = "10/8C & 4C"), type = "response", se.fit = T)
predict(M3, list(Genotype = "Williams82", Pre.Treat = "4C"), type = "response", se.fit = T)
M4 <- glm(PGC ~ Pre.Treat * D, family = binomial, data = Exp3_1)
Anova(M4, test = "Chisq")
summary(M4)
predict(M4, list(Pre.Treat = "10/8C & 4C", D = "0D"), type = "response", se.fit = T)
predict(M4, list(Pre.Treat = "10/8C & 4C", D = "40D"), type = "response", se.fit = T)
predict(M4, list(Pre.Treat = "4C", D = "0D"), type = "response", se.fit = T)
predict(M4, list(Pre.Treat = "4C", D = "40D"), type = "response", se.fit = T)
M5 <- glm(PGC ~ Genotype * Sorbitol, family = binomial, data = Exp3_1)
Anova(M5, test = "Chisq")
summary(M5)
predict(M5, list(Genotype = "Jack", Sorbitol = "0%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Jack", Sorbitol = "2%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Jack", Sorbitol = "4%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Jack", Sorbitol = "6%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Thorne", Sorbitol = "0%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Thorne", Sorbitol = "2%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Thorne", Sorbitol = "4%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Thorne", Sorbitol = "6%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Williams82", Sorbitol = "0%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Williams82", Sorbitol = "2%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Williams82", Sorbitol = "4%"), type = "response", se.fit = T)
predict(M5, list(Genotype = "Williams82", Sorbitol = "6%"), type = "response", se.fit = T)
M6 <- glm(PGC ~ Pre.Treat * Sorbitol, family = binomial, data = Exp3_1)
Anova(M6, test = "Chisq")
summary(M6)
predict(M6, list(Pre.Treat = "10/8C & 4C", Sorbitol = "0%"), type = "response", se.fit = T)
predict(M6, list(Pre.Treat = "10/8C & 4C", Sorbitol = "2%"), type = "response", se.fit = T)
predict(M6, list(Pre.Treat = "10/8C & 4C", Sorbitol = "4%"), type = "response", se.fit = T)
predict(M6, list(Pre.Treat = "10/8C & 4C", Sorbitol = "6%"), type = "response", se.fit = T)
predict(M6, list(Pre.Treat = "4C", Sorbitol = "0%"), type = "response", se.fit = T)
predict(M6, list(Pre.Treat = "4C", Sorbitol = "2%"), type = "response", se.fit = T)
predict(M6, list(Pre.Treat = "4C", Sorbitol = "4%"), type = "response", se.fit = T)
predict(M6, list(Pre.Treat = "4C", Sorbitol = "6%"), type = "response", se.fit = T)
M7 <- glm(PGC ~ D * Time, family = binomial, data = Exp3_1)
Anova(M7, test = "Chisq")
summary(M7)
predict(M7, list(D = "0D", Time = "0 days"), type = "response", se.fit = T)
predict(M7, list(D = "40D", Time = "0 days"), type = "response", se.fit = T)
predict(M7, list(D = "0D", Time = "3 days"), type = "response", se.fit = T)
predict(M7, list(D = "40D", Time = "3 days"), type = "response", se.fit = T)
M8 <- glm(PGC ~ Time * Sucrose, family = binomial, data = Exp3_1)
Anova(M8, test = "Chisq")
summary(M8)
predict(M8, list(Time = "0 days", Sucrose = "2%"), type = "response", se.fit = T)
predict(M8, list(Time = "0 days", Sucrose = "9%"), type = "response", se.fit = T)
predict(M8, list(Time = "3 days", Sucrose = "2%"), type = "response", se.fit = T)
predict(M8, list(Time = "3 days", Sucrose = "9%"), type = "response", se.fit = T)
M9 <- glm(PGC ~ Genotype * Temp, family = binomial, data = Exp3_1)
Anova(M9, test = "Chisq")
summary(M9)
predict(M9, list(Genotype = "Jack", Temp = "11C"), type = "response", se.fit = T)
predict(M9, list(Genotype = "Jack", Temp = "25C"), type = "response", se.fit = T)
predict(M9, list(Genotype = "Thorne", Temp = "11C"), type = "response", se.fit = T)
predict(M9, list(Genotype = "Thorne", Temp = "25C"), type = "response", se.fit = T)
predict(M9, list(Genotype = "Williams82", Temp = "11C"), type = "response", se.fit = T)
predict(M9, list(Genotype = "Williams82", Temp = "25C"), type = "response", se.fit = T)
M10 <- glm(PGC ~ Genotype * Sucrose, family = binomial, data = Exp3_1)
Anova(M10, test = "Chisq")
summary(M10)
predict(M10, list(Genotype = "Jack", Sucrose = "2%"), type = "response", se.fit = T)
predict(M10, list(Genotype = "Jack", Sucrose = "9%"), type = "response", se.fit = T)
predict(M10, list(Genotype = "Thorne", Sucrose = "2%"), type = "response", se.fit = T)
predict(M10, list(Genotype = "Thorne", Sucrose = "9%"), type = "response", se.fit = T)
predict(M10, list(Genotype = "Williams82", Sucrose = "2%"), type = "response", se.fit = T)
predict(M10, list(Genotype = "Williams82", Sucrose = "9%"), type = "response", se.fit = T)

Exp3_1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3.csv", header = T, sep = ",")
M11 <- glm(PGC ~ Genotype * Pre.Treat + Pre.Treat * D + Genotype * Sorbitol + D * Time + Pre.Treat * Sorbitol + Time * Sucrose + Genotype * Temp + Genotype * Sucrose, family = binomial, data = Exp3_1)
Anova(M11, test = "Chisq")
summary(M11)
M3 <- glm(PGC ~ Genotype * Pre.Treat, family = binomial, data = Exp3_1)
Anova(M3, test = "Chisq")
summary(M3)
M4 <- glm(PGC ~ Pre.Treat * D, family = binomial, data = Exp3_1)
Anova(M4, test = "Chisq")
summary(M4)
M5 <- glm(PGC ~ Genotype * Sorbitol, family = binomial, data = Exp3_1)
Anova(M5, test = "Chisq")
summary(M5)
M6 <- glm(PGC ~ Pre.Treat * Sorbitol, family = binomial, data = Exp3_1)
Anova(M6, test = "Chisq")
summary(M6)
M7 <- glm(PGC ~ D * Time, family = binomial, data = Exp3_1)
Anova(M7, test = "Chisq")
summary(M7)
M8 <- glm(PGC ~ Time * Sucrose, family = binomial, data = Exp3_1)
Anova(M8, test = "Chisq")
summary(M8)
M9 <- glm(PGC ~ Genotype * Temp, family = binomial, data = Exp3_1)
Anova(M9, test = "Chisq")
summary(M9)
M10 <- glm(PGC ~ Genotype * Sucrose, family = binomial, data = Exp3_1)
Anova(M10, test = "Chisq")
summary(M10)

Exp3_2 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3_y.csv", header = T, sep = ",")
M11a <- glm(PGC ~ Genotype * Pre.Treat + Pre.Treat * D + Genotype * Sorbitol + D * Time + Pre.Treat * Sorbitol + Time * Sucrose + Genotype * Temp + Genotype * Sucrose, family = binomial, data = Exp3_2)
Anova(M11a, test = "Chisq")
summary(M11a)
M3a <- glm(PGC ~ Genotype * Pre.Treat, family = binomial, data = Exp3_2)
Anova(M3a, test = "Chisq")
summary(M3a)
M4a <- glm(PGC ~ Pre.Treat * D, family = binomial, data = Exp3_2)
Anova(M4a, test = "Chisq")
summary(M4a)
M5a <- glm(PGC ~ Genotype * Sorbitol, family = binomial, data = Exp3_2)
Anova(M5a, test = "Chisq")
summary(M5a)
M6a <- glm(PGC ~ Pre.Treat * Sorbitol, family = binomial, data = Exp3_2)
Anova(M6a, test = "Chisq")
summary(M6a)
M7a <- glm(PGC ~ D * Time, family = binomial, data = Exp3_2)
Anova(M7a, test = "Chisq")
summary(Ma7)
M8a <- glm(PGC ~ Time * Sucrose, family = binomial, data = Exp3_2)
Anova(M8a, test = "Chisq")
summary(M8a)
M9a <- glm(PGC ~ Genotype * Temp, family = binomial, data = Exp3_2)
Anova(M9a, test = "Chisq")
summary(M9a)
M10a <- glm(PGC ~ Genotype * Sucrose, family = binomial, data = Exp3_2)
Anova(M10a, test = "Chisq")
summary(M10a)

Exp3_3 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3_Z.csv", header = T, sep = ",")
M11b <- glm(PGC ~ Genotype * Pre.Treat + Pre.Treat * D + Genotype * Sorbitol + D * Time + Pre.Treat * Sorbitol + Time * Sucrose + Genotype * Temp + Genotype * Sucrose, family = binomial, data = Exp3_3)
Anova(M11b, test = "Chisq")
summary(M11b)
M3b <- glm(PGC ~ Genotype * Pre.Treat, family = binomial, data = Exp3_3)
Anova(M3b, test = "Chisq")
summary(M3b)
M4b <- glm(PGC ~ Pre.Treat * D, family = binomial, data = Exp3_3)
Anova(M4b, test = "Chisq")
summary(M4b)
M5b <- glm(PGC ~ Genotype * Sorbitol, family = binomial, data = Exp3_3)
Anova(M5b, test = "Chisq")
summary(M5b)
M6b <- glm(PGC ~ Pre.Treat * Sorbitol, family = binomial, data = Exp3_3)
Anova(M6b, test = "Chisq")
summary(M6b)
M7b <- glm(PGC ~ D * Time, family = binomial, data = Exp3_3)
Anova(M7b, test = "Chisq")
summary(M7b)
M8 <- glm(PGC ~ Time * Sucrose, family = binomial, data = Exp3_3)
Anova(M8, test = "Chisq")
summary(M8)
M9b <- glm(PGC ~ Genotype * Temp, family = binomial, data = Exp3_3)
Anova(M9b, test = "Chisq")
summary(M9b)
M10b <- glm(PGC ~ Genotype * Sucrose, family = binomial, data = Exp3_3)
Anova(M10b, test = "Chisq")
summary(M10b)

Exp3_4 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3_x.csv", header = T, sep = ",")
M11c <- glm(PGC ~ Genotype * Pre.Treat + Pre.Treat * D + Genotype * Sorbitol + D * Time + Pre.Treat * Sorbitol + Time * Sucrose + Genotype * Temp + Genotype * Sucrose, family = binomial, data = Exp3_3)
Anova(M11c, test = "Chisq")
summary(M11c)
M3c <- glm(PGC ~ Genotype * Pre.Treat, family = binomial, data = Exp3_4)
Anova(M3c, test = "Chisq")
summary(M3c)
M4c <- glm(PGC ~ Pre.Treat * D, family = binomial, data = Exp3_4)
Anova(M4c, test = "Chisq")
summary(M4c)
M5c <- glm(PGC ~ Genotype * Sorbitol, family = binomial, data = Exp3_4)
Anova(M5c, test = "Chisq")
summary(M5)
M6c <- glm(PGC ~ Pre.Treat * Sorbitol, family = binomial, data = Exp3_4)
Anova(M6c, test = "Chisq")
summary(M6c)
M7c <- glm(PGC ~ D * Time, family = binomial, data = Exp3_4)
Anova(M7c, test = "Chisq")
summary(M7c)
M8c <- glm(PGC ~ Time * Sucrose, family = binomial, data = Exp3_4)
Anova(M8c, test = "Chisq")
summary(M8c)
M9c <- glm(PGC ~ Genotype * Temp, family = binomial, data = Exp3_4)
Anova(M9c, test = "Chisq")
summary(M9c)
M10c <- glm(PGC ~ Genotype * Sucrose, family = binomial, data = Exp3_4)
Anova(M10c, test = "Chisq")
summary(M10c)

Exp3_5 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3_m.csv", header = T, sep = ",")
M12 <- glm(PGC ~ Genotype * Pre.Treat, family = binomial, data = Exp3_5)
Anova(M12, test = "Chisq")
summary(M12)
Exp3_6 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3_mz.csv", header = T, sep = ",")
M12a <- glm(PGC ~ Genotype * Pre.Treat, family = binomial, data = Exp3_6)
Anova(M12a, test = "Chisq")
summary(M12a)

Exp3_6 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp3_x2.csv", header = T, sep = ",")
M13 <- glm(PGC ~ Sorbitol * Pre.Treat, family = binomial, data = Exp3_6)
Anova(M13, test = "Chisq")
summary(M13)

#Contrasts
options(contrasts = rep("contr.sum", 2))
contr.sum(3)
contr.sum(2)
glm(PGC ~ Genotype + Pre.Treat + D + Time + Temp + Sucrose + Sorbitol, data = Exp3)
a <- factor(c("Jack", "Thorne", "Williams82"))
a
a <- factor(as.character(a), levels = c("Williams82", "Jack", "Thorne"))
a
glm(PGC ~ Genotype + Pre.Treat + D + Time + Temp + Sucrose + Sorbitol, data = Exp3_1)
contr.poly(3)

sapply(Exp3, levels)
options(contrasts = rep("contr.treatment", 2))
glm(PGC ~ Genotype + Pre.Treat + D + Time + Temp + Sucrose + Sorbitol, data = Exp3)


#Experiment 4 ----
#BNN vs BN
#0D vs 40D vs Moraes

Exp4 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp4.csv", header = T, sep = ",")
View(Exp4)
Exp4 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp4.csv", header = T, sep = ",")
Exp4a <- glm(PGC ~ (Pre.Treat + D + Nitrogen) ^ 2, family = binomial, data = Exp4)
Anova(Exp4a, test = "Chisq")
summary(Exp4a)
Exp4a1 <- glm(PGC ~ Pre.Treat * Nitrogen, family = binomial, data = Exp4)
Anova(Exp4a1, test = "Chisq")
summary(Exp4a1)

Exp4 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp4_z.csv", header = T, sep = ",")
Exp4a1 <- glm(PGC ~ Pre.Treat * D * Nitrogen + Months, family = binomial, data = Exp4)
Anova(Exp4a1, test = "Chisq")
summary(Exp4a1)

#Experiment 5 ----
#0D vs 40D
#pH 4, 5.8,Regular,7 and 9
#Pre.Treat 10/8 and 10/8+4

Exp5 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp5.csv", header = T, sep = ",")
View(Exp5)
Exp5 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp5.csv", header = T, sep = ",")
Exp5a <- glm(PGC ~ (Pre.Treat + D + pH) ^ 2, family = binomial, data = Exp5)
Anova(Exp5a, test = "Chisq")
summary(Exp5a)
Exp5a1 <- glm(PGC ~ D * pH, family = binomial, data = Exp5)
Anova(Exp5a1, test = "Chisq")
summary(Exp5a1)
Exp5a2 <- glm(PGC ~ D * Pre.Treat, family = binomial, data = Exp5)
Anova(Exp5a2, test = "Chisq")
summary(Exp5a2)

Exp5_1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp5_z.csv", header = T, sep = ",")
View(Exp5_1)
Exp5_1 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp5_z.csv", header = T, sep = ",")
Exp5b <- glm(PGC ~ (Pre.Treat + D + pH) ^ 2, family = binomial, data = Exp5_1)
Anova(Exp5b, test = "Chisq")
summary(Exp5b)
Exp5b1 <- glm(PGC ~ D * pH, family = binomial, data = Exp5_1)
Anova(Exp5b1, test = "Chisq")
summary(Exp5b1)
Exp5b2 <- glm(PGC ~ pH * Pre.Treat, family = binomial, data = Exp5_1)
Anova(Exp5b2, test = "Chisq")
summary(Exp5b2)

#Experiment 6 ----
#Sucrose 2vs 9
#Sorbitol 0 vs 2
#0D, 10D vs Moraes
#Young vs Long
#PreTreatment 10/8+4C
#Nitrogen: N vs NN

Exp6 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp6.csv", header = T, sep = ",")
View(Exp6)
Exp6 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp6.csv", header = T, sep = ",")
Exp6a <- glm(PGC ~ (D + Nitrogen + Sucrose + Sorbitol + Yeung + Long) ^ 2, family = binomial, data = Exp6)
Anova(Exp6a, test = "Chisq")
summary(Exp6a)
Exp6a1 <- glm(PGC ~ D * Yeung, family = binomial, data = Exp6)
Anova(Exp6a1, test = "Chisq")
summary(Exp6a1)

predict(Exp6a, list(D = "10D", Nitrogen = "NN"), type = "response", se.fit = T)



#Experiment 7 ----
#Nitrogen
#D
#Sorbitol

Exp7 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp7.csv", header = T, sep = ",")
Exp7a <- glm(PGC ~ (D + Nitrogen + Sorbitol) ^ 2, family = binomial, data = Exp7)
Anova(Exp7a, test = "Chisq")
summary(Exp7a)

#Experiment 8 ----
#D
#Silver Nitrate

Exp8 <- read.table("E:/Soybean Anther Data/R-Analysis/Exp8.csv", header = T, sep = ",")
Exp8a <- glm(PGC ~ (D + Silver) ^ 2, family = binomial, data = Exp8)
Anova(Exp8a, test = "Chisq")
summary(Exp8a)
