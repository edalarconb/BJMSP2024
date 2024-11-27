rm(list=ls())
library("plot3D")
library(readxl)
library(dplyr)
library(rio)
library(ggplot2)
library(ggpattern)
library(tidyverse)
library(lubridate)
require(nnet)
library(ggpubr)
library(sampleSelection)
library(data.table)


### Organizing the Database for Analysis
dataBritish=import("DataBritish.xlsx")
carreras=names(table(dataBritish$nom_carrera))
# [1]"BIOLOGÕA" [2]"BIOLOGÕA MARINA" [3]"BIOQUÕMICA"

dataBritish.unique = dataBritish %>%
  mutate(
    bioM = ifelse(nom_carrera == "BIOLOGÕA MARINA", 1, 0),
    biol = ifelse(nom_carrera == "BIOLOGÕA", 1, 0),
    bioQ = ifelse(nom_carrera == "BIOQUÕMICA", 1, 0)
  ) %>%
  
  group_by(id) %>%
  summarise(
    
    bioM = ifelse(any(bioM == 1), 1, 0),
    biol = ifelse(any(biol == 1), 1, 0),
    bioQ = ifelse(any(bioQ == 1), 1, 0),
    
    matem = if (all(is.na(matem))) {
      NA
    } else {
      max(matem, na.rm = TRUE)
    },
    LyC = if (all(is.na(`leng y Com`))) {
      NA
    } else {
      max(`leng y Com`, na.rm = TRUE)
    },
    Ciencias = if (all(is.na(ciencias))) {
      NA
    } else {
      max(ciencias, na.rm = TRUE)
    },
    NEM = if (all(is.na(`ptje nem`))) {
      NA
    } else {
      max(`ptje nem`, na.rm = TRUE)
    },
    Ranking = if (all(is.na(ranking))) {
      NA
    } else {
      max(ranking, na.rm = TRUE)
    },
    GPA = if (all(is.na(PGA_1ERsemestre))) {
      NA
    } else {
      max(PGA_1ERsemestre, na.rm = TRUE)
    },
    nom_carrera = if (any(!is.na(PGA_1ERsemestre))) {
      first(nom_carrera[which.max(PGA_1ERsemestre)])
    } else {
      ""
    }
  ) %>%
  ungroup() %>% mutate(
    Z=case_when(
      bioM == 1 & biol == 0 & bioQ == 0 ~ 1,
      bioM == 0 & biol == 1 & bioQ == 0 ~ 2,
      bioM == 0 & biol == 0 & bioQ == 1 ~ 3,
      bioM == 1 & biol == 1 & bioQ == 0 ~ 4,
      bioM == 1 & biol == 0 & bioQ == 1 ~ 5,
      bioM == 0 & biol == 1 & bioQ == 1 ~ 6,
      TRUE ~ 7
    ),
    S=case_when(
      nom_carrera == "BIOLOGÕA MARINA" ~ "MB",
      nom_carrera == "BIOLOGÕA" ~ "B",
      nom_carrera == "BIOQUÕMICA" ~ "BC",
      TRUE ~ "NE"
    )) 

data.Ready=dataBritish.unique %>% mutate(
  nom_carrera = case_when(
    S=="NE" ~ NA,
    TRUE ~ nom_carrera
  )
) %>% mutate(Z0bs=case_when(S!="NE" ~ 1,
                            TRUE ~ 0))

sample.space=table(data.Ready$nom_carrera, useNA = "always")
sum(sample.space)
round(prop.table(table(data.Ready$nom_carrera, useNA = "always")),3)
round(prop.table(table(data.Ready$nom_carrera)),3)

dataObs=data.Ready %>% filter(Z0bs==1)

## Using X=Math test score
#E(Y|X,Z=1)
mean.gpaP1=lm(GPA~matem,
              data=dataObs)

#P(Z=1|X)
LogitZ=multinom(as.factor(Z0bs)~matem,
                data=data.Ready)

data.Ready=data.Ready %>% mutate(fittedZ=LogitZ$fitted.values)

pje.mate=data.frame(matem=dataObs %>% 
                      select(matem))

predGPA=predict(mean.gpaP1,newdata = pje.mate) #estimated E(Y|X,Z=1)

prob.Z=data.Ready %>% 
  filter(Z0bs==1) %>% select(fittedZ)
pje.mate=dataObs %>% select(matem)
GPA=dataObs %>% select(GPA)
### dataframe containing
## observed math score
## observed GPA
## predicted GPA
## P(Z=1|X)
## Lower bound eq. 3.3 using y0=1.0 and y1=7.0
## Upper bound eq. 3.3 using y0=1.0 and y1=7.0
data.fullP1=data.frame(pje.mate=pje.mate$matem,
                       GPA=GPA$GPA,
                       pred.GPA=predGPA,
                       prob.Z=prob.Z$fittedZ) %>% 
  arrange(pje.mate) %>% 
  mutate(LB_p11=pred.GPA*prob.Z+(1-prob.Z),
         UB_p11=pred.GPA*prob.Z+7*(1-prob.Z))

## WIDER BOUNDS
# Fig 3.1a
library(grid)
ggplot()+
  geom_point(data=data.fullP1,
             aes(x=pje.mate,
                 y=GPA,
                 shape="Observed GPAs")
  )+
  ylim(1,7)+
  ylab("GPA")+xlab("Mathematics test score")+
  geom_line(data=data.fullP1,
            aes(x=pje.mate,
                y=pred.GPA,
                color="Predicted GPAs in selected applicants",
                linetype="Predicted GPAs in selected applicants"),
            size=1.1)+
  geom_line(data=data.fullP1,
            aes(x=pje.mate,
                y=LB_p11,
                color="Identificaction bounds under WIA",
                linetype = "Identificaction bounds under WIA"
            ),
            size=1.1)+
  geom_line(data=data.fullP1,
            aes(x=pje.mate,
                y=UB_p11,
                color="Identificaction bounds under WIA",
                linetype = "Identificaction bounds under WIA"),
            size=1.1)+
  geom_ribbon(data=data.fullP1,
              aes(x=pje.mate,
                  ymin = LB_p11, ymax = UB_p11), 
              fill = "#2ca25f", alpha = 0.1)+
  scale_shape(name = "",
              breaks="Observed GPAs")+
  scale_color_manual(name='',
                     breaks=c('Identificaction bounds under WIA', 
                              'Predicted GPAs in selected applicants', 
                              'Lower bound for predicted GPAs'
                     ),
                     values=c('Identificaction bounds under WIA'='#2ca25f', 
                              'Predicted GPAs in selected applicants'='#084594', 
                              'Identificaction bounds under WIA'='#2ca25f')
  )+
  scale_linetype_manual(name='',
                        breaks=c('Identificaction bounds under WIA', 
                                 'Predicted GPAs in selected applicants', 
                                 'Identificaction bounds under WIA'
                        ),
                        values=c('Identificaction bounds under WIA'='solid', 
                                 'Predicted GPAs in selected applicants'='solid', 
                                 'Identificaction bounds under WIA'='solid'))+
  theme_bw()+
  theme(
    legend.direction = "vertical", 
    legend.position = c(0.75, 0.2),
    legend.box="vertical",
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "transparent", colour = "transparent"))


# width of the bounds univariate Math score c(550,650,820)
vec.bounds=c(550,650,820)
probZ0.X=(1-predict(LogitZ,newdata=list(matem=vec.bounds),"probs"))
y0=1;y1=7
data.frame(
  matem=vec.bounds,
  width=round((y1-y0)*probZ0.X,3))

#### with X1 and X2
fit=lm(GPA~matem+LyC,
       data=dataObs)
prob=glm(Z0bs ~ matem + LyC,
         data = data.Ready,
         family = "binomial")
grid.lines = 20

matem <- seq(min(dataObs$matem), max(dataObs$matem), length.out = grid.lines)
LyC <- seq(min(dataObs$LyC), max(dataObs$LyC), length.out = grid.lines)
xy <- expand.grid( x = matem, y = LyC)

z.pred <- matrix(predict(fit, newdata = list(matem = xy$x, LyC=xy$y)), 
                 nrow = grid.lines, ncol = grid.lines)

pred.probabilities=data.frame(matem = xy$x, LyC=xy$y)
colnames(pred.probabilities)=c("matem", "LyC")
probs.pred= matrix(predict(prob, newdata = pred.probabilities, type="response"), 
                   nrow = grid.lines, ncol = grid.lines)

LB=z.pred*probs.pred+1*(1-probs.pred)
UB=z.pred*probs.pred+7*(1-probs.pred)

## Bound Width for table 3.1
m=c(rep(550,3),rep(650,3),rep(820,3))
l=c(rep(c(520,650,800),3))
data.predict=data.frame(a=m,b=l)
colnames(data.predict)=c("matem","LyC")
probZ0.x1x2=1-predict(prob, newdata =data.predict, type="response")

## Table 3.1
data.frame(X1=m,
           X2=l,
           BoundWidth=round((y1-y0)*probZ0.x1x2,3))


## Fig 3.1b
par(mar = c(5, 4, 4, 2) - 1)
scatter3D(dataObs$matem, dataObs$LyC, dataObs$GPA,
          pch = 19, cex = 0.5, colvar = NULL, col = "black",
          theta = 20, phi = 20, bty = "b", zlim = c(1, 7),
          xlab = "Math score", ylab = "Language score", zlab = "GPA",  
          surf = list(x = matem, y = LyC, z = z.pred,  
                      facets = TRUE,
                      col = ramp.col(col = c("#084594", "#084594"), alpha = 0.7)), 
          main = "Predicted GPA"
)

par(mar = c(5, 4, 4, 2) + 0.1)
persp3D(x = matem, y = LyC, z = LB,  
        colvar = NULL, add = TRUE, col = "#2ca25f", alpha = 0.5)

persp3D(x = matem, y = LyC, z = UB,  
        add = TRUE, colvar = NULL, col = "#2ca25f", alpha = 0.5)

legend("bottomleft", 
       legend=c("Identification bounds under WIA",
                "Predicted GPAs in selected",
                "applicants"), 
       col=c("#2ca25f","#084594","white"), pch=15,
       cex=0.8,bg = "transparent")


### Section 3.2 Using additional information from the selection process
data.biol.mat=dataObs
names(data.biol.mat)

## Multinomial regression
### P(G=g|X)
multinom.carreras=multinom(as.factor(S)~matem,
                           data=data.biol.mat)

### predicted values for P(G=g|X) 
fitted.carreras=multinom.carreras$fitted.values

## Database by program
biologia=data.biol.mat %>% filter(S=="B")
biologia.M=data.biol.mat %>% filter(S=="MB")
bioquimica=data.biol.mat %>% filter(S=="BC")

## linear regression by program
lm.biologia.M=lm(GPA~matem,
                 data=biologia.M) ## #E(Y|X,G=1)
lm.biologia=lm(GPA~matem,
               data=biologia) ## #E(Y|X,G=2)
lm.bioquimica=lm(GPA~matem,
                 data=bioquimica) ## #E(Y|X,G=3)

pje.mate=data.frame(matem=data.biol.mat$matem)

pred.biologia.M=predict(lm.biologia.M,newdata = pje.mate) #estimated E(Y|X,G=1)
pred.biologia=predict(lm.biologia,newdata = pje.mate) #estimated E(Y|X,G=2)
pred.bioquimica=predict(lm.bioquimica,newdata = pje.mate) #estimated E(Y|X,G=3)


## dataframe containing:
# observed Math score
# predicted GPA in Biology
# predicted GPA in Marine Biology
# predicted GPA in Biochemistry
# P(G=Biology|X)
# P(G=Marine Biology|X)
# P(G=Biochemestry|X)
# law of iterated expectation for the observed values, i.e.,
# E(Y|X,Z=1)=E(Y|X,Z=1,G=1)P(G=1|X) + E(Y|X,Z=1,G=2)P(G=2|X) + E(Y|X,Z=1,G=3)P(G=3|X)

data.full=data.frame(pje.mate=pje.mate[,1],
                     pred.biol=pred.biologia,
                     pred.biol.M=pred.biologia.M,
                     pred.bioq=pred.bioquimica,
                     prob.biol=fitted.carreras[,"B"],
                     prob.biol.M=fitted.carreras[,"MB"],
                     prob.bioq=fitted.carreras[,"BC"],
                     ltp=pred.biologia*fitted.carreras[,"B"]+
                       pred.biologia.M*fitted.carreras[,"MB"]+
                       pred.bioquimica*fitted.carreras[,"BC"]) %>% 
  arrange(pje.mate)
head(fitted.carreras)
data.full.obs=data.full

## P(Z=1|X)
pz1=multinom(as.factor(Z0bs)~matem,
             data=data.Ready)

## Data in Z=0
data.Z0=data.Ready %>% filter(Z0bs==0)

#P(U=u|X,Z=0)
# In the database we have that:
# Z=1 --> u=(1,0,0)
# Z=2 --> u=(0,1,0)
# Z=3 --> u=(0,0,1)
# Z=4 --> u=(1,1,0)
# Z=5 --> u=(1,0,1)
# Z=6 --> u=(0,1,1)
# Z=7 --> u=(1,1,1)
pS=multinom(as.factor(Z)~matem,
            data=data.Z0)

#grid for Math score
ptje_mate=data.frame(matem=seq(min(data.full$pje.mate),
                               max(data.full$pje.mate),
                               length=1000))

## observed min and max for each program

min_max.gpa=data.biol.mat %>% group_by(nom_carrera) %>% 
  summarise(min.gpa=min(GPA),
            max.gpa=max(GPA))

## dataframe containing the following information
## EG1 --> E(Y|X,G=1)
## EG2 --> E(Y|X,G=2)
## EG3 --> E(Y|X,G=3)
## PG.BIOLOGIA --> P(G=BIOLOGIA|X)
## PG.BIOLOGIA.MARINA --> P(G=BIOLOGIA.MARINA|X)
## PG.BIOQUIMICA --> P(G=BIOQUIMICA|X)
## pz1=P(Z=1|X)
## pS..=P(U=u|X,G=0)
## B1 Lower bound WIA (Eq 3.3) - Lower Bound PSA (Eq 3.7)
## B2 Upper bound PSA (Eq 3.7) - Lower Bound FSA (Eq 3.9)
## B3 Upper bound FSA (Eq 3.9) - Lower Bound WSA (Eq 3.8)
## B4 Upper bound WIA (Eq 3.3) - Upper Bound WSA (Eq 3.8)

# Thus we have that the Bound widths are given by
## B4-B1 = bound width for WIA
## B2-B1 = bound width for PSA
## B4-B3 = bound width for WSA
## B3-B2 = bound width for FSA

data.bounds=data.frame(
  Mate=ptje_mate,
  EG1=predict(lm.biologia.M,newdata=ptje_mate),
  EG2=predict(lm.biologia,newdata=ptje_mate),
  EG3=predict(lm.bioquimica,newdata=ptje_mate),
  pG=predict(multinom.carreras,newdata=ptje_mate,type="prob"),
  pz1=predict(pz1,newdata=ptje_mate,probs,type="prob"),
  pS=predict(pS,newdata=ptje_mate,probs,type="prob"))  %>% 
  mutate(B1=(EG1*pG.MB+
               EG2*pG.B+
               EG3*pG.BC)*pz1+(1-pz1),
         B2=(EG1*pG.MB+
               EG2*pG.B+
               EG3*pG.BC)*pz1+
           (min_max.gpa$min.gpa[2]*(pS.1+pS.4+0+pS.7)+
              min_max.gpa$min.gpa[1]*(pS.2+pS.6)+
              min_max.gpa$min.gpa[3]*pS.3)*(1-pz1),
         B3=(EG1*pG.MB+
                   EG2*pG.B+
                   EG3*pG.BC)*pz1+
           (EG1*(pS.1+pS.4+0+pS.7)+
              EG2*(pS.2+pS.6)+
              EG3*pS.3)*(1-pz1),
         B4=(EG1*pG.MB+
                EG2*pG.B+
                EG3*pG.BC)*pz1+7*(1-pz1)
  )


pcH=data.biol.mat$nom_carrera
pcH[which(pcH=="BIOLOGÕA")]="Biology"
pcH[which(pcH=="BIOLOGÕA MARINA")]="Marine Biology"
pcH[which(pcH=="BIOQUÕMICA")]="Biochemistry"

data.biol.mat=data.biol.mat %>% 
  mutate(Program=pcH)
data.pinta=data.frame(x=data.bounds$matem,
                      lower=data.bounds$B1,upper=data.bounds$B2)

## PSA (Perfect Selection Assumption) Figure 3.2a
ggplot()+
  geom_point(data=data.biol.mat,
             aes(x=matem,
                 y=GPA,shape=Program)
  )+
  ylim(1,7)+
  ylab("GPA")+xlab("Mathematics test score")+
  geom_line(data=data.full.obs,
            aes(x=pje.mate,
                y=ltp,
                color="Predicted GPAs in selected applicants",
                linetype="Predicted GPAs in selected applicants"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B1,
                color="Identification bounds under PSA",
                linetype = "Identification bounds under PSA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B4,
                color="Upper bound under WIA",
                linetype = "Upper bound under WIA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B2,
                color="Identification bounds under PSA",
                linetype = "Identification bounds under PSA"),
            size=1.1)+  
  geom_ribbon(data=data.bounds,
              aes(x=matem,ymin = B1, ymax = B2), fill = "#d95f02", alpha = 0.1)+
  scale_color_manual(name='',
                     breaks=c('Upper bound under WIA', 
                              'Identification bounds under PSA',
                              'Predicted GPAs in selected applicants', 
                              'Identification bounds under PSA'),
                     values=c('Identification bounds under PSA'='#d95f02',
                              'Upper bound under WIA'='#2ca25f', 
                              'Predicted GPAs in selected applicants'='#084594', 
                              'Identification bounds under PSA'='#d95f02'))+
  scale_linetype_manual(name='',
                        breaks=c('Upper bound under WIA', 
                                 'Identification bounds under PSA',
                                 'Predicted GPAs in selected applicants', 
                                 'Identification bounds under PSA'),
                        values=c('Upper bound under WIA'='solid',
                                 'Identification bounds under PSA'='solid',
                                 'Predicted GPAs in selected applicants'='solid', 
                                 'Identification bounds under PSA'='solid'))+
  theme_bw()+
  theme(
    legend.direction = "vertical", 
    legend.position = c(0.78, 0.25),
    legend.box="vertical",
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "transparent", colour = "transparent"))



########### WSA (Worst Selection Assumption) Figure 3.3a
ggplot()+
  geom_point(data=data.biol.mat,
             aes(x=matem,
                 y=GPA,shape=Program)
  )+
  ylim(1,7)+
  ylab("GPA")+xlab("Mathematics test score")+
  geom_line(data=data.full.obs,
            aes(x=pje.mate,
                y=ltp,
                color="Predicted GPAs in selected applicants",
                linetype="Predicted GPAs in selected applicants"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B3,
                color="Identification bounds under WSA",
                linetype = "Identification bounds under WSA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B4,
                color="Identification bounds under WSA",
                linetype = "Identification bounds under WSA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B1,
                color="Lower bound under WIA",
                linetype = "Lower bound under WIA"),
            size=1.1)+ 
  geom_ribbon(data=data.bounds,
              aes(x=matem,ymin = B3, ymax = B4), fill = "#d95f02", alpha = 0.1)+
  scale_color_manual(name='',
                     breaks=c('Lower bound under WIA', 
                              'Identification bounds under WSA',
                              'Predicted GPAs in selected applicants', 
                              'Identification bounds under WSA'),
                     values=c('Identification bounds under WSA'='#d95f02',
                              'Lower bound under WIA'='#2ca25f', 
                              'Predicted GPAs in selected applicants'='#084594', 
                              'Identification bounds under WSA'='#d95f02'))+
  scale_linetype_manual(name='',
                        breaks=c('Lower bound under WIA', 
                                 'Identification bounds under WSA',
                                 'Predicted GPAs in selected applicants', 
                                 'Identification bounds under WSA'),
                        values=c('Lower bound under WIA'='solid',
                                 'Identification bounds under WSA'='solid',
                                 'Predicted GPAs in selected applicants'='solid', 
                                 'Identification bounds under WSA'='solid'))+
  theme_bw()+
  theme(
    legend.direction = "vertical", 
    legend.position = c(0.78, 0.25),
    legend.box="vertical",
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "transparent", colour = "transparent"))

##### FSA (Fallible Selection Assumption) Figure 3.4a
ggplot()+
  geom_point(data=data.biol.mat,
             aes(x=matem,
                 y=GPA,shape=Program)
  )+
  ylim(1,7)+
  ylab("GPA")+xlab("Mathematics test score")+
  geom_line(data=data.full.obs,
            aes(x=pje.mate,
                y=ltp,
                color="Predicted GPAs in selected applicants",
                linetype="Predicted GPAs in selected applicants"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B1,
                color="Identification bounds under WIA",
                linetype = "Identification bounds under WIA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B4,
                color="Identification bounds under WIA",
                linetype = "Identification bounds under WIA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B3,
                color="Identification bounds under FSA",
                linetype = "Identification bounds under FSA"),
            size=1.1)+
  geom_line(data=data.bounds,
            aes(x=matem,
                y=B2,
                color="Identification bounds under FSA",
                linetype = "Identification bounds under FSA"),
            size=1.1)+
  geom_ribbon(data=data.bounds,
              aes(x=matem,ymin = B2, ymax = B3), fill = "#d95f02", alpha = 0.1)+
  scale_color_manual(name='',
                     breaks=c('Identification bounds under WIA', 
                              'Identification bounds under FSA',
                              'Predicted GPAs in selected applicants', 
                              "Identification bounds under FSA",
                              'Identification bounds under WIA'),
                     values=c('Identification bounds under FSA'='#d95f02',
                              'Identification bounds under WIA'='#2ca25f', 
                              'Predicted GPAs in selected applicants'='#084594',
                              "Identification bounds under FSA"="#d95f02",
                              'Identification bounds under WIA'='#2ca25f'))+
  scale_linetype_manual(name='',
                        breaks=c('Identification bounds under WIA', 
                                 'Identification bounds under FSA',
                                 'Predicted GPAs in selected applicants', 
                                 "Identification bounds under FSA",
                                 'Identification bounds under WIA'),
                        values=c('Identification bounds under WIA'='solid',
                                 'Identification bounds under FSA'='solid',
                                 'Predicted GPAs in selected applicants'='solid',
                                 "Identification bounds under FSA"="solid",
                                 'Identification bounds under WIA'='solid'))+
  theme_bw()+
  theme(
    legend.direction = "vertical", 
    legend.position = c(0.78, 0.25),
    legend.box="vertical",
    legend.key = element_blank(), 
    legend.background = element_rect(fill = "transparent", colour = "transparent"))

#### Information for Tables 3.2, 3.4 and 3.6
ptje_mate=data.frame(matem=c(550,650,820))
EG1=predict(lm.biologia.M,newdata=ptje_mate)
EG2=predict(lm.biologia,newdata=ptje_mate)
EG3=predict(lm.bioquimica,newdata=ptje_mate)
pG=predict(multinom.carreras,newdata=ptje_mate,type="prob")
pz1.=predict(pz1,newdata=ptje_mate,probs,type="prob")
pS.=predict(pS,newdata=ptje_mate,probs,type="prob")
B1=(EG1*pG[,"MB"]+
       EG2*pG[,"B"]+
       EG3*pG[,"BC"])*pz1.+(1-pz1.)
B2=(EG1*pG[,"MB"]+
      EG2*pG[,"B"]+
      EG3*pG[,"BC"])*pz1.+
  (min_max.gpa$min.gpa[2]*(pS.[,"1"]+pS.[,"4"]+0+pS.[,"7"])+
     min_max.gpa$min.gpa[1]*(pS.[,"2"]+pS.[,"6"])+
     min_max.gpa$min.gpa[3]*pS.[,"3"])*(1-pz1.)
B3=(EG1*pG[,"MB"]+
      EG2*pG[,"B"]+
      EG3*pG[,"BC"])*pz1.+
  (EG1*(pS.[,"1"]+pS.[,"4"]+0+pS.[,"7"])+
     EG2*(pS.[,"2"]+pS.[,"6"])+
     EG3*pS.[,"3"])*(1-pz1.)
B4=(EG1*pG[,"MB"]+
      EG2*pG[,"B"]+
      EG3*pG[,"BC"])*pz1.+7*(1-pz1.)

#Table 3.2
data.frame(X1=ptje_mate,
           WIA=round(B4-B1,3),
           PSA=round(B2-B1,3))

#Table 3.4
data.frame(X1=ptje_mate,
           WIA=round(B4-B1,3),
           PSA=round(B2-B1,3),
           WSA=round(B4-B3, 3))

##Table 3.6
data.frame(X1=ptje_mate,
           WIA=round(B4-B1,3),
           PSA=round(B2-B1,3),
           WSA=round(B4-B3, 3),
           FSA=round(B3-B2,3))


#################################################
#### Rcode for computing results using X1,X2#####
#################################################
## Multinomial Regression
pz1=multinom(as.factor(Z0bs)~matem+LyC,
             data=data.Ready)

### lm by program
biologia=data.Ready %>% filter(nom_carrera=="BIOLOGÕA")
biologia.M=data.Ready %>% filter(nom_carrera=="BIOLOGÕA MARINA")
bioquimica=data.Ready %>% filter(nom_carrera=="BIOQUÕMICA")


lm.biologia.M=lm(GPA~matem+LyC,
                 data=biologia.M) 
lm.biologia=lm(GPA~matem+LyC,
               data=biologia)
lm.bioquimica=lm(GPA~matem+LyC,
                 data=bioquimica) 

matem = seq(min(dataObs$matem), max(dataObs$matem), length.out = grid.lines)
LyC = seq(min(dataObs$LyC), max(dataObs$LyC), length.out = grid.lines)
xy = expand.grid( x = matem, y = LyC)
xy=as.data.frame(xy)
colnames(xy)=c("matem","LyC")

## Predicted GPAs
pred.biologia.M=predict(lm.biologia.M, newdata = xy) 
pred.biologia=predict(lm.biologia, newdata = xy) 
pred.bioquimica=predict(lm.bioquimica, newdata = xy)


fitted.carreras=predict(multinom.carreras, newdata = xy, type="probs")

data.Z0=data.Ready %>% filter(Z0bs==0)

## P(U=u|X1,X2,G=0), where in the database we have that
# Z=1 --> u=(1,0,0)
# Z=2 --> u=(0,1,0)
# Z=3 --> u=(0,0,1)
# Z=4 --> u=(1,1,0)
# Z=5 --> u=(1,0,1)
# Z=6 --> u=(0,1,1)
# Z=7 --> u=(1,1,1)

pS=multinom(as.factor(Z)~matem+LyC,
            data=data.Z0)
pS.pred=predict(pS,newdata=xy,probs,type="probs")



##############################
## Information for Tables 3.3, 3.5, and 3.7
m=c(rep(550,3),rep(650,3),rep(820,3))
l=c(rep(c(520,650,800),3))
data.predict=data.frame(a=m,b=l)
colnames(data.predict)=c("matem","LyC")
pS.pred.1=predict(pS,newdata=data.predict,probs,type="probs")

### Estimated conditional expectations
pred.biologia.M.1=predict(lm.biologia.M, newdata = list(matem=m,LyC=l)) #E(Y|X1,X2,G=1)
pred.biologia.1=predict(lm.biologia, newdata = list(matem=m,LyC=l)) #E(Y|X1,X2,G=2)
pred.bioquimica.1=predict(lm.bioquimica, newdata = list(matem=m,LyC=l)) #E(Y|X1,X2,G=3)
fitted.carreras.1=predict(multinom.carreras, newdata = data.predict, type="probs")
pred.pz1=predict(pz1, newdata = data.predict, type="probs")

bound1=(1-pred.pz1)+
  (pred.biologia.M.1*fitted.carreras.1[,"MB"]+
     pred.biologia.1*fitted.carreras.1[,"B"]+
     pred.bioquimica.1*fitted.carreras.1[,"BC"])*pred.pz1

bound2=(as.numeric(min_max.gpa$min.gpa[2])*(pS.pred.1[1]+pS.pred.1[4]+pS.pred.1[6])+
                  as.numeric(min_max.gpa$min.gpa[1])*(pS.pred.1[2]+pS.pred.1[5])+
                  as.numeric(min_max.gpa$min.gpa[3])*pS.pred.1[3])*(1-pred.pz1)+
  (pred.biologia.M.1*fitted.carreras.1[,"MB"]+
     pred.biologia.1*fitted.carreras.1[,"B"]+
     pred.bioquimica.1*fitted.carreras.1[,"BC"])*pred.pz1

bound3=(pred.biologia.M.1*(fitted.carreras.1[,"MB"]*pred.pz1+
                             (pS.pred.1[1]+pS.pred.1[4]+pS.pred.1[6])*(1-pred.pz1))+
          pred.biologia.1*(fitted.carreras.1[,"B"]*pred.pz1+
                             (pS.pred.1[2]+pS.pred.1[5])*(1-pred.pz1))+
          pred.bioquimica.1*(fitted.carreras.1[,"BC"]*pred.pz1+
                               pS.pred.1[3]*(1-pred.pz1)))

bound4=(pred.biologia.M.1*fitted.carreras.1[,"MB"]+
           pred.biologia.1*fitted.carreras.1[,"B"]+
           pred.bioquimica.1*fitted.carreras.1[,"BC"])*pred.pz1+
  7*(1-pred.pz1)

## Table 3.3
data.frame(X1=m,X2=l,
           WIA=round(bound4-bound1,3),
           PSA=round(bound2-bound1,3))

## Table 3.5
data.frame(X1=m,X2=l,
           WIA=round(bound4-bound1,3),
           PSA=round(bound2-bound1,3),
           WSA=round(bound4-bound3,3))

## Table 3.7
data.frame(X1=m,X2=l,
        WIA=round(bound4-bound1,3),
         PSA=round(bound2-bound1,3),
         WSA=round(bound4-bound3,3),
         FSA=round(bound3-bound2,3))

#############################  
## Organization of the data for for Figures 3.2b, 3.3b, and 3.4b
pred.pz=predict(pz1, newdata = xy, type="probs")  

LBY0=(1-pred.pz)+
  (pred.biologia.M*fitted.carreras[,"MB"]+
     pred.biologia*fitted.carreras[,"B"]+
     pred.bioquimica*fitted.carreras[,"BC"])*pred.pz


UBmonNM=(pred.biologia.M*fitted.carreras[,"MB"]+
           pred.biologia*fitted.carreras[,"B"]+
           pred.bioquimica*fitted.carreras[,"BC"])*pred.pz+
  (pred.biologia.M*(pS.pred[,1]+pS.pred[,4]+pS.pred[,6])+
     pred.biologia*(pS.pred[,2]+pS.pred[,5])+
     pred.bioquimica*pS.pred[,3])*(1-pred.pz)


UBmonNM2=(pred.biologia.M*fitted.carreras[,"MB"]+
            pred.biologia*fitted.carreras[,"B"]+
            pred.bioquimica*fitted.carreras[,"BC"])*pred.pz+
  7*(1-pred.pz)

LBmon=matrix(LBY0, 
             nrow = grid.lines, ncol = grid.lines) 
UBmon=matrix(UBmonNM, 
             nrow = grid.lines, ncol = grid.lines) 

UBmon2=matrix(UBmonNM2, 
              nrow = grid.lines, ncol = grid.lines) 

min_max.gpa=data.biol.mat %>% group_by(nom_carrera) %>% 
  summarise(min.gpa=min(GPA),
            max.gpa=max(GPA))

lm.biologia.M1=lm(GPA~matem+LyC,
                  data=biologia.M) ## #E(Y|X1,X2,G=1)
lm.biologia1=lm(GPA~matem+LyC,
                data=biologia) ## #E(Y|X1,X2,G=2)
lm.bioquimica1=lm(GPA~matem+LyC,
                  data=bioquimica) ## #E(Y|X1,X2,G=3)


pred.biologia.M=predict(lm.biologia.M,newdata =xy) #estimated E(Y|X1,X2,G=1)
pred.biologia=predict(lm.biologia,newdata = xy) #estimated E(Y|X1,X2,G=2)
pred.bioquimica=predict(lm.bioquimica,newdata = xy) #estimated E(Y|X1,X2,G=3)

ignorabilidad=pred.biologia*fitted.carreras[,"B"]+
  pred.biologia.M*fitted.carreras[,"MB"]+
  pred.bioquimica*fitted.carreras[,"BC"]

igno.mat=matrix(ignorabilidad, 
                nrow = grid.lines, ncol = grid.lines)

table(data.Ready$nom_carrera)
LBminNM=(as.numeric(min_max.gpa$min.gpa[2])*(pS.pred[,1]+pS.pred[,4]+pS.pred[,6])+
           as.numeric(min_max.gpa$min.gpa[1])*(pS.pred[,2]+pS.pred[,5])+
           as.numeric(min_max.gpa$min.gpa[3])*pS.pred[,3])*(1-pred.pz)+
  pred.biologia.M*fitted.carreras[,"MB"]*pred.pz+
  pred.biologia*fitted.carreras[,"B"]*pred.pz+
  pred.bioquimica*fitted.carreras[,"BC"]*pred.pz

LBmin=matrix(LBminNM, 
             nrow = grid.lines, ncol = grid.lines)

####### PSA Figure 3.2b
library(rgl)
par(mar = c(5, 4, 4, 2) - 1)
scatter3D(dataObs$matem, dataObs$LyC, dataObs$GPA,
          pch = 19, cex = 0.5,colvar = NULL, col="black",
          theta =20, phi = 20, bty="b", zlim=c(1,7),
          xlab = "Math score", ylab = "Language score", zlab = "GPA",  
          surf = list(x = matem, y = LyC, z = igno.mat,  
                      facets = TRUE,
                      col=ramp.col(col = c("#084594","#084594"), alpha=0.7)), main = "Predicted GPA")

persp3D(x = matem, y = LyC, z = UBmon2, add = TRUE,
        colvar = NULL, alpha = 0.6,col="#2ca25f")

persp3D(x = matem, y = LyC, z = LBmin,  
        add=TRUE,colvar = NULL,
        col="#d95f02", alpha=0.5)

persp3D(x = matem, y = LyC, z = LBmon,add=TRUE,
        colvar = NULL,col="#d95f02",alpha=0.4)

legend("bottomleft", 
       legend=c("Upper bound under WIA",
                "Identification bounds under PSA",
                "Predicted GPAs in selected",
                "applicants"), 
       col=c("#2ca25f","#d95f02","#084594","white"), pch=15,
       cex=0.8,bg = "transparent")

####### WSA Figure 3.3b
par(mar = c(5, 4, 4, 2) - 1)
scatter3D(dataObs$matem, dataObs$LyC, dataObs$GPA,
          pch = 19, cex = 0.5,colvar = NULL, col="black",
          theta = 20, phi = 18, bty="b", zlim=c(1,7),
          xlab = "Math score", ylab = "Language score", zlab = "GPA",  
          surf = list(x = matem, y = LyC, z = igno.mat,  
                      facets = TRUE,
                      col=ramp.col(col = c("#084594","#084594"), alpha=0.7)), main = "Predicted GPA")


persp3D(x = matem, y = LyC, z = UBmon,  
        add=TRUE,colvar = NULL,
        col="#d95f02", alpha=0.5)

persp3D(x = matem, y = LyC, z = UBmon2,add=TRUE,
        colvar = NULL,col="#d95f02",alpha=0.6)

persp3D(x = matem, y = LyC, z = LBmon,add=TRUE,
        colvar = NULL,col="#2ca25f",alpha=0.4)

legend("bottomleft", 
       legend=c("Lower bound under WIA",
                "Identification bounds under WSA",
                "Predicted GPAs in selected",
                "applicants"), 
       col=c("#2ca25f","#d95f02","#084594","white"), pch=15,
       cex=0.8,bg = "transparent")

####### FSA Figures 3.4b
par(mar = c(5, 4, 4, 2) - 1)
scatter3D(dataObs$matem, dataObs$LyC, dataObs$GPA,
          pch = 19, cex = 0.5,colvar = NULL, col="black",
          theta =20, phi = 20, bty="b", zlim=c(1,7),
          xlab = "Math score", ylab = "Language score", zlab = "GPA",  
          surf = list(x = matem, y = LyC, z = igno.mat,  
                      facets = TRUE,
                      col=ramp.col(col = c("#084594","#084594"), alpha=0.7)), main = "Predicted GPA")

persp3D(x = matem, y = LyC, z = LBmin,  
        add=TRUE,colvar = NULL,
        col="#d95f02", alpha=0.5)

persp3D(x = matem, y = LyC, z = UBmon,  
        add=TRUE,colvar = NULL,
        col="#d95f02", alpha=0.5)

persp3D(x = matem, y = LyC, z = LBmon,add=TRUE,
        colvar = NULL,col="#2ca25f",alpha=0.4)

persp3D(x = matem, y = LyC, z = UBmon2,add=TRUE,
        colvar = NULL,col="#2ca25f",alpha=0.6)



legend("bottomleft", 
       legend=c("Identification bounds under WIA",
                "Identification bounds under FSA",
                "Predicted GPAs in selected",
                "applicants"), 
       col=c("#2ca25f","#d95f02","#084594","white"), pch=15,
       cex=0.8,bg = "transparent")




### 3.4 Contrasting the results under PSA, FSA, and WSA with traditional approaches
## Figure 3.5
heckman.model=heckit(Z0bs ~ matem+Ciencias+
                       LyC+NEM+Ranking,
                     GPA~ matem,
                     data=data.Ready)

data.heck=data.Ready %>% select(matem,Z0bs) %>% 
  mutate(h.model=predict(heckman.model,
                         data.Ready)) %>% arrange(matem)

data.heck=data.heck %>% filter(Z0bs==1)
ggplot() +
  geom_point(data = data.biol.mat,
             aes(x = matem, y = GPA, shape = Program),
             show.legend = FALSE,
             col="darkgrey") +
  ylim(1, 7) +
  ylab("GPA") + xlab("Mathematics test score") +
  geom_line(data = data.heck,
            aes(x = matem, y = h.model, color = "Heckman's Solution", 
                linetype = "Heckman's Solution"),
            size = 1.1) +
  geom_line(data = data.full.obs,
            aes(x = pje.mate, y = ltp, color = "Ignorability Solution", 
                linetype = "Ignorability Solution"),
            size = 1.1) +
  geom_ribbon(data = data.bounds,
              aes(x = matem, ymin = B3, ymax = B4, 
                  fill = "Worst Selection Assumption"),
              alpha = 0.2) +
  geom_ribbon(data = data.bounds,
              aes(x = matem, ymin = B2, ymax = B3, fill = 
                    "Fallible Selection Assumption"),
              alpha = 0.2) +
  geom_ribbon(data = data.bounds,
              aes(x = matem, ymin = B1, ymax = B2, 
                  fill = "Perfect Selection Assumption"),
              alpha = 0.2)+
  scale_color_manual(name = 'Current solution', 
                     values = c("Heckman's Solution" = 'black', 
                                "Ignorability Solution" = 'black')) +
  scale_linetype_manual(name = 'Current solution', values = 
                          c("Heckman's Solution" = 'dotted', 
                            "Ignorability Solution" = 'solid')) +
  scale_fill_manual(name = 'Identification regions', 
                    breaks=c("Worst Selection Assumption",
                             "Fallible Selection Assumption",
                             "Perfect Selection Assumption"),
                    values = c("Worst Selection Assumption" = 'red', 
                               "Fallible Selection Assumption" = 'blue',
                               "Perfect Selection Assumption"="green")) +
  theme_bw() +
  theme(
    legend.direction = "vertical",
    legend.position = "right",
    legend.box = "vertical",
    legend.key = element_blank(),
    legend.background = element_rect(fill = "white", colour = "white"))

