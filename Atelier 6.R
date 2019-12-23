---
title: "Atelier 6 - Travaux Pratique"
author: "SAUGER Jade"
date: "6/12/2019"
output: pdf_document
---
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

## Étude cas témois sur l'infarctus du myocarde


Les données suivantes sont issues d'une enquête cas témoins dont le but  était d'évaluer l'existence d'un risque plus élevé de survenue d'un infarctus du myocarde chez les femmes qui utilisent ou ont utilisé des contraceptifs oraux. L'étude a  été menée auprès de 149 femmes ayant eu un infarctus du myocarde (cas) et 300 femmes n'en n'ayant pas eu (témoins). Le facteur d'exposition principal est la prise de contraceptifs oraux, les autres facteurs recueillis sont : l'âge, le poids, la taille, la consommation de tabac, l'hypertension artérielle, les antécédents familiaux de maladies cardio-vasculaires.


### Variables et codage


\begin{tabular}{|l|p{5cm}|c|}
\hline
Description & Unite ou Codage & Variable \\
\hline
Infarctus du myocarde & 0 = Témoins; 1 = Cas & \texttt{INFARCT} \\
\hline
Prise de contraceptifs oraux & 0 = Jamais; 1 = Oui & \texttt{CO} \\
\hline
Consommation de tabac & 0 = Non;
1 = Fumeuse actuelle; 2 = Ancienne fumeuse & \texttt{TABAC} \\
\hline
Âge & Années & \texttt{AGE} \\
\hline
Poids & kg & \texttt{POIDS} \\
\hline
Taille & cm  & \texttt{TAILLE} \\
\hline 
Antécédents familiaux de maladie cardio-vasculaire & 0 = Non ; 1 = Oui & \texttt{ATCD} \\
\hline
Hypertension artérielle  &  0 = Non ; 1 = Oui & \texttt{HTA} \\
\hline
\end{tabular}


Vous avez de telecharcher le fichier \texttt{Infarct.csv} de le site:
  
  
   \url{https://github.com/afalco/Atelier6/}


avec la fonction \texttt{read.csv2()}, à mon ordinateur:
  
  
  
  ```{r}
Infarct <- read.csv2("C:/Users/Dadoune/Desktop/Infarct.csv")
View(Infarct)
```


et après de répondre a toutes les questions suivantes vous avez de télécharger le fichier \texttt{Atelier6.Rmd} rempli à votre répertoire dans \url{https://github.com/}. \textbf{La data limite pour le rentrer est le lundi 23 décembre 2019}:
  
  
  
   1. Etudier si la variable \texttt{TAILLE} suit une distribution normal


```{r}
summary(Infarct$TAILLE)
z.taille <- (Infarct$TAILLE-mean(Infarct$TAILLE))/sd(Infarct$TAILLE)
set.seed(123)
normal.simulation <- rnorm(length(z.taille))
qqplot(normal.simulation,z.taille,xlim=c(-3,3),ylim=c(-3,3))
par(new=T)
plot(normal.simulation,normal.simulation,type="l",col="red",xlim=c(-3,3),ylim=c(-3,3),xlab = "",ylab = "")
```

Commentaire: Nous pouvons dire que la taille suit une loi normale car la moyen et la mediane sont très proche. 
  
  
  
  2. Etudier si la variable \texttt{POIDS} suit une distribution normal.



```{r}
summary(Infarct$POIDS)
z.poids <- (Infarct$POIDS - mean(Infarct$POIDS))/sd(Infarct$POIDS)
set.seed(123)
normal.simulation <-rnorm(length(z.poids))
qqplot(normal.simulation,z.poids,xlim=c(-3,3),ylim=c(-3,3))
par(new=T)
plot(normal.simulation,normal.simulation,type = "l",col="red",xlim=c(-3,3),ylim=c(-3,3),xlab="",ylab="")
```


Commentaire: La moyenne et la mediane sont quasiment éguale donc le poids suit une loi normale 
  
  
  
  3. Calculer chez les femmes ayant eu un infarctus du myocarde le pourcentage de qui a prise des contraceptifs oraux et aussi chez le femmes n'en n'ayant pas eu un infarctus du myocarde le pourcentage de qui a prise de des contraceptifs oraux. 



```{r}
infarct.CO0 <- Infarct$CO[Infarct$INFARCT == 0]
infarct.CO1 <- Infarct$CO[Infarct$INFARCT == 1]
table(infarct.CO0)
table(infarct.CO1)
prob.infarct.CO0 <- (table(infarct.CO0)/length(infarct.CO0))*100
prob.infarct.CO1 <- (table(infarct.CO1)/length(infarct.CO1))*100
prob.infarct.CO0[2]
prob.infarct.CO1[2]
```


Commentaire: Nous pouvons constater que parmis les femmes ayant eu un infarctus du myocarde un peu plus de 75% on pris des contraceptif oraux et parmis les femmes n'ayant pas eu un infarctus du myocarde le pourcentage qui ont pris des contraceptif oraux est de 29,3%.
  
  
  
  4. Calculer chez le femmes qui a prise des contraceptifs oraux quelle est la probabilité d'avoir un infarctus du myocarde.



```{r}
CO.infarct <- Infarct$INFARCT[Infarct$CO == 1]
table(CO.infarct)
prob.CO.infarct <- table(CO.infarct)/length(CO.infarct)
prob.CO.infarct[2]
```


 Commentaire: Pour une femme ayant pris des contraceptifs oraux la probabilité d'avoir un infractus du myocarde est de 0,56



 5. Représentez la fonction de densité de probabilité de la variable \texttt{AGE} chez l'échantillon. Est-ce qu'il est une variable discrète ou continue?


```{r}
hist(Infarct$AGE,freq=FALSE,col = "red",xlab = "âge",main = "Représentation la fonction de densité de probalité de l'âge chez l'échantillon")
```

 
 Commentaire: L'age est ici une variable quantitative discrète. 





  6. Est-ce qu'il \texttt{IMC} de la base de données \texttt{Infarct} est bien calculé?
  
  
  
  ```{r}
IMC.1 <- Infarct$POIDS/(Infarct$TAILLE/100)^2
x_maille <- seq(from=0,to=0.9,by=0.1)
set.seed(123)
A <- quantile(Infarct$IMC,x_maille)
B <- quantile(IMC.1,x_maille)
erreur <- sum((A-B)^2)/length(x_maille)
erreur
```


Commentaire: Puisque nous pouvons constater que l'erreur est très faible donc nous pouvons en deduir que la base de donner est bien calculer.
  
  
  
  
  
  7. Est-ce que la consommation du tabac est une facteur de risque chez le femmes ayant eu un infarctus du myocarde?
  
  
  
  ```{r}
TABAC.infarct <- Infarct$TABAC[Infarct$INFARCT == 1]
table(TABAC.infarct)
prob.TABAC.infarct <- table(TABAC.infarct)/length(TABAC.infarct)
prob.TABAC.infarct[1]
prob.TABAC.infarct[2]
prob.TABAC.infarct[3]
```

Commentaire: Oui, la consomation de tabac est bien un facteur de risque que se soit une consomation ancienne ou actuel.
  
  
  
  
  
  8. Quelle est la pourcentage chez le femmes avec antécédents familiaux de maladie cardio-vasculaire d'avoir un infarctus du myocarde?

  

```{r}
ATCD.infarct <- Infarct$INFARCT[Infarct$ATCD == 1]
table(ATCD.infarct)
prob.ATCD.infarct <- (table(ATCD.infarct)/length(ATCD.infarct))*100
prob.ATCD.infarct[2]
```
commentaire : le pourcentage de femme ayant eu des antecedant familiaux de maladi cardio vasculaire d'Infarctus est de 43,6%


  9. Est-ce le femmes ayant eu un infarctus du myocarde ont un poids moyenne différent de le femmes n'en n'ayant pas eu un infarctus du myocarde?

  

```{r}
infarct.pinf <- Infarct$POIDS[Infarct$INFARCT == 1]
infarct.p <- Infarct$POIDS[Infarct$INFARCT == 0]
mean(infarct.pinf)
mean(infarct.p)
```

  Commentaire: Oui, le poid moyen est different, puisque nous avons ici une différence de poid moyen importante, d'environ 7,4 kg.  

  

  10. Est-ce le femmes ayant eu un infarctus du myocarde ont une taille moyenne différent de le femmes n'en n'ayant pas eu un infarctus du myocarde?



```{r}
m.tle.inf<-Infarct$TAILLE[(Infarct$INFARCT=="1")]
mean(m.tle.inf)
m.tle.fem<-Infarct$TAILLE[(Infarct$INFARCT=="0")]
mean(m.tle.fem)
```


  Commentaire: Nous pouvons donc constaater que la taille ne joue pas de role pour ce qui d'avoir des infarctus du myocarde.