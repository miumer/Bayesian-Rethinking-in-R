#' 6 vaatlust, 9. katses, kus ?he vaatluse t?en?osus 50%
#' j?rgneval funktsioonil dbinom() on alati ka r algusega vaste
#' ja p algusega vaste: vastavalt random samples ja cumulative probabilities
dbinom(6, size=9, prob=0.5) 
p_grid <- seq(from=0, to=1, length.out=20)
p_grid
help(seq)
#GRID APPROXIMATION N?IDE

#'Grid approximation v?imalus Bayes inferenciks
#'defineerime gridi. Osime vee proportsioonide
#'t?en?osuste jaotust antud 
#'andmete p?hjal 6 (YESi) 9st pmtslt. 
p_grid <- seq(from=0 , to=1 , length.out=20)

# defineerimine priori. 
prior <- ifelse(p_grid < 0.5, 0, 1)
#'arvutame likelihoodi iga gridi v??rtuse juures
#'iga v??rtuse t?en?osus ongi tema enda v??rtus ja likelihood
#' likelihood on selle proportsiooni juures 6(YES) 9st saamise plausibility
#' ehk [9st6|proportion]
likelihood <- dbinom(2, size=5, prob=p_grid)

plot(p_grid, likelihood)

#'arvutame posteriori likelihoodi ja priori abil
#'Pr(proportion|9st6) = Pr(9st6|proportsioon)*Pr(proportsioon)
unstd.posterior <- likelihood * prior


posterior <- unstd.posterior / sum (unstd.posterior)

#J?rgnevalt plotime posteriori iga v??rtuse kohta.
plot(p_grid, posterior, type="b" ,
     xlab="probability of water" , ylab = "posterior probability")
     mtext("20 points")
     
#installin STANi ja rethinking paketi
install.packages("rethinking")
library(rethinking)
install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
pkgbuild::has_build_tools(debug = TRUE)
install.packages(c('devtools','coda','mvtnorm')) 
library(devtools) 
install_github("rmcelreath/rethinking")

#'QUADRATIC APPROXIMATION 
#'(suurendame kogu n?ites sampli suurus, 
#'proportsion j??b samaks, mis raamatus)
globe.qa <- map(
  alist(
    w ~ dbinom(36,p), #binomial likelihood
    p ~ dunif(0,1)  # uniform prior
  ),
  data=list(w=24))

#'display summary of quadratic approcimation
#'saame sellise vaste: Mean StdDev 5.5% 94.5%
#'                   p 0.67 0.16   0.42  0.92
#'Selles vastes t?hendab "Mean" kvadraadi tippu,
#'StdDev n?itab kvadraadi kurvatuuri
#'Tipp t?hendab ilmselt punkti, mis on k?ige t?en?olisem 
#'otsitav v??rtus andmeid vaadates ja priorit ning likelihoodi teades
precis(globe.qa)

#'Vaatame, kas meie kvadraat aproksimatsioon on
#'?htne ?ige vastusega (teame seda, sest teame posteriori)
#'
#'anal??tiline kalkulatsioon. Mis on beta jaotus?
w <- 24
n <- 36
curve(dbeta(x, w+1, n-w+1), from=0, to=1)

#'kvadraat aproksimatsioon. Siia sisestame precis()
#'funktsioonist saadavad meani  ja Std. 
curve(dnorm(x, 0.67, 0.08), lty=2, add=TRUE)

#'ÜLESANDED lk 61

#'2H1/H2
twins1 <- (0.1*0.5)+(0.2*0.5)
prob_tw_A <- 0.1
prob_tw_B <- 0.2
prob_A_tw <- (0.1*0.5)/twins1
prob_B_tw <- (0.2*0.5)/twins1
twins2 <- (prob_A_tw*0.1) + (prob_B_tw*0.2)
twins2

#teine (õigem) viis:
#'kaksikute tõenäosus valem: p(twins) = p(species=A)*p(twins|A)+p(species=B)*p(twins|B)
#'Alguses on liikide tõenäosus sama, niiet prior on uniform. 
#'Esimese sigimise posterior liikide tõenäosused saavad teise prioriks.
#'p(tweens_2|tweens_1) = p(species=A|tweens_1)*p(twins|A)+p(species=B|tweens_1)*p(twins|B)
#'Kaksikute saamise tõenäosus jääb saamaks, aga selle tõenäosus, mis liik on, muutub.
#'Sellepärast muutub ka teiste kaksikute saamine. Update'ime.
p_twins_A <- 0.1
p_twins_B <- 0.2
likelihood <- c(p_twins_A, p_twins_B)
prior <- c(1, 1) #liikide esinemise tõenäosus on võrdne. (ükskõik, mis number, sest nagunii standardiseerime kaks rida all pool)
unst_posterior <- prior * likelihood # A given twins ja B given twins ehk liikide esinemise tõenõosuste posteriorid
std_posterior <- unst_posterior/sum(unst_posterior) # standardiseeritud posterior (Vt Bayes valemit) [sum(posterior) on k?ik viisid, kuidas saada twinse]

sum(posterior*likelihood) #kaksikute t?en?osus valemi kasutamine

#'2H3
#'Siin Update'ime natukene teistmoodi ja saame uue liigi esinemise
#'tõenäosuse. 
p_twins_A <- 0.1
p_twins_B <- 0.2
p_ones_A <- 0.9
p_ones_B <- 0.8
likelihood <- c(p_twins_A, p_twins_B)
likelihood2 <- c(p_ones_A, p_ones_B)
prior <- c(1, 1)
posterior <- prior * likelihood
posterior <- posterior/sum(posterior)
posterior <- posterior*likelihood2
posterior <- posterior/sum(posterior)
posterior[1]

#2H4
p_testA_A <- 0.8 #positive A given A
p_testA_B <- 1-0.65#positive A given B
likelihood <- c(p_testA_A, p_testA_B)
p_twins_A <- 0.1#twins given A
p_twins_B <- 0.2#twins given B
likelihood_twins <- c(p_twins_A, p_twins_B)
p_ones_A <- 0.9#ones given A
p_ones_B <- 0.8#ones given B
likelihood_ones <- c(p_ones_A, p_ones_B)
prior <- c(1, 1)
posterior <- prior * likelihood # A given test pos. A ja B given test pos. A
posterior <- posterior/sum(posterior) # standardiseeritud
posterior #B on madalam, sest test n?itab A-d B korral v?hem

first_round <- posterior*likelihood_twins # A given twins given test pos A, B given twins given pos A
first_round <- first_round/sum(first_round) #standardized
first_round # B on t?usnud, sest kaksikud t?stavad B t?en?osust

#Liikide t?en?osus kui teises raundis on ?ks laps
posterior1_species <- first_round*likelihood_ones #A given one child, given twins, given test pos A
posterior1_species <- posterior_species/sum(posterior_species)
posterior1_species
#liikide t?en?osused kui teises raundis on kaks last
posterior2_species <- first_round*likelihood_twins
posterior2_species <- posterior2_species/sum(posterior2_species)
posterior2_species


