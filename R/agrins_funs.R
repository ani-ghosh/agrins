
utility <- function(income, rho, scale=FALSE) {
	if (rho==1) {
		u <- log(income)
	} else {
		u <- (income ^ (1 - rho)) / (1 - rho)
	}
	if (scale) {
		u <- u - min(u)
		u <- u / max(u)
	}
	u
}

ce_utility <- function(utility, rho){
	if (rho == 1) { 
		exp (utility)
	} else {
		((1-rho) * utility) ^ (1/(1-rho))
	}
}


ce_income <- function(income, rho){
    income <- na.omit(pmax(0, income))
    if (rho==1) {
        mean(income)
    } else {
        exputil <- mean((income ^ (1 - rho)) / (1 - rho))
        ((1-rho) * exputil) ^ (1/(1-rho))
    }
}


mqs <- function(ce_insured, ce_not_insured) {
   m = sign(ce_insured - ce_not_insured)
   c("bad", "neutral", "good")[m+2]
}
