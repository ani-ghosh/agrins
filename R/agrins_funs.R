
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

cert_equiv <- function(expected_utility, rho){
	if (rho == 1) { 
		exp (expected_utility)
	} else {
		((1-rho) * expected_utility) ^ (1/(1-rho))
	}
}


cert_equiv_income <- function(income, rho){
    income <- na.omit(pmax(0, income))
    if (rho==1) {
        mean(income)
    } else {
        exputil <- mean((income ^ (1 - rho)) / (1 - rho))
        ((1-rho) * exputil) ^ (1/(1-rho))
    }
}
