# P(y=1)

Pone = 1/(1 + exp(-1 * (-1.5 + 3*1 -0.5* 5)))
Pone

# P(y=0)
Pzero = 1 - Pone

# Odds
Odds = Pone/Pzero
Odds
# Logit
log(Odds)


# easier way to compute Logit, it's the linear regression equation
logit = -1.5 + 3*1 -0.5*5   # -1

# Odds = e^(Logit)
exp(logit)

Pone2 = 1/(1 + exp(-logit))
Pone2
