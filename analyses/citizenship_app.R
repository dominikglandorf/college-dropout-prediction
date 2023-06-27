bg3=bg[bg$cohort>=2011&bg$cohort<=2016,]

plot(x=rank(bg3$mellon_id), y=rank(bg3$student_sid), col=as.numeric(factor(bg3$citizenship_app)), pch="o")
legend("topright", legend = legend_labels, col = 1:4, pch = 19, bty = "n")

plot(x=rank(bg3$mellon_id), y=rank(bg3$student_sid), col=as.numeric(factor(is.na(bg3$graduated_term))), pch="o")

plot(x=rank(bg3$mellon_id), y=rank(bg3$student_sid), col=as.numeric(factor(bg3$cohort)), pch="o")
legend("topright", legend = levels(factor(bg3$cohort)), col = 1:6, pch = 19, bty = "n")


bg3$citizenship_app[bg3$citizenship_app=="US Citizen in Transit"]="US Citizen"
