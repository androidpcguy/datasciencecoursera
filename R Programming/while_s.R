z <- 5
coin <- 1
while(z>= 3 && z <= 10) {
	print(z)
	coin <- rbinom(1,1,0.5)

	z <- if(coin == 1) {
		z + 1 
		} else {
		z-1 }
}

