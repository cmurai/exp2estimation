# ----- generate simulation data ----- #

files = list.files('./daw2011', pattern = "*.r", full.names = TRUE)
sapply(files[2:6], source)

library(Rsolnp)

# run demo
results050 = sim_demo(50)
results100 = sim_demo(100)
results150 = sim_demo(150)
results200 = sim_demo(200)
results250 = sim_demo(250)
results300 = sim_demo(300)
results350 = sim_demo(350)

results = sim_demo(201,200)