getwd()
odddata = read.csv("ankit.csv", header = TRUE, sep=",")
print(nrow(odddata))

shuffleodd = odddata[sample(nrow(odddata)),]
head(shuffleodd)

tlen = 1:round(0.70*nrow(odddata))
telen =(round(0.70*nrow(odddata))+1):nrow(odddata)
trdata = shuffleodd[tlen,]
tstdata = shuffleodd[telen,]

glm = glm(quality~., data = trdata, family = binomial(link = "logit"))
