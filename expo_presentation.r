# load agreement statistics functions
source('./agree.coeff2.r')
source('./agree.coeff3.raw.r')
source('./sem_distance.r')

#
# Random data, difference between percentage agreement and agreement measures
#

# generate random data
data <- matrix(sample(1:2, 20000, replace=T), nrow = 2)

# convert to contingency table
cont_table <- table(data[1,], data[2,])

# calculate agreement measures
kappa2.table(cont_table)


# read ASSESS CT data
data_assessment <- read.table('./agree_data_assess_ct.csv', header = TRUE, sep = ',')
# select SNOMED CT coded cases
sct <- matrix(as.vector(data_assessment[,2]), nrow = 101, ncol = 6)

# calculate semantic distance weights based on Lin
weight <- lin_sem_distance(sct)

# calculate Krippendorff's alpha w/o weights
krippen.alpha.raw(sct)

# calculate weighted Krippendorff's alpha
krippen.alpha.raw(sct, weight)

