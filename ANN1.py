from keras.models import Sequential
from keras.layers import Dense
import numpy
#fix random seed for reproducability
numpy.random.seed(7)

# load the dataset
dataset = numpy.loadtxt("pima-indians-diabetes.csv", delimiter=",")
# split into input and output variables
X = dataset[:,0:8]
Y = dataset[:,8]

print(X.shape)

model = Sequential()
#first layer, input_dim is number of inputs
model.add(Dense(12, input_dim=8, activation='relu'))
#second hidden layer
model.add(Dense(8, activation='relu'))
#output layer
model.add(Dense(1, activation='sigmoid'))

#model summary
model.summary()

#compile the model
model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])

#fit the model
model.fit(X, Y, epochs=150, batch_size=10)

scores = model.evaluate(X, Y)
print("\n%s: %.2f%%" % (model.metrics_names[1], scores[1]*100))
