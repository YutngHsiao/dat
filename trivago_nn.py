import tensorflow as tf
import matplotlib.pyplot as plt
import numpy as np
from numpy import genfromtxt
import pandas as pd
import math

train = genfromtxt("/content/dat/dat2.csv", delimiter=',')
train = np.delete(train, 0, 0)
train = np.delete(train, 0, 1)

x = train[:,0:6];x

y = train[:,6][:, np.newaxis];y
LR = 0.02
batchz = 1000
epoch = 100
datapoints = len(y)
steps = 10000

tf_x = tf.placeholder(tf.float32, x.shape)     # input x holder
tf_y = tf.placeholder(tf.float32, y.shape)

# Network layers
L = tf.layers.dense(tf_x, 6, tf.nn.relu)
L = tf.layers.dense(tf_x, 4, tf.nn.relu)
L = tf.layers.dense(L, 2, tf.nn.relu)
output = tf.layers.dense(L, 1)                     # output layer

loss = tf.losses.mean_squared_error(tf_y, output)   # compute cost
optimizer = tf.train.GradientDescentOptimizer(learning_rate=LR)
train_op = optimizer.minimize(loss)

saver = tf.train.Saver()

with tf.Session() as sess:
    sess.run(tf.global_variables_initializer())
        for step in range(steps):
        # train and net output
        _, loss_, pred = sess.run([train_op, loss, output], {tf_x: x, tf_y: y})
        if step % batchz == 0:
            print("step: {}, RMSE: {}".format(step, math.sqrt(loss_)))

    test = sess.run(output, {tf_x: x})
    save_model = saver.save(sess, "/content/dat/model.ckpt")
    test_hits = pd.DataFrame(test)
    print("Summary of predicted hits")
    print(test_hits.describe())
