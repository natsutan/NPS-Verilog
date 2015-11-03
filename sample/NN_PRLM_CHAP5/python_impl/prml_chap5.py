# http://qiita.com/tkazusa/items/b8e4d287f94f8b87ed52

import matplotlib.pyplot as plt
from pylab import *
import numpy as np
import random


def heaviside(x):
    return 0.5 * (np.sign(x) + 1)


def NN(x_train, t, n_imput, n_hidden, n_output, eta, W1, W2, n_loop):
    fp_w1 = open("W1.txt", "w")
    fp_w2 = open("W2.txt", "w")
    fp_diff1 = open("diff1.txt", "w")
    fp_diff2 = open("diff2.txt", "w")


    for n in range(n_loop):
        header = "# loop %d\n" % n
        fp_w1.write(header)
        fp_w2.write(header)
        fp_diff1.write(header)
        fp_diff2.write(header)


        for n in range(len(x_train)):
            x = np.array([x_train[n]])

            #feedforward
            X = np.insert(x, 0, 1) #Insert fixed term

            A = np.dot(W1, X) #(5.62)
            Z = np.tanh(A)  #(5.63)
            Z[0] = 1.0
            Y = np.dot(W2, Z) #(5.64)


            #Backprobagation
            D2 = Y - t[n]#(5.65)
            D1 = (1-Z**2)*W2*D2 #(5.66)

            diff_w1 = W1 - eta*D1.T*X
            diff_w2 = W2 - eta*D2.T*Z

            W1 = diff_w1 #(5.67), (5.43)
            W2 = diff_w2 #(5.67), (5.43)

            str_w1 = "%s\n" % str(W1)
            str_w2 = "%s\n" % str(W2)
            str_diff1 = "%s\n" % str(diff_w1)
            str_diff2 = "%s\n" % str(diff_w2)
            fp_w1.write(str_w1)
            fp_w2.write(str_w2)
            fp_diff1.write(str_diff1)
            fp_diff2.write(str_diff2)

    fp_w1.close()
    fp_w2.close()
    fp_diff1.close()
    fp_diff2.close()

    return  W1, W2

def output(x, W1, W2):
    X = np.insert(x, 0, 1) #Insert fixed term

    A = np.dot(W1, X) #(5.62)
    Z = np.tanh(A)  #(5.63)
    Z[0] = 1.0 #Insert fixed term
    Y = np.dot(W2, Z) #(5.64)
    return Y, Z


def print_weight(w, name):
    fp = open(name+".txt", "w")
    s = str(w)
    fp.write(s)
    fp.close()

if __name__ == "__main__":
#    global fp_w1, fp_w2
    #Set form of nueral network 
    n_imput = 2
    n_hidden = 4
    n_output = 1
    eta = 0.1
    W1 = np.random.random((n_hidden, n_imput))
    W2 = np.random.random((n_output, n_hidden))
    print_weight(W1, "W1_init")
    print_weight(W2, "W2_init")

    n_loop = 1000

    #Set train data
    x_train = np.linspace(-4, 4, 300).reshape(300, 1)
    y_train_1 = x_train * x_train
    y_train_2 = np.sin(x_train)
    y_train_3 = np.abs(x_train)
    y_train_4 = heaviside(x_train)

    fp_xtrain = open("xtrain.dat", "w")
    fp_xtrain_1 = open("xtrain_1.dat", "w")
    for x in x_train:
        fp_xtrain.write("%f\n" % x)
    for x in y_train_1:
        fp_xtrain_1.write("%f\n" % x)
    fp_xtrain.close()
    fp_xtrain_1.close()

    W1_1, W2_1= NN(x_train, y_train_1, n_imput, n_hidden, n_output, eta, W1, W2, n_loop) 
    # W1_2, W2_2= NN(x_train, y_train_2, n_imput, n_hidden, n_output, eta, W1, W2, n_loop)
    # W1_3, W2_3= NN(x_train, y_train_3, n_imput, n_hidden, n_output, eta, W1, W2, n_loop)
    # W1_4, W2_4= NN(x_train, y_train_4, n_imput, n_hidden, n_output, eta, W1, W2, n_loop)
    print_weight(W1_1, "W1_1_last")
    print_weight(W2_1, "W2_2_last")

    Y_1 = np.zeros((len(x_train), n_output))
    Z_1 = np.zeros((len(x_train), n_hidden))

#    Y_2 = np.zeros((len(x_train), n_output))
#    Z_2 = np.zeros((len(x_train), n_hidden))

#    Y_3 = np.zeros((len(x_train), n_output))
#    Z_3 = np.zeros((len(x_train), n_hidden))

#    Y_4 = np.zeros((len(x_train), n_output))
#    Z_4 = np.zeros((len(x_train), n_hidden))

    for n in range(len(x_train)):
        Y_1[n], Z_1[n] =output(x_train[n], W1_1, W2_1)
       # Y_2[n], Z_2[n] =output(x_train[n], W1_2, W2_2)
       # Y_3[n], Z_3[n] =output(x_train[n], W1_3, W2_3)
       # Y_4[n], Z_4[n] =output(x_train[n], W1_4, W2_4)


    plt.plot(x_train, Y_1, "r-")
    plt.plot(x_train, y_train_1, "bo", markersize=3)
    for i in range(n_hidden):
        plt.plot(x_train, Z_1[:,i], 'm--')
    xlim([-1,1])
    ylim([0, 1])
    title("Figure 5.3(a)")
    show()

    # plt.plot(x_train, Y_2, "r-")
    # plt.plot(x_train, y_train_2, "bo", markersize=2)
    # for i in range(n_hidden):
    #     plt.plot(x_train, Z_2[:,i], 'm--')
    # xlim([-3.14,3.14])
    # ylim([-1, 1])
    # title("Figure 5.3(b)")
    # show()
    #
    #
    # plt.plot(x_train, Y_3, "r-")
    # plt.plot(x_train, y_train_3, "bo", markersize=4)
    # for i in range(n_hidden):
    #     plt.plot(x_train, Z_3[:,i], 'm--')
    # xlim([-1,1])
    # ylim([0, 1])
    # title("Figure 5.3(c)")
    # show()
    #
    #
    # plt.plot(x_train, Y_4, "r-")
    # plt.plot(x_train, y_train_4, "bo" ,markersize=2)
    # for i in range(n_hidden):
    #     plt.plot(x_train, Z_4[:,i], 'm--')
    # xlim([-2,2])
    # ylim([-0.05, 1.05])
    # title("Figure 5.3(d)")
    # show()


