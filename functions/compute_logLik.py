import tensorflow as tf
from functions.utils import gamma_to_theta

def compute_logLik(model, gamma, beta, eta, y, multilabel, pm = None, im = None):
    if(multilabel):
        gamma = tf.reshape(gamma, shape = (tf.math.multiply(gamma.shape[0], gamma.shape[1]), 1))
        y = tf.reshape(y, shape = (tf.math.multiply(y.shape[0],y.shape[1])))
    theta = gamma_to_theta(gamma)
    n = y.shape[0]
    idx = tf.range(n, dtype = tf.int32) # index to get the theta values
    if(multilabel):
        yu = tf.cast(y + 1, dtype = tf.int32)
    else:
        yu = tf.math.argmax(y, axis = 1, output_type = tf.int32) + 1 # labels of class k
    yl = yu -1 # labels of class k-1
    yu = tf.reshape(yu, shape = (n, 1))
    yl = tf.reshape(yl, shape = (n, 1))
    idx = tf.reshape(idx, shape = (n, 1))
    idx_yu = tf.concat((idx, yu), axis = 1)
    idx_yl = tf.concat((idx, yl), axis = 1)
    thetau = tf.gather_nd(theta, indices = idx_yu)
    thetal = tf.gather_nd(theta, indices = idx_yl)
    if(model.nn_x != None):
        if(isinstance(beta, int)):
            beta = beta
        else:
            beta = tf.reshape(beta, shape = (beta.shape[0],))
    if(model.nn_im != None):
        if(isinstance(eta, int)):
            eta = eta
        else:
            eta = tf.reshape(eta, shape = (eta.shape[0],))
    lli = model.distr.cdf(thetau - beta - eta) - model.distr.cdf(thetal - beta - eta)
    nll = - tf.reduce_mean(tf.math.log(lli + 1e-16)) # epsilon to make sure to get no 0 in the log function
    return nll