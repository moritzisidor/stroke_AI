# Define the model architecture

from tensorflow.keras import backend as K
from tensorflow.keras import initializers
from tensorflow.keras.layers import Layer, Input, Dense, Dropout, Activation, Flatten, Lambda, Convolution2D, MaxPooling2D, Reshape, BatchNormalization, concatenate
from tensorflow.keras import layers
from tensorflow import keras
from tensorflow.keras.models import Model


# Convolutional block with two convolitional layers
def conv_block2_all_dropout(input_x, size, dropout_level):
    x = Convolution2D(size, (3,3), kernel_initializer=initializers.he_normal(seed=3004), padding='same')(input_x)
    x = BatchNormalization(axis=3)(x)
    x = Activation('relu')(x)
    x = Lambda(lambda x: K.dropout(x, level=dropout_level))(x)
    x = Convolution2D(size, (3,3), kernel_initializer=initializers.he_normal(seed=3004), padding='same')(x)
    x = BatchNormalization(axis=3)(x)
    x = Activation('relu')(x)
    x = Lambda(lambda x: K.dropout(x, level=dropout_level))(x)
    x = MaxPooling2D(pool_size=(2, 2))(x)
    return x

# Convolutional block with three convolitional layers
def conv_block3_all_dropout(input_x, size, dropout_level):
    x = Convolution2D(size, (3,3), kernel_initializer=initializers.he_normal(seed=3004), padding='same')(input_x)
    x = BatchNormalization(axis=3)(x)
    x = Activation('relu')(x)
    x = Lambda(lambda x: K.dropout(x, level=dropout_level))(x)
    x = Convolution2D(size, (3,3), kernel_initializer=initializers.he_normal(seed=3004), padding='same')(x)
    x = BatchNormalization(axis=3)(x)
    x = Activation('relu')(x)
    x = Lambda(lambda x: K.dropout(x, level=dropout_level))(x)
    x = Convolution2D(size, (3,3), kernel_initializer=initializers.he_normal(seed=3004), padding='same')(x)
    x = BatchNormalization(axis=3)(x)
    x = Activation('relu')(x)
    x = Lambda(lambda x: K.dropout(x, level=dropout_level))(x)
    x = MaxPooling2D(pool_size=(2, 2))(x) 
    return x

# Stack the convolutional blocks to an architecture
def cnn_all_dropout(input_shape, drop_level = 0.3):
    
    img_input = Input(shape=input_shape)
    
    # Convolutional part
    x = conv_block2_all_dropout(img_input,32,dropout_level=drop_level)
    x = conv_block2_all_dropout(x,64,dropout_level=drop_level)
    x = conv_block3_all_dropout(x,128,dropout_level=drop_level)
    x = conv_block3_all_dropout(x,256,dropout_level=drop_level)
    x = conv_block3_all_dropout(x,512,dropout_level=drop_level)
    x = conv_block3_all_dropout(x,512,dropout_level=drop_level)
    
    # Dense part
    x = Flatten()(x)
    x = Dense(400, kernel_initializer=initializers.he_normal(seed=3004))(x)
    x = BatchNormalization()(x)
    x = Activation('relu')(x)
    x = Lambda(lambda x: K.dropout(x, level=drop_level))(x)
    x = Dense(100, kernel_initializer=initializers.he_normal(seed=3004))(x)
    x = BatchNormalization()(x)
    x = Activation('relu')(x)
    x = Lambda(lambda x: K.dropout(x, level=drop_level))(x)
    x = Dense(2, kernel_initializer=initializers.he_normal(seed=3004), activation='softmax')(x)
    
    model = Model(img_input, x)
    return model

class Conv2Plus1D(keras.layers.Layer):
  def __init__(self, filters, kernel_size, padding, activation, kernel_initializer):
    """
      A sequence of convolutional layers that first apply the convolution operation over the
      spatial dimensions, and then the temporal dimension. 
    """
    super().__init__()
    self.seq = keras.Sequential([  
        # Spatial decomposition
        layers.Convolution3D(filters=filters,
                      kernel_size=(1, kernel_size[1], kernel_size[2]),
                      padding=padding, activation=activation, kernel_initializer=kernel_initializer),
        # Temporal decomposition
        layers.Convolution3D(filters=filters, 
                      kernel_size=(kernel_size[0], 1, 1),
                      padding=padding, activation=activation, kernel_initializer=kernel_initializer)
        ])

  def call(self, x):
    return self.seq(x)