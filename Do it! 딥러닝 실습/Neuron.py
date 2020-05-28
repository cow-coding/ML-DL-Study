class Neuron:

    def __init__(self):
        self.w = 1.0                                        # Initialized the Weight
        self.b = 1.0                                        # Initialized the Bias

    def forpass(self, x):
        y_hat = x * self.w + self.b                         # Calculate the linear equation
        return y_hat

    def backprop(self, x, err):
        w_grad = x * err                                    # Calculate the Gradient about Wight
        b_grad = 1 * err                                    # Calculate the Gradient about Bias
        return w_grad, b_grad

    def fit(self, x, y, epochs=100):
        for i in range(epochs):                             # Repeat as epochs
            for x_i, y_i in zip(x,y):                       # Repeat as every samples
                y_hat = self.forpass(x_i)                   # Calculate Forward Propagation
                err = -(y_i - y_hat)                        # Calculate Error
                w_grad, b_grad = self.backprop(x_i, err)    # Calculate Back Propagation
                self.w -= w_grad                            # Update the Weight
                self.b -= b_grad                            # Update the Bias
