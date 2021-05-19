const Matrix = require('./matrix.js');

function sigmoid(x) {
    return 1 / (1 + Math.exp(-x));
}

module.exports = class NeuralNetwork {
    // various activation functions 
    static activations = {
        sigmoid: {
            func: sigmoid,
            dfunc: (x) => sigmoid(x) * (1 - sigmoid(x)),
        },
        tanh: {
            func: Math.tanh,
            dfunc: (x) => 1 - (Math.tanh(x) ** 2),
        },
        identity: {
            func: (x) => x,
            dfunc: (x) => 1,
        },
        relu: {
            func: (x) => x >= 0 ? x : 0,
            dfunc: (x) => x >= 0 ? 1 : 0,
        },
    };

    constructor(layerdims = [], activationName = "relu") {
        // A[nhidden][ninput] * b[ninput] -> c[hnidden]
        // B[noutput][nhidden] * c[nhidden] -> d[noutput]

        this.layers = [];
        for (let i = 1; i < layerdims.length; i++) {
            const layerdim = layerdims[i];
            const prevlayerdim = layerdims[i - 1];
            this.layers.push({
                weights: Matrix.random(layerdim, prevlayerdim),
                bias: Matrix.random(layerdim, 1),
            });
        }
        
        if (activationName in NeuralNetwork.activations)
            this.activationName = activationName;
        else
            this.activationName = "relu";
    }

    get activation() {
        return NeuralNetwork.activations[this.activationName];
    }

    outputHistory(input) {
        input = new Matrix(input);
        const ffhistory = this.feedForwardHistory(input);
        ffhistory.forEach((_, i) => ffhistory[i] = ffhistory[i].nonlinear.mat);
        return ffhistory;
    }

    output(input) {
        return this.outputHistory(input).pop();
    }

    trainInput(example, target, rate = 0.1) {
        example = new Matrix(example);
        target = new Matrix(target);
        this.backpropagate(example, target, rate);
    }

    trainDataset(dataset, rate = 0.1, epochs = 100) {
        while (epochs-- > 0) {
            for (const {input, target} of dataset) {
                this.trainInput(input, target, rate);
            }
        }
    }

    testInput(input, target) {
        input = new Matrix(input);
        target = new Matrix(target);
        const output = this.feedForwardHistory(input).pop().nonlinear;
        return {
            input: input.mat,
            target: target.mat,
            output: output.mat,
            error: target.minus(output).mat,
        };
    }

    testDataset(dataset) {
        return dataset.map(({input, target}) => this.testInput(input, target));
    }


    feedForwardLayer(layer, input) {
        const linear = layer.weights.times(input).plus(layer.bias);
        const nonlinear = linear.map(this.activation.func);
        return {linear, nonlinear};
    }

    feedForwardHistory(input) {
        const output = [{nonlinear: input}];
        for (const layer of this.layers) {
            output.push(this.feedForwardLayer(layer, output[output.length - 1].nonlinear));
        }
        return output;
    }

    feedForward(input) {
        return this.feedForwardHistory(input).pop();
    }

    computeErrorLayer(layer, previousError) {
        return layer.weights.transposed().times(previousError);
    }

    computeErrors(output, target) {
        const errors = [target.minus(output.nonlinear)];
        for (let i = this.layers.length - 1; i > 0; i--) {
            const layer = this.layers[i];
            errors.unshift(this.computeErrorLayer(layer, errors[0]));
        }
        return errors;
    }

    computeDeltaLayer(rate, error, output, input) {
        const activationDerivative = output.linear.map(this.activation.dfunc);
        const biasdelta = error.timesElementWise(activationDerivative).scaledBy(rate);
        const weightsdelta = biasdelta.times(input.nonlinear.transposed());
        return {bias: biasdelta, weights: weightsdelta};
    }

    computeDeltas(rate, errors, ffhistory) {
        const deltas = [];
        for (let i = 0; i < this.layers.length; i++) {
            const input = ffhistory[i];
            const output = ffhistory[i + 1];
            const error = errors[i];
            deltas.push(this.computeDeltaLayer(rate, error, output, input));
        }
        return deltas;
    }

    backpropagate(example, target, rate) {
        const ffhistory = this.feedForwardHistory(example);
        const errors = this.computeErrors(ffhistory[ffhistory.length - 1], target); 
        const deltas = this.computeDeltas(rate, errors, ffhistory);

        for (let i = 0; i < deltas.length; i++) {
            const delta = deltas[i];
            this.layers[i].weights = this.layers[i].weights.plus(delta.weights);
            this.layers[i].bias = this.layers[i].bias.plus(delta.bias);
        }
    }

    serialize() {
        return JSON.stringify(this);
    }

    static deserialize(data) {
        const parsed = typeof data === "string" ? JSON.parse(data) : data;

        const nn = new NeuralNetwork();
        for (const layer of parsed.layers) {
            nn.layers.push({
                weights: Matrix.deserialize(layer.weights),
                bias: Matrix.deserialize(layer.bias),
            });
        }

        nn.activationName = parsed.activationName;

        return nn;
    }
}
