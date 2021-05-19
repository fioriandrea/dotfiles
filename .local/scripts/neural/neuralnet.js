const Matrix = require('./matrix.js');

module.exports = class NeuralNetwork {
    // various activation functions (must be recursive derivatives)
    static activations = {
        sigmoid: {
            func: (x) => 1 / (1 + Math.exp(-x)), 
            dfunc: (y) => y * (1 - y),
        },
        tanh: {
            func: (x) => Math.tanh(x),
            dfunc: (y) => 1 - (y * y),
        },
        identity: {
            func: (x) => x,
            dfunc: (y) => 1,
        },
    };

    constructor(layerdims = [], activationName = "sigmoid") {
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
            this.activationName = "sigmoid";
    }

    get activation() {
        return NeuralNetwork.activations[this.activationName];
    }

    outputHistoryArray(input) {
        input = new Matrix(input);
        const ffhistory = this.feedForwardHistory(input);
        ffhistory.forEach((_, i) => ffhistory[i] = ffhistory[i].mat);
        return ffhistory;
    }

    outputArray(input) {
        return this.outputHistoryArray(input).pop();
    }

    feedForwardLayer(layer, input) {
        return layer.weights.times(input).plus(layer.bias).map(this.activation.func);
    }

    feedForwardHistory(input) {
        const output = [input];
        for (const layer of this.layers) {
            output.push(this.feedForwardLayer(layer, output[output.length - 1]))
        }
        return output;
    }

    testInput(input, target) {
        input = new Matrix(input);
        target = new Matrix(target);
        const output = this.feedForwardHistory(input).pop();
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

    feedForward(input) {
        return this.feedForwardHistory(input).pop();
    }

    computeErrorLayer(layer, previousError) {
        return layer.weights.transposed().times(previousError);
    }

    computeErrors(output, target) {
        const errors = [target.minus(output)];
        for (let i = this.layers.length - 1; i > 0; i--) {
            const layer = this.layers[i];
            errors.unshift(this.computeErrorLayer(layer, errors[0]));
        }
        return errors;
    }

    computeDeltaLayer(rate, error, output, input) {
        const activationDerivative = output.map(this.activation.dfunc);
        const biasdelta = error.timesElementWise(activationDerivative).scaledBy(rate);
        const weightsdelta = biasdelta.times(input.transposed());
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

    backpropagate(example, target, rate = 0.1) {
        example = new Matrix(example);
        target = new Matrix(target);

        const ffhistory = this.feedForwardHistory(example);
        const errors = this.computeErrors(ffhistory[ffhistory.length - 1], target); 
        const deltas = this.computeDeltas(rate, errors, ffhistory);

        //console.log(JSON.stringify({errors, deltas, ffhistory}, null, 4))

        for (let i = 0; i < deltas.length; i++) {
            const delta = deltas[i];
            this.layers[i].weights = this.layers[i].weights.plus(delta.weights);
            this.layers[i].bias = this.layers[i].bias.plus(delta.bias);
        }
    }

    backpropagateDataset(dataset, rate = 0.1, epochs = 100) {
        while (epochs-- > 0) {
            for (const {input, target} of dataset) {
                this.backpropagate(input, target, rate);
            }
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
