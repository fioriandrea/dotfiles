function filledtensor(dims, gen = () => 0) { 
    if (!dims.length)
        return gen();
    return Array.from({length: dims[0]}).map(() => filledtensor(dims.slice(1), gen));
}

function randomtensor(dims) {
    return filledtensor(dims, () => Math.random() * 2 - 1);
}

module.exports = class Matrix {
    constructor(init) {
        if (typeof init === "number")
            init = [[init]];
        else if (typeof init[0] === "number")
            init = init.map(e => [e]);
        else if (init instanceof Matrix)
            init = init.mat;
        this.mat = init;
        this.rows = init.length;
        this.columns = init[0].length;
    }

    map(fn) {
        const init = filledtensor([this.rows, this.columns]);
        let i = 0;
        for (let j = 0; j < this.rows; j++) {
            for (let k = 0; k < this.columns; k++) {
                init[j][k] = fn(this.mat[j][k], i++);
            }
        }
        return new Matrix(init);
    }

    times(other) {
        if (this.columns !== other.rows)
            throw new Error(`unmatched dimensions (${this.rows}, ${this.columns}), (${other.rows}, ${other.columns}) of [${this}] and [${other}]`);

        //Aij * Bjk
        const res = Matrix.filled(this.rows, other.columns);
        for (let i = 0; i < this.rows; i++) {
            for (let k = 0; k < other.columns; k++) {
                for (let j = 0; j < this.columns; j++) {
                    res.mat[i][k] += this.mat[i][j] * other.mat[j][k];
                }
            }
        }
        return res;
    }

    timesElementWise(other) {
        if (this.columns !== other.columns && this.rows !== other.rows)
            throw new Error(`unmatched dimensions (${this.rows}, ${this.columns}), (${other.rows}, ${other.columns})`);
        
        const res = Matrix.filled(this.rows, this.columns);
        for (let i = 0; i < this.rows; i++) {
            for (let j = 0; j < this.columns; j++) {
                res.mat[i][j] = this.mat[i][j] * other.mat[i][j];
            }
        }
        return res;
    }

    scaledBy(scalar) {
        return this.map(e => e * scalar);
    }

    dividedBy(scalar) {
        return this.scaleBy(1 / scalar);
    }

    transposed() {
        const res = Matrix.filled(this.columns, this.rows);
        for (let i = 0; i < this.rows; i++) {
            for (let j = 0; j < this.columns; j++) {
                res.mat[j][i] = this.mat[i][j];
            }
        }
        return res;
    }

    plus(other) {
        if (this.columns !== other.columns && this.rows !== other.rows)
            throw new Error(`unmatched dimensions (${this.rows}, ${this.columns}), (${other.rows}, ${other.columns}) of [${this}] and [${other}]`);
        
        const res = Matrix.filled(this.rows, this.columns);
        for (let i = 0; i < this.rows; i++) {
            for (let j = 0; j < this.columns; j++) {
                res.mat[i][j] = this.mat[i][j] + other.mat[i][j];
            }
        }
        return res;
    }

    minus(other) {
        return this.plus(other.scaledBy(-1));
    }

    get(i, j) {
        return this.mat[i][j];
    }

    toString() {
        return `${this.mat}`;
    }

    serialize() {
        return JSON.stringify(this);
    }

    static deserialize(data) {
        const parsed = typeof data === "string" ? JSON.parse(data) : data;
        const init = parsed.mat;
        return new Matrix(init);
    }

    static filled(rows, columns, gen) {
        return new Matrix(filledtensor([rows, columns], gen));
    }

    static random(rows, columns) {
        return new Matrix(randomtensor([rows, columns]));
    }
}