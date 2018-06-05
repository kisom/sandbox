class EMatrix:

    def __init__(self, mat, sol):
        self.mat = []
        l = -1
        for i in range(len(mat)):
            tmp = []
            if l == -1:
                l = len(mat[i])
            else:
                assert(len(mat[i]) == l)
            for j in range(len(mat[i])):
                tmp.append(mat[i][j])
            self.mat.append(tmp)

        assert(len(sol) == l)
        self.sol = []
        for i in range(len(sol)):
            self.sol.append(sol[i])

    def copy(self):
        mat = []
        sol = []
        for i in range(len(self.mat)):
            tmp = []
            for j in range(len(self.mat[i])):
                tmp.append(self.mat[i][j])
            mat.append(tmp)

        for i in range(len(self.sol)):
            sol.append(self.sol[i])

        return EMatrix(mat, sol)

    def add(self, k, r1, r2):
        row1 = [v for v in self.mat[r1]]
        row2 = self.mat[r2]

        for i in range(len(row1)):
            row1[i] = k * row1[i]
            row2[i] += row1[i]

        sol = self.sol[r1] * k

        self.mat[r2] = row2
        self.sol[r2] += sol

    def sub(self, k, r1, r2):
        row1 = [v for v in self.mat[r1]]
        row2 = self.mat[r2]

        for i in range(len(row1)):
            row1[i] = k * row1[i]
            row2[i] -= row1[i]

        sol = self.sol[r1] * k

        self.mat[r2] = row2
        self.sol[r2] -= sol

    def scale(self, k, r):
        for i in range(len(self.mat[r])):
            self.mat[r][i] *= k

        self.sol[r] *= k

    def __repr__(self):
        return "{}x{} EMatrix".format(len(self.sol), len(self.mat))

    def __str__(self):
        m = 0
        s = ''
        for i in range(len(self.mat)):
            for j in range(len(self.mat[i])):
                m = max(m, len('{:g}'.format(self.mat[i][j])))

        for i in range(len(self.sol)):
            m = max(m, len('{:g}'.format(self.sol[i])))
        
        fmt = '{' + ':>{}g'.format(m) + '}'

        for i in range(len(self.mat)):
            for j in range(len(self.mat[i])):
                s += fmt.format(self.mat[i][j])
                s += ' '
            
            s += ' -> ' + fmt.format(self.sol[i]) + '\n'
        
        return s
