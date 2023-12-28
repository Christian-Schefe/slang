let non_primes = [];
let primes = [];
for i in range(2, 100) {
    for j in range(1, 100) {
        if i * j > 100 {
            break;
        };
        non_primes = non_primes.concat([i * j]);
    }
};
print(non_primes);