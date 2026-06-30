# ProbLang

A small probabilistic programming language in Haskell. Models mix random draws and observations, and the runtime runs rejection sampling to return the posterior.

## Running

```
stack run    -- runs the medical diagnosis example
stack test   -- runs the tests
```

## Language

```
x ~ Bernoulli(0.01);       // sample
let tp = 0.95;             // deterministic binding
if x then {
  y ~ Bernoulli(tp);
} else {
  y ~ Bernoulli(0.10);
}
observe y = 1;             // condition on observed value
return [x];
```

Distributions: `Bernoulli(p)`, `Categorical(p1, p2, ...)`, `Binomial(n, p)`, `Poisson(lambda)`, `Geometric(p)`, `NegBinomial(r, p)`.

Operators: `+`, `-`, `*`, `/`, `==`, `<`, `&&`, `||`.

## Example: Medical Diagnosis

```
has_disease ~ Bernoulli(0.01);
let tp = 0.95;
let fp = 0.10;
if has_disease then {
  test ~ Bernoulli(tp);
} else {
  test ~ Bernoulli(fp);
}
observe test = 1;
return [has_disease];
```

Analytic answer: `0.95 * 0.01 / (0.95 * 0.01 + 0.10 * 0.99) = ~8.76%`

The sampler gets close to this with enough accepted traces (~10k).

## Structure

```
src/
  AST.hs           -- data types
  Distributions.hs -- samplers for all 6 distributions
  Parser.hs        -- Megaparsec parser
  Interpreter.hs   -- evaluator + statement executor
  Inference.hs     -- rejection sampling + posterior
  Main.hs          -- medical diagnosis demo

test/
  DistributionsSpec.hs
  ParserSpec.hs
  InterpreterSpec.hs
  InferenceSpec.hs
```

## Tests

75 tests covering distribution convergence (mean + variance), parser correctness, interpreter, and 6 end-to-end models with analytic answers.
