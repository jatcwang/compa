# Questions:

## Usage/Feature design

### Allow same path 'shape' but different converters (aka falling back)?

/hello/:int
/hello/:string            <- fallback to this one if first one fails?

I think we can make this configurable

### Allow for streaming bodies into the function

### Allow for mounting a common set of paths with a common prefix
e.g.

Given these routes
/foo
/bar

mount them behind /base

/base/foo
/base/bar

## End-User friendliness

### Accumulate errors?

## Documentation and examples

- Dependent parsing between query parameters

# Thinking

Err = 1. not matched 2. bad input 3. bad auth 4. exception

* A prefilter is just Kleisli[EitherT[F, Err, ?], Request[F], Request[F]] AKA do something with the request (can fail with Err type)
