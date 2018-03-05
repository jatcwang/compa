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

val urlMatcher = root / "path" / intVar ?: Q.int("my_int") & Q.str("my_str")

val checkRequest = ??? // something that just checks the request
Req => Task[Either[Err, A]]

type MM[F[_], In, +Err, Out] = Kl[F, In, Either[Err, Out]]
// Auth type here is the authenticated context e.g. user ID
def auth[Auth, B](mm: Kl[Task, Req, B]): Kl[Task, Req, (Auth, B)]

Err = 1. not matched 2. bad input 3. bad auth 4. exception

handleError( auth(urlMatcher ::: checkRequest) )

handleError:: MM[F[_], In, Err, Out] -> (Err -> Out) -> Kl[F[_], In, Out]

* A prefilter is just Kleisli[EitherT[F, Err, ?], Request[F], Request[F]] AKA do something with the request (can fail with Err type)
