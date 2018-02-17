# Questions:

## Usage design

### Allow same path 'shape' but different converters (aka falling back)?

/hello/:int
/hello/:string            <- fallback to this one if first one fails?

I think we can make this configurable
