# DOT Graph Layout

This extension provides new graph types for McCLIM that perform layout by
integrating with other tools that speak
the [DOT language](https://www.graphviz.org/doc/info/lang.html). While the most
well known tools are these from the [Graphviz](https://www.graphviz.org/)
software suite, anything that speaks DOT should be usable.

This extension is MIT licensed.

## How it works

This extension integrates with the [cl-dot](https://github.com/michaelw/cl-dot)
project. When laying out the graph, an instance of `cl-dot::graph` is created
that represents the graph. This graph is then passed to a user overridable
function that must return a `cl-dot::graph` instance, with all the layout
relevant attributes (like `pos`) populated.

To use it, simply have Graphviz installed and specify the `:dot-digraph` graph
type.

## Demo

Here is a graph layed out using McCLIM's default layout mechanisms:

![](imgs/sample-1-built-in.png)

Here is the same graph layed out using the `dot` tool from Graphviz:

![](imgs/sample-1-dot.png)

## Known Issues

1. The input DOT graph has only the bounding box of each node. This means there
   may be a gap between arrows and the nodes to which they are connected.
