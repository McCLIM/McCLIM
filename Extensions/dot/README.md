# DOT Graph Layout

This extension provides new graph types for McCLIM that perform layout by
integrating with other tools that speak
the [DOT language](https://www.graphviz.org/doc/info/lang.html). While the most
well known tools are these from the [Graphviz](https://www.graphviz.org/)
software suite, anything that speaks DOT should be usable.

This extension is MIT licensed.

## How it works

This extension integrates with the [cl-dot](https://github.com/michaelw/cl-dot)
project. When laying out the graph, an instance of `cl-dot:graph` is created
that represents the graph. This graph is then passed to a user overridable
function that must return a `cl-dot:graph` instance, with all the layout
relevant attributes (like `pos`) populated.

The default layout function (`external-graphviz-dot-processor`) calls
Graphviz's `dot` program, prints the graph to its stdin, and reads the layed
out graph from stdout.

For most graphs, using this layout engine is as simple as installing Graphviz
and specifying the `:dot-digraph` graph type. However, there are also some
advanced features that this extension exposes.

## Default layout function keyword arguments

The default layout function (`external-graphviz-dot-processor`) accepts the
following keyword arguments:

- `:program`: the command to execute. Defaults to `dot`.
- `:noop`: a numeric value that is added to the graphviz invocation with the
  `-n` option.
- `:splines`: sets the splines attribute using `-G`. This defaults to `"true"`,
  which is different than Graphviz's default! If you want to switch to
  Graphviz's default, explicitly set it to `nil`. This default is chosen for a
  uniform experience between layout engines, particularly when using consistent
  layouts.
- `:directed`: if non-NIL, the graph is directed. This will be automatically
  set depending on your arguments to `format-graph-from-roots`.

## Other features

### Easy edge labels

It is possible, but not trivial, to put labels on edges with
`format-graph-from-roots` (by specifying a custom `:arc-drawer`). However, it's
cumbersome (IMO) to do so and it's impossilbe to get McCLIM's built-in graph
layout algorithms to take them into account. However, the Dot language
trivially supports this.

This extension provides the class `dot-arc-drawer`. This is a funcallable
class, so instances of it can be passed directly as the `:arc-drawer`
argument. This class creates a protocol for drawing edges and drawing
labels. In the most common case, you need only provide a function to the
`:edge-label-printer` initarg that accepts four required arguments (the
`dot-arc-drawer` instance, a stream, the from object, and the to object) and
any number of keyword arugments (the arguments to `:arc-drawing-options`). This
function should draw the label to the provided stream.

Note that the Dot language only supports specifying text labels. This extension
supports arbitrary output records as labels. To do this, the extension computes
the bounding box of the label record and creates a text that closely resembles
the true size of the record and pastes that string into the Dot specification
of the graph.

### Multiple layout engines

The default layout function can use any installed layout engine. Just pass the
name of the layout engine by specifying `:layout-engine` in the
`:dot-processor-options` argument. If not specified, it defaults to something
that's (hopefully) reasonable.

### Consistent layout

It can be very confusing/frustrating when a tiny change to the graph
drastically changes the layout. You can use the `:layout-override` option to
provide all the node (and optionally edge) positions in the graph.

Currently, the extension still calls the layout function even if the position
of every node and edge is overridden. However, that may change in the future.

If the layout is overridden, the layout function will be called with the
keyword argument `:noop` set to 2. When using the default
`external-graphviz-dot-processor` function, this corresponds to passing the
argument `-n2`. In the Graphviz suite of tools, only the neato layout engine
currently supports this option. Therefore, you should make sure to specify the
`:layout-engine` as neato or specify no engine, in which case neato will be
automatically chosen.

Currently, the only way to obtain an object to pass to `:layout-override` is to
call `make-layout-override` on a graph record returned by this extension.

## Demo

Here is a graph layed out using McCLIM's default layout mechanisms:

![](imgs/sample-1-built-in.png)

Here is the same graph layed out using the `dot` tool from Graphviz:

![](imgs/sample-1-dot.png)

## Known Issues

1. The input DOT graph has only the bounding box of each node. This means there
   may be a gap between arrows and the nodes to which they are connected.
