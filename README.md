## diagrams-gl

A low level OpenGL backend to the diagrams library. **Only works on my
own [`diagrams`](https://github.com/cchalmers/diagrams) fork.**

This backend only handles the OpenGL commands, it cannot produce
anything directly. It either needs another backend (like
[`diagrams-sdl`](https://github.com/cchalmers/diagrams-sdl)) or for you
to handle the OpenGL commands yourself.

### 3D backend

Currently only supports basic shapes with a single light source and
very basic paths.

### 2D backend

There is currently no support for 2D primitives, but there are plans
to do so.

