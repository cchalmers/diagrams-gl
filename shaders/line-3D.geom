#version 330 core

// Quick and dirty straight lines in opengl. The shader recieves the
// start and end point of the line and outputs two triangles with width
// w, extended in the normal direction of the line in camera space. This
// means the line should have the same width from any angle.

layout(lines) in;
layout(triangle_strip, max_vertices = 6) out;

/* in vec3 vColor[]; */
/* out vec3 fColor; */

uniform mat4 view;
uniform mat4 proj;
uniform float width;

// This gives very strange results when the line width is higher than
// the size of the actual line.

void main()
{
  /* fColor = vColor[0]; */

  // line start and end points
  vec4 a = proj * view * gl_in[0].gl_Position;
  vec4 b = proj * view * gl_in[1].gl_Position;

  // we could normalise the points a before stroking to get lines with
  // the same width no matter where we look
  /* a = vec4(a.xyz/abs(a.w), sign(a.w)); */
  /* b = vec4(b.xyz/abs(b.w), sign(b.w)); */

  // the change in the normalized points
  vec3 aN = a.xyz/a.w;
  vec3 bN = b.xyz/b.w;
  vec3 d = bN - aN;
  // the normal to the line
  vec4 normal = width*normalize(vec4(-d.y,d.x,0.0f,0.0f));
  /* vec4 normal = vec4(0.2f, 0.0f, 0.0f ,0.0f); */

  d = width*normalize(d);

  // Lines are extended by the width so two lines placed at an angle
  // don't look so bad (it's still pretty bad)
  /* a = a - d; */
  /* b = b + d; */

  // first triangle
  gl_Position = a - normal;
  EmitVertex();
  gl_Position = a + normal;
  EmitVertex();
  gl_Position = b + normal;
  EmitVertex();

  // second triangle
  gl_Position = a - normal;
  EmitVertex();
  gl_Position = b - normal;
  EmitVertex();
  gl_Position = b + normal;
  EmitVertex();

  EndPrimitive();
}
