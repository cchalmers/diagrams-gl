#version 150 core

layout(points) in;
layout(triangle_strip, max_vertices = 16) out;

in vec3 vColor[];
out vec3 fColor;

// uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

void main()
{
    fColor = vColor[0];

    /* gl_in[0].gl_Position = proj * view * gl_in[0].gl_Position; */

    // +X direction is "North", -X direction is "South"
    // +Y direction is "Up",    -Y direction is "Down"
    // +Z direction is "East",  -Z direction is "West"
    //                                     N/S   U/D   E/W
    vec4 NEU = proj * view * vec4( 0.1,  0.1,  0.1, 0.0);
    vec4 NED = proj * view * vec4( 0.1, -0.1,  0.1, 0.0);
    vec4 NWU = proj * view * vec4( 0.1,  0.1, -0.1, 0.0);
    vec4 NWD = proj * view * vec4( 0.1, -0.1, -0.1, 0.0);
    vec4 SEU = proj * view * vec4(-0.1,  0.1,  0.1, 0.0);
    vec4 SED = proj * view * vec4(-0.1, -0.1,  0.1, 0.0);
    vec4 SWU = proj * view * vec4(-0.1,  0.1, -0.1, 0.0);
    vec4 SWD = proj * view * vec4(-0.1, -0.1, -0.1, 0.0);

    // Create a cube centered on the given point.
    gl_Position = gl_in[0].gl_Position + NED;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + NWD;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + SWD;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + SED;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + SEU;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + SWU;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + NWU;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + NEU;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + NED;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + SED;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + SEU;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + NEU;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + NWU;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + NWD;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + SWD;
    EmitVertex();

    gl_Position = gl_in[0].gl_Position + SWU;
    EmitVertex();

    EndPrimitive();
}
