#version 330 core

uniform vec3 colour;

out vec4 outColor;

void main()
{
    outColor = vec4(colour, 1.0);
}
