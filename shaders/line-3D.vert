#version 150 core

in vec3 pos;
/* in vec3 color; */

uniform mat4 view;
uniform mat4 proj;

/* out vec3 vColor; */

void main()
{
    /* gl_Position = proj * view * vec4(pos, 1.0); */
    gl_Position = vec4(pos, 1.0);
    /* vColor = vec3(0, 0, 0); */
}

