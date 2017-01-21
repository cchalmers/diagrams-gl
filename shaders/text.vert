#version 330 core

layout (location = 0) in vec2 Offset;
layout (location = 1) in vec2 TextureCoord;

uniform vec3 Anchor;

out vec2 UV;

void main()
{
  gl_Position = vec4(Anchor,1) + vec4 (Offset,0,0);

  UV = TextureCoord;
}

