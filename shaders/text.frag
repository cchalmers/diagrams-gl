#version 330 core

in vec2 UV;

uniform sampler2D glyph;
uniform vec4 textColour;

out vec4 color;

void main()
{
  float alpha = texture(glyph, UV).r;
  color = vec4(textColour.rgb, textColour.a * alpha);
}

