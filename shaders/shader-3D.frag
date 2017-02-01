#version 330 core

// Interpolated values from the vertex shaders
// in vec2 UV;
in vec3 Position_worldspace;
in vec3 Normal_cameraspace;
in vec3 EyeDirection_cameraspace;
in vec3 LightDirection_cameraspace;

// Ouput data
out vec4 color;

// Values that stay constant for the whole mesh.
// uniform sampler2D myTextureSampler;
uniform vec3 MaterialDiffuseColor;
// uniform mat4 MV;
uniform vec3 LightPos_w;

void main(){

  // Light emission properties
  // You probably want to put them as uniforms
  vec3 LightColor = vec3(1,1,1);
  float LightPower = 120.0f;

  vec3 material_colour;

  if (gl_FrontFacing) {
    material_colour = MaterialDiffuseColor;
  } else {
    material_colour = vec3(0.3, 0.3, 0.3);
  }

  // Material properties
  // vec3 MaterialDiffuseColor = texture( myTextureSampler, UV ).rgb;

  vec3 MaterialAmbientColor  = vec3(0.5,0.5,0.5) * material_colour;
  vec3 MaterialSpecularColor = vec3(0.3,0.3,0.3);

  // Distance to the light
  /* float distance = length( LightPos_w - Position_worldspace ); */
  float distance = 10.0;
  // float distance = length( vec3(5.0,5.0,5.0) - Position_worldspace );

  // Normal of the computed fragment, in camera space
  vec3 n = normalize( Normal_cameraspace );
  // Direction of the light (from the fragment to the light)
  vec3 l = normalize( LightDirection_cameraspace );
  // Cosine of the angle between the normal and the light direction,
  // clamped above 0
  //  - light is at the vertical of the triangle -> 1
  //  - light is perpendicular to the triangle -> 0
  //  - light is behind the triangle -> 0
  float cosTheta = max(dot(n,l), 0);

  // Eye vector (towards the camera)
  vec3 E = normalize(EyeDirection_cameraspace);
  // Direction in which the triangle reflects the light
  vec3 R = reflect(-l,n);
  // Cosine of the angle between the Eye vector and the Reflect vector,
  // clamped to 0
  //  - Looking into the reflection -> 1
  //  - Looking elsewhere -> < 1
  float cosAlpha = max(0, dot(E,R));

  vec3 c =
    // Ambient : simulates indirect lighting
    MaterialAmbientColor +
    // Diffuse : "color" of the object
    material_colour * LightColor * LightPower * cosTheta / (distance*distance) +
    // Specular : reflective highlight, like a mirror
    MaterialSpecularColor * LightColor * LightPower * pow(cosAlpha,5) / (distance*distance);

  color = vec4(c, 1);

}
