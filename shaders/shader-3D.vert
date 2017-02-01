#version 330 core

// Input vertex data, different for all executions of this shader.
layout(location = 0) in vec3 vertexPosition_modelspace;
layout(location = 1) in vec3 vertexNormal_modelspace;

// Output data ; will be interpolated for each fragment.
out vec3 Position_worldspace;
out vec3 Normal_cameraspace;
out vec3 EyeDirection_cameraspace;
out vec3 LightDirection_cameraspace;

// Values that stay constant for the whole mesh.
uniform mat4 MVP;
uniform mat4 V;
uniform mat4 M;
// uniform mat4 M_inverse_transpose;
uniform vec3 LightPos_w;

void main(){

  // Output position of the vertex, in clip space : MVP * position
  gl_Position =  MVP * vec4(vertexPosition_modelspace,1);

  // Position of the vertex, in worldspace : M * position
  Position_worldspace = (M * vec4(vertexPosition_modelspace,1)).xyz;

  // Vector that goes from the vertex to the camera, in camera space.
  // In camera space, the camera is at the origin (0,0,0).
  vec3 vertexPosition_cameraspace = ( V * vec4(vertexPosition_modelspace,1)).xyz;
  EyeDirection_cameraspace = vec3(0,0,0) - vertexPosition_cameraspace;

  // Vector that goes from the vertex to the light, in camera space. M
  // is ommited because it's identity.
  /* vec3 Light_pos = vec3(40,40,(-5)); */
  // vec3 LightPosition_cameraspace = ( V * vec4(2,2,-2,1)).xyz;
  vec3 far_lightpos = vec3(50,000,200);
  vec3 LightPosition_cameraspace = ( V * vec4(far_lightpos,1)).xyz;
  /* vec3 LightPosition_cameraspace = ( V * vec4(LightPos_w,1)).xyz; */
  LightDirection_cameraspace = LightPosition_cameraspace + EyeDirection_cameraspace;

  // Normal of the the vertex, in camera space
  Normal_cameraspace = ( V * vec4(vertexNormal_modelspace,0)).xyz;
  // Only correct if ModelMatrix does not scale the model ! Use its inverse transpose if not.

}

