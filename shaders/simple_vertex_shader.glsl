#version 330 core

layout(location = 0) in vec4 vertexPosition_modelspace;

uniform vec3 color;
out vec3 fragmentColor;

void main(){
  fragmentColor = color;
  gl_Position = vertexPosition_modelspace;
}
